#' Download Sound Recordings from iNaturalist
#'
#' Queries the iNaturalist API to find and optionally download sound recordings
#' for a specified taxon. Can filter by geographic location and observation quality.
#'
#' @param taxon_name Character. Scientific or common name of the taxon to search for.
#' @param place_name Character. Name of the geographic place to filter by. Required
#'   if \code{use_place_filter = TRUE}. Default is "Australia".
#' @param target_n Numeric. Target number of sound files to download. Default is 300.
#' @param download Logical. If TRUE, downloads audio files. If FALSE, only returns
#'   the count of available recordings. Default is TRUE
#' @param out_dir Character. Output directory for downloaded files. Will create
#'   subdirectories "audio" and "metadata.csv". Default is "sounds".
#' @param allowed_licenses Character vector. Lowercase license codes to accept.
#'   Default includes Creative Commons licenses: cc0, cc-by, cc-by-sa, cc-by-nc,
#'   cc-by-nc-sa. Empty license codes are also accepted.
#' @param use_place_filter Logical. If TRUE, filters observations by place. If FALSE,
#'   searches globally, UNLESS \code{place_name} is explicitly provided. Default is FALSE.
#' @param quality Character. Quality grade filter: "research" for research-grade
#'   observations only, or "all" for all quality grades. Default is "research".
#' @param include_taxon_name Logical. If TRUE, prepends the taxon name to the filename
#'   (e.g., "Genus_species_ObsID_SoundID.ext"). Default is TRUE.
#'
#' @return Integer. Total number of matching records found in iNaturalist.
#'
#' @details
#' When \code{download = TRUE}, the function:
#' \itemize{
#'   \item Creates \code{out_dir/audio/} directory for audio files
#'   \item Creates \code{out_dir/metadata.csv} with observation metadata
#'   \item Downloads files named as \code{<observation_id>_<sound_id>.<ext>}
#'   \item Skips files that already exist in the output directory
#'   \item Respects API rate limits with 1.1 second delays between requests
#' }
#'
#' @examples
#' \dontrun{
#' # Check how many recordings are available
#' n <- get_inat_sounds(
#'   "Turnix maculosus",
#'   place_name = "Australia",
#'   download = FALSE
#' )
#'
#' # Download up to 300 research-grade recordings
#' get_inat_sounds(
#'   "Turnix maculosus",
#'   place_name = "Australia",
#'   target_n = 300,
#'   download = TRUE,
#'   quality = "research"
#' )
#'
#' # Global search (no place filter)
#' get_inat_sounds(
#'   "Gryllus bimaculatus",
#'   use_place_filter = FALSE,
#'   target_n = 100,
#'   download = TRUE
#' )
#' }
#'
#' @export
#' @importFrom httr GET user_agent timeout stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.csv write.table
get_inat_sounds <- function(
  taxon_name,
  place_name = "Australia",
  target_n = 300,
  download = TRUE,
  out_dir = "sounds",
  allowed_licenses = tolower(c("cc0", "cc-by", "cc-by-sa", "cc-by-nc", "cc-by-nc-sa")),
  use_place_filter = FALSE, # set FALSE for global queries
  quality = c("research", "all"), # NEW: "research" or "all"
  include_taxon_name = TRUE # Include species name in filename
) {
  quality <- match.arg(quality)

  # Auto-detect if place filter should be used
  if (!missing(place_name)) {
    use_place_filter <- TRUE
  }

  # Check API availability
  if (!is_api_reachable("inat")) {
    message("iNaturalist API is unreachable. Returning 0.")
    return(0)
  }

  ua <- user_agent("inat-audio-downloader/1.0 (your_email@example.com)")
  base <- "https://api.inaturalist.org/v1/observations"

  ## --- Helper: resolve taxon name to ID ---
  tax_resp <- GET(
    "https://api.inaturalist.org/v1/taxa",
    query = list(q = taxon_name, per_page = 1),
    ua,
    timeout(30)
  )
  stop_for_status(tax_resp)
  tax_json <- content(tax_resp, as = "parsed", type = "application/json")
  if (length(tax_json$results) == 0) {
    stop("No taxon found for name: ", taxon_name)
  }
  taxon_id <- tax_json$results[[1]]$id

  ## --- Helper: resolve place name to ID (only if filtering by place) ---
  place_id <- NULL
  if (use_place_filter) {
    if (is.null(place_name)) {
      stop("place_name must be provided when use_place_filter = TRUE")
    }

    place_resp <- GET(
      "https://api.inaturalist.org/v1/places/autocomplete",
      query = list(q = place_name, per_page = 1),
      ua,
      timeout(30)
    )
    stop_for_status(place_resp)
    place_json <- content(place_resp, as = "parsed", type = "application/json")
    if (length(place_json$results) == 0) {
      stop("No place found for name: ", place_name)
    }
    place_id <- place_json$results[[1]]$id
  }

  ## --- First query: get total number of matching records ---
  count_params <- list(
    taxon_id = taxon_id,
    sounds   = "true",
    per_page = 1,
    page     = 1
  )
  if (quality == "research") {
    count_params$quality_grade <- "research"
  }
  if (use_place_filter && !is.null(place_id)) {
    count_params$place_id <- place_id
  }

  count_resp <- GET(base, query = count_params, ua, timeout(60))
  stop_for_status(count_resp)
  count_json <- content(count_resp, as = "parsed", type = "application/json")
  total_records <- count_json$total_results

  loc_msg <- if (use_place_filter) {
    paste0("in place '", place_name, "'")
  } else {
    "globally"
  }
  qual_msg <- if (quality == "research") "research grade" else "all quality grades"

  message(
    "Found ", total_records,
    " observations with sounds for taxon '", taxon_name,
    "' ", loc_msg, " (", qual_msg, ")."
  )

  ## If we're not downloading, just return the count
  if (!download) {
    return(total_records)
  }

  ## --- Download section ---
  dir.create(file.path(out_dir, "audio"), recursive = TRUE, showWarnings = FALSE)

  csv_path <- file.path(out_dir, "metadata.csv")
  if (!file.exists(csv_path)) {
    write.csv(
      data.frame(
        observation_id = integer(),
        sound_id = integer(),
        taxon_name = character(), # Added taxon_name to metadata
        file_url = character(),
        file_path = character(),
        license_code = character(),
        stringsAsFactors = FALSE
      ),
      csv_path,
      row.names = FALSE
    )
  }

  audio_dir <- file.path(out_dir, "audio")
  if (!dir.exists(audio_dir)) dir.create(audio_dir, recursive = TRUE, showWarnings = FALSE)

  seen <- list.files(audio_dir)
  already_had <- length(seen)
  downloaded <- 0
  page <- 1

  repeat {
    if (downloaded >= target_n) break

    params <- list(
      taxon_id  = taxon_id,
      sounds    = "true",
      per_page  = 200,
      page      = page,
      order     = "desc",
      order_by  = "created_at"
    )
    if (quality == "research") {
      params$quality_grade <- "research"
    }
    if (use_place_filter && !is.null(place_id)) {
      params$place_id <- place_id
    }

    resp <- GET(base, query = params, ua, timeout(60))
    stop_for_status(resp)
    j <- content(resp, as = "parsed", type = "application/json")

    res <- j$results
    if (length(res) == 0) {
      message("No more results from API.")
      break
    }

    for (obs in res) {
      if (downloaded >= target_n) break
      if (is.null(obs$sounds) || length(obs$sounds) == 0) next

      # Prepare taxon string for filename if requested
      tax_str <- ""
      if (include_taxon_name && !is.null(obs$taxon$name)) {
        # Sanitize taxon name: replace spaces with underscores, remove non-alphanumeric
        cleaned_name <- gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", obs$taxon$name))
        tax_str <- paste0(cleaned_name, "_")
      }

      for (s in obs$sounds) {
        if (downloaded >= target_n) break
        url <- s$file_url
        if (is.null(url) || !nzchar(url)) next

        lic <- tolower(ifelse(is.null(s$license_code), "", s$license_code))
        if (nzchar(lic) && !(lic %in% allowed_licenses)) next

        ext <- tolower(sub(".*(\\.mp3|\\.wav|\\.m4a|\\.ogg).*", "\\1", url))
        if (!grepl("^\\.", ext)) ext <- ".mp3"

        # New filename format: [Taxon_Name_]ObsID_SoundID.ext
        fname <- sprintf("%s%s_%s%s", tax_str, obs$id, s$id, ext)

        if (fname %in% seen) next

        dest <- file.path(audio_dir, fname)
        ok <- tryCatch(
          {
            bin <- content(GET(url, ua, timeout(120)), "raw")
            writeBin(bin, dest)
            TRUE
          },
          error = function(e) FALSE
        )

        if (ok) {
          downloaded <- downloaded + 1
          seen <- c(seen, fname)
          cat(sprintf("[%d/%d] %s\n", downloaded, target_n, fname))
          write.table(
            data.frame(
              observation_id = obs$id,
              sound_id       = s$id,
              taxon_name     = ifelse(is.null(obs$taxon$name), NA, obs$taxon$name),
              file_url       = url,
              file_path      = dest,
              license_code   = lic
            ),
            csv_path,
            sep = ",",
            row.names = FALSE,
            col.names = FALSE,
            append = TRUE
          )
        }
      }
    }

    page <- page + 1
    Sys.sleep(1.1)
  }

  cat(sprintf(
    "Done. Already had %d files, downloaded %d new files into %s\n",
    already_had, downloaded, audio_dir
  ))

  # still return the total number of matching records
  return(total_records)
}
