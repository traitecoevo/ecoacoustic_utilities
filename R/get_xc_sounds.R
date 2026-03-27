#' Download Sound Recordings from Xeno-Canto
#'
#' Queries the Xeno-Canto API (v3) to find and optionally download sound
#' recordings for a specified taxon. Can filter by country and quality grade.
#'
#' @param taxon_name Character. Scientific name of the taxon to search for.
#' @param country Character. Country name to filter by (e.g. "Australia").
#'   If provided, restricts results to that country. Default is NULL (global).
#' @param api_key Character. Your Xeno-Canto API key. Retrieve one from
#'   \url{https://xeno-canto.org/account}. Defaults to the environment variable
#'   \code{XC_API_KEY} if set.
#' @param target_n Numeric. Maximum number of recordings to download. Default is 300.
#' @param download Logical. If TRUE, downloads audio files. If FALSE, only returns
#'   the count of available recordings. Default is TRUE.
#' @param out_dir Character. Output directory for downloaded files. Will create
#'   subdirectories "audio" and "metadata.csv". Default is "sounds".
#' @param allowed_licenses Character vector. Lowercase CC license codes to accept.
#'   Default includes: cc0, cc-by, cc-by-sa, cc-by-nc, cc-by-nc-sa.
#' @param quality Character vector. Xeno-Canto quality grades to include. Grades
#'   run from "A" (best) to "E" (worst). Default is c("A", "B").
#' @param sound_type Character or NULL. Filter by sound type, e.g. "song", "call",
#'   "alarm call". NULL means no filter. Default is NULL.
#' @param include_taxon_name Logical. If TRUE, prepends the taxon name to the
#'   filename (e.g. "Genus_species_XC123456.mp3"). Default is TRUE.
#' @param as_wav Logical. If TRUE, converts downloaded audio files to WAV format.
#'   Default is FALSE.
#'
#' @return Integer. Total number of matching recordings found on Xeno-Canto.
#'
#' @details
#' When \code{download = TRUE}, the function:
#' \itemize{
#'   \item Creates \code{out_dir/audio/} directory for audio files
#'   \item Creates \code{out_dir/metadata.csv} with recording metadata
#'   \item Downloads files named as \code{[Taxon_Name_]XC{id}.mp3}
#'   \item Skips files that already exist in the output directory
#'   \item Respects API rate limits with 1.1 second delays between page requests
#'   \item Automatically converts recordings to WAV if \code{as_wav = TRUE}
#' }
#'
#' @examples
#' \dontrun{
#' # Check how many recordings are available globally
#' n <- get_xc_sounds("Turnix maculosus", download = FALSE)
#'
#' # Download up to 200 A/B-quality recordings from Australia
#' get_xc_sounds(
#'   "Turnix maculosus",
#'   country = "Australia",
#'   target_n = 200,
#'   quality = c("A", "B")
#' )
#'
#' # Download calls only, convert to WAV
#' get_xc_sounds(
#'   "Gryllus bimaculatus",
#'   sound_type = "call",
#'   target_n = 100,
#'   as_wav = TRUE
#' )
#' }
#'
#' @export
#' @importFrom httr GET user_agent timeout stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.csv write.table read.csv
get_xc_sounds <- function(
  taxon_name,
  country       = NULL,
  api_key       = Sys.getenv("XC_API_KEY"),
  target_n      = 300,
  download      = TRUE,
  out_dir       = "sounds",
  allowed_licenses = tolower(c("cc0", "cc-by", "cc-by-sa", "cc-by-nc", "cc-by-nc-sa")),
  quality       = c("A", "B"),
  sound_type    = NULL,
  include_taxon_name = TRUE,
  as_wav        = FALSE
) {
  if (!nzchar(api_key)) {
    stop(
      "A Xeno-Canto API key is required.\n",
      "Get yours at https://xeno-canto.org/account then pass it via:\n",
      "  get_xc_sounds(..., api_key = 'YOUR_KEY')  or\n",
      "  Sys.setenv(XC_API_KEY = 'YOUR_KEY')"
    )
  }

  # Check API availability
  if (!is_api_reachable("xc")) {
    message("Xeno-Canto API is unreachable. Returning 0.")
    return(0)
  }

  ua   <- user_agent("xc-audio-downloader/1.0 (your_email@example.com)")
  base <- "https://xeno-canto.org/api/3/recordings"

  # Build XC v3 query string using required search tags.
  # sp: tag is mandatory in v3; cnt: and type: are optional field tags.
  xc_query <- paste0('sp:"', taxon_name, '"')
  if (!is.null(country) && nzchar(country)) {
    xc_query <- paste0(xc_query, ' cnt:"', country, '"')
  }
  if (!is.null(sound_type) && nzchar(sound_type)) {
    xc_query <- paste0(xc_query, ' type:"', sound_type, '"')
  }

  # Map quality grades to an XC quality filter if a subset was requested.
  # XC supports q_gt (greater-than) but not multi-value; we filter client-side.
  quality <- toupper(quality)

  ## --- First query: get total count ---
  count_resp <- GET(
    base, query = list(query = xc_query, page = 1, key = api_key), ua, timeout(60)
  )
  stop_for_status(count_resp)
  count_json <- content(count_resp, as = "parsed", type = "application/json")

  total_records <- as.integer(count_json$numRecordings)
  total_pages   <- as.integer(count_json$numPages)

  loc_msg  <- if (!is.null(country)) paste0("in '", country, "'") else "globally"
  qual_msg <- paste(quality, collapse = "/")

  message(
    "Found ", total_records,
    " recordings for '", taxon_name,
    "' ", loc_msg, " (quality: ", qual_msg, ")."
  )

  if (!download) {
    return(total_records)
  }

  ## --- Download section ---
  dir.create(file.path(out_dir, "audio"), recursive = TRUE, showWarnings = FALSE)

  csv_path <- file.path(out_dir, "metadata.csv")
  if (!file.exists(csv_path)) {
    write.csv(
      data.frame(
        xc_id        = character(),
        taxon_name   = character(),
        recordist    = character(),
        country      = character(),
        locality     = character(),
        latitude     = numeric(),
        longitude    = numeric(),
        date         = character(),
        sound_type   = character(),
        quality      = character(),
        length       = character(),
        file_url     = character(),
        file_path    = character(),
        license_code = character(),
        stringsAsFactors = FALSE
      ),
      csv_path,
      row.names = FALSE
    )
  }

  audio_dir <- file.path(out_dir, "audio")

  # Resolve taxon string once for filenames
  tax_str <- ""
  if (include_taxon_name && nzchar(taxon_name)) {
    tax_str <- paste0(gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", taxon_name)), "_")
  }

  seen       <- list.files(audio_dir)
  downloaded <- 0
  skipped    <- 0
  failed     <- 0
  total_checked <- 0
  page       <- 1

  if (!as_wav) {
    message("Note: Xeno-Canto recordings are typically MP3 files.")
    message("      If your downstream software requires WAV, use as_wav = TRUE.")
  }
  message(paste("Checking up to", target_n, "records for download..."))

  # Process the first page (already fetched) then subsequent pages
  current_json <- count_json

  repeat {
    recordings <- current_json$recordings
    if (is.null(recordings) || length(recordings) == 0) break

    for (rec in recordings) {
      if (total_checked >= target_n) break
      total_checked <- total_checked + 1

      # Quality filter
      rec_quality <- toupper(ifelse(is.null(rec$q) || !nzchar(rec$q), "E", rec$q))
      if (!rec_quality %in% quality) {
        skipped <- skipped + 1
        next
      }

      # License filter — XC license field is a URL like "//creativecommons.org/..."
      lic_url  <- tolower(ifelse(is.null(rec$lic), "", rec$lic))
      lic_code <- .xc_license_url_to_code(lic_url)
      if (nzchar(lic_code) && !(lic_code %in% allowed_licenses)) {
        skipped <- skipped + 1
        next
      }

      # Build download URL — XC file field is protocol-relative ("//xeno-canto.org/...")
      file_url_raw <- ifelse(is.null(rec$file), "", rec$file)
      if (!nzchar(file_url_raw)) {
        skipped <- skipped + 1
        next
      }
      file_url <- if (startsWith(file_url_raw, "//")) {
        paste0("https:", file_url_raw)
      } else {
        file_url_raw
      }

      xc_id <- as.character(rec$id)
      fname <- paste0(tax_str, "XC", xc_id, ".mp3")

      if (fname %in% seen) {
        skipped <- skipped + 1
        next
      }

      dest <- file.path(audio_dir, fname)
      ok <- tryCatch(
        {
          bin <- content(GET(file_url, ua, timeout(120)), "raw")
          writeBin(bin, dest)
          TRUE
        },
        error = function(e) {
          message(paste("Failed to download:", file_url, "-", e$message))
          if (file.exists(dest)) unlink(dest)
          FALSE
        }
      )

      if (ok) {
        downloaded <- downloaded + 1
        seen <- c(seen, fname)
        cat(sprintf("[%d/%d] %s\n", total_checked, target_n, fname))

        lat_val <- suppressWarnings(as.numeric(ifelse(is.null(rec$lat), NA, rec$lat)))
        lng_val <- suppressWarnings(as.numeric(ifelse(is.null(rec$lon), NA, rec$lon)))

        write.table(
          data.frame(
            xc_id        = xc_id,
            taxon_name   = taxon_name,
            recordist    = ifelse(is.null(rec$rec),  NA_character_, rec$rec),
            country      = ifelse(is.null(rec$cnt),  NA_character_, rec$cnt),
            locality     = ifelse(is.null(rec$loc),  NA_character_, rec$loc),
            latitude     = lat_val,
            longitude    = lng_val,
            date         = ifelse(is.null(rec$date), NA_character_, rec$date),
            sound_type   = ifelse(is.null(rec$type), NA_character_, rec$type),
            quality      = rec_quality,
            length       = ifelse(is.null(rec$length), NA_character_, rec$length),
            file_url     = file_url,
            file_path    = dest,
            license_code = lic_code
          ),
          csv_path,
          sep       = ",",
          row.names = FALSE,
          col.names = FALSE,
          append    = TRUE
        )
      } else {
        failed <- failed + 1
      }
    }

    if (total_checked >= target_n) break
    if (page >= total_pages) break

    page <- page + 1
    Sys.sleep(1.1)

    resp <- GET(
      base, query = list(query = xc_query, page = page, key = api_key), ua, timeout(60)
    )
    stop_for_status(resp)
    current_json <- content(resp, as = "parsed", type = "application/json")
  }

  message(sprintf(
    "Done. %d records checked: %d downloaded, %d skipped, %d failed.",
    total_checked, downloaded, skipped, failed
  ))

  # Post-download WAV conversion
  if (as_wav && downloaded > 0) {
    message("Converting downloaded files to WAV format...")
    converted <- convert_to_wav(audio_dir, delete_original = TRUE)

    if (length(converted) > 0) {
      message(sprintf("Successfully converted %d files to WAV.", length(converted)))

      meta <- read.csv(csv_path)
      conv_basenames_no_ext <- tools::file_path_sans_ext(basename(converted))

      updated <- FALSE
      for (i in seq_len(nrow(meta))) {
        meta_basename_no_ext <- tools::file_path_sans_ext(basename(meta$file_path[i]))
        if (meta_basename_no_ext %in% conv_basenames_no_ext) {
          old_path <- meta$file_path[i]
          meta$file_path[i] <- paste0(tools::file_path_sans_ext(old_path), ".wav")
          updated <- TRUE
        }
      }
      if (updated) write.csv(meta, csv_path, row.names = FALSE)
    }
  }

  return(total_records)
}

# Internal helper: map a XC license URL to a short CC code.
# XC license field looks like "//creativecommons.org/licenses/by-nc-sa/4.0/"
.xc_license_url_to_code <- function(lic_url) {
  if (!nzchar(lic_url)) return("")
  if (grepl("publicdomain/zero", lic_url))      return("cc0")
  if (grepl("/by-nc-nd", lic_url))              return("cc-by-nc-nd")
  if (grepl("/by-nc-sa", lic_url))              return("cc-by-nc-sa")
  if (grepl("/by-nc",    lic_url))              return("cc-by-nc")
  if (grepl("/by-nd",    lic_url))              return("cc-by-nd")
  if (grepl("/by-sa",    lic_url))              return("cc-by-sa")
  if (grepl("/by/",      lic_url))              return("cc-by")
  return("")  # Unknown — treat as unlicensed/permissive
}
