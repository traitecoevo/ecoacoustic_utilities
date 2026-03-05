#' Download Images from iNaturalist
#'
#' Queries the iNaturalist API to find and optionally download photos
#' for a specified taxon. Can filter by geographic location and observation quality.
#'
#' @param taxon_name Character. Scientific or common name of the taxon to search for.
#' @param place_name Character. Name of the geographic place to filter by. Required
#'   if \code{use_place_filter = TRUE}. Default is "Australia".
#' @param target_n Numeric. Target number of image files to download. Default is 300.
#' @param download Logical. If TRUE, downloads image files. If FALSE, only returns
#'   the count of available photos. Default is TRUE.
#' @param out_dir Character. Output directory for downloaded files. Will create
#'   subdirectories "images" and "metadata.csv". Default is "images".
#' @param allowed_licenses Character vector. Lowercase license codes to accept.
#'   Default includes Creative Commons licenses: cc0, cc-by, cc-by-sa, cc-by-nc,
#'   cc-by-nc-sa. Empty license codes are also accepted.
#' @param use_place_filter Logical. If TRUE, filters observations by place. If FALSE,
#'   searches globally, UNLESS \code{place_name} is explicitly provided. Default is FALSE.
#' @param quality Character. Quality grade filter: "research" for research-grade
#'   observations only, or "all" for all quality grades. Default is "research".
#' @param include_taxon_name Logical. If TRUE, prepends the taxon name to the filename
#'   (e.g., "Genus_species_ObsID_PhotoID.ext"). Default is TRUE.
#' @param image_size Character. Size of the image to download. One of "original"
#'   (up to 2048px), "large" (up to 1024px), "medium" (up to 500px), or "small"
#'   (up to 240px). Default is "original".
#'
#' @return Integer. Total number of matching records found in iNaturalist.
#'
#' @details
#' When \code{download = TRUE}, the function:
#' \itemize{
#'   \item Creates \code{out_dir/images/} directory for image files
#'   \item Creates \code{out_dir/metadata.csv} with observation metadata
#'   \item Downloads files named as \code{[Taxon_Name_]ObsID_PhotoID.ext}
#'   \item Skips files that already exist in the output directory
#'   \item Respects API rate limits with 1.1 second delays between requests
#' }
#'
#' iNaturalist stores photos in several predefined sizes. The default URL
#' returned by the API uses the "square" (75px) size. This function replaces
#' the size component in the URL with the requested \code{image_size}.
#'
#' @examples
#' \dontrun{
#' # Check how many photos are available
#' n <- get_inat_images(
#'     "Cacatua galerita",
#'     place_name = "Australia",
#'     download = FALSE
#' )
#'
#' # Download up to 300 research-grade photos
#' get_inat_images(
#'     "Cacatua galerita",
#'     place_name = "Australia",
#'     target_n = 300,
#'     download = TRUE,
#'     quality = "research"
#' )
#'
#' # Global search with medium-sized images
#' get_inat_images(
#'     "Danaus plexippus",
#'     use_place_filter = FALSE,
#'     target_n = 100,
#'     download = TRUE,
#'     image_size = "medium"
#' )
#' }
#'
#' @export
#' @importFrom httr GET user_agent timeout stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom utils write.csv write.table read.csv
get_inat_images <- function(
  taxon_name,
  place_name = "Australia",
  target_n = 300,
  download = TRUE,
  out_dir = "images",
  allowed_licenses = tolower(c("cc0", "cc-by", "cc-by-sa", "cc-by-nc", "cc-by-nc-sa")),
  use_place_filter = FALSE, # set FALSE for global queries
  quality = c("research", "all"),
  include_taxon_name = TRUE,
  image_size = c("original", "large", "medium", "small")
) {
    quality <- match.arg(quality)
    image_size <- match.arg(image_size)

    # Auto-detect if place filter should be used
    if (!missing(place_name)) {
        use_place_filter <- TRUE
    }

    # Check API availability
    if (!is_api_reachable("inat")) {
        message("iNaturalist API is unreachable. Returning 0.")
        return(0)
    }

    ua <- user_agent("inat-image-downloader/1.0 (your_email@example.com)")
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
    resolved_taxon_name <- if (!is.null(tax_json$results[[1]]$name)) {
        tax_json$results[[1]]$name
    } else {
        taxon_name
    }

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
        photos   = "true",
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
        " observations with photos for taxon '", taxon_name,
        "' ", loc_msg, " (", qual_msg, ")."
    )

    ## If we're not downloading, just return the count
    if (!download) {
        return(total_records)
    }

    ## --- Download section ---
    dir.create(file.path(out_dir, "images"), recursive = TRUE, showWarnings = FALSE)

    csv_path <- file.path(out_dir, "metadata.csv")
    if (!file.exists(csv_path)) {
        write.csv(
            data.frame(
                observation_id = integer(),
                photo_id = integer(),
                taxon_name = character(),
                observed_taxon_name = character(),
                observed_on = character(),
                latitude = numeric(),
                longitude = numeric(),
                file_url = character(),
                file_path = character(),
                license_code = character(),
                image_size = character(),
                stringsAsFactors = FALSE
            ),
            csv_path,
            row.names = FALSE
        )
    }

    images_dir <- file.path(out_dir, "images")
    if (!dir.exists(images_dir)) dir.create(images_dir, recursive = TRUE, showWarnings = FALSE)

    seen <- list.files(images_dir)
    downloaded <- 0
    skipped <- 0
    failed <- 0
    total_checked <- 0
    page <- 1

    message(paste("Downloading", image_size, "size images."))
    message(paste("Checking up to", target_n, "records for download..."))

    repeat {
        if (total_checked >= target_n) break

        params <- list(
            taxon_id  = taxon_id,
            photos    = "true",
            per_page  = min(200, target_n - total_checked),
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
            if (page == 1) message("No results from API.")
            break
        }

        for (obs in res) {
            if (total_checked >= target_n) break
            if (is.null(obs$photos) || length(obs$photos) == 0) {
                total_checked <- total_checked + 1
                next
            }

            # Extract observation-level metadata
            obs_taxon_name <- tryCatch(
                if (!is.null(obs$taxon$name)) obs$taxon$name else NA_character_,
                error = function(e) NA_character_
            )
            obs_date <- if (!is.null(obs$observed_on)) obs$observed_on else NA_character_
            obs_lat <- NA_real_
            obs_lng <- NA_real_
            if (!is.null(obs$location) && nzchar(obs$location)) {
                loc_parts <- strsplit(obs$location, ",")[[1]]
                if (length(loc_parts) == 2) {
                    obs_lat <- as.numeric(loc_parts[1])
                    obs_lng <- as.numeric(loc_parts[2])
                }
            }

            # Prepare taxon string for filename if requested
            tax_str <- ""
            if (include_taxon_name && nzchar(resolved_taxon_name)) {
                cleaned_name <- gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", resolved_taxon_name))
                tax_str <- paste0(cleaned_name, "_")
            }

            for (p in obs$photos) {
                if (total_checked >= target_n) break
                total_checked <- total_checked + 1
                url <- p$url
                if (is.null(url) || !nzchar(url)) {
                    skipped <- skipped + 1
                    next
                }

                # Replace the default "square" size in the URL with the requested size
                url <- gsub("square", image_size, url, fixed = TRUE)

                lic <- tolower(ifelse(is.null(p$license_code), "", p$license_code))
                if (nzchar(lic) && !(lic %in% allowed_licenses)) {
                    skipped <- skipped + 1
                    next
                }

                ext <- tolower(sub(".*(\\.jpe?g|\\.png|\\.gif).*", "\\1", url))
                if (!grepl("^\\.", ext)) ext <- ".jpg"

                # Filename format: [Taxon_Name_]ObsID_PhotoID.ext
                fname <- sprintf("%s%s_%s%s", tax_str, obs$id, p$id, ext)

                if (fname %in% seen) {
                    skipped <- skipped + 1
                    next
                }

                dest <- file.path(images_dir, fname)
                ok <- tryCatch(
                    {
                        bin <- content(GET(url, ua, timeout(120)), "raw")
                        writeBin(bin, dest)
                        TRUE
                    },
                    error = function(e) {
                        message(paste("Failed to download:", url, "-", e$message))
                        if (file.exists(dest)) unlink(dest)
                        FALSE
                    }
                )

                if (ok) {
                    downloaded <- downloaded + 1
                    seen <- c(seen, fname)
                    cat(sprintf("[%d/%d] %s\n", total_checked, target_n, fname))
                    write.table(
                        data.frame(
                            observation_id      = obs$id,
                            photo_id            = p$id,
                            taxon_name          = resolved_taxon_name,
                            observed_taxon_name = obs_taxon_name,
                            observed_on         = obs_date,
                            latitude            = obs_lat,
                            longitude           = obs_lng,
                            file_url            = url,
                            file_path           = dest,
                            license_code        = lic,
                            image_size          = image_size
                        ),
                        csv_path,
                        sep = ",",
                        row.names = FALSE,
                        col.names = FALSE,
                        append = TRUE
                    )
                } else {
                    failed <- failed + 1
                }
            }
        }

        page <- page + 1
        Sys.sleep(1.1)
    }

    message(sprintf(
        "Done. %d records checked: %d downloaded, %d skipped (including duplicates), %d failed.",
        total_checked, downloaded, skipped, failed
    ))

    # Return the total number of matching records
    return(total_records)
}
