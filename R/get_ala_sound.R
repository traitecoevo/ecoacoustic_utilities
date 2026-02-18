#' Download Sound Recordings from Atlas of Living Australia (ALA)
#'
#' Queries the ALA for sound recordings of a specified taxon and downloads them.
#' This function improves upon basic media downloads by handling metadata
#' and limiting download counts.
#'
#' @param taxon_name Character. Scientific or common name of the taxon to search for.
#' @param target_n Numeric. Target number of sound files to download. Default is 50.
#' @param download Logical. If TRUE, downloads audio files. If FALSE, only returns
#'   the count of available recordings. Default is TRUE.
#' @param out_dir Character. Output directory for downloaded files. Will create
#'   subdirectories "audio" and "metadata.csv". Default is "sounds".
#' @param include_taxon_name Logical. If TRUE, prepends the taxon name to the filename.
#'   Default is TRUE.
#' @param supplier Character. Data supplier to filter for. Options are "all" or "CSIRO".
#'   If "CSIRO", specifically filters for records with institutionCode "ANWC" and
#'   collectionCode "Sounds". Default is "all".
#' @param as_wav Logical. If TRUE, converts downloaded audio files to WAV format.
#'   Default is FALSE.
#'
#' @return Character. The absolute path to the metadata CSV file created/updated.
#'
#' @details
#' When \code{download = TRUE}, the function:
#' \itemize{
#'   \item Creates \code{out_dir/audio/} directory for audio files
#'   \item Creates \code{out_dir/metadata_ala.csv} with record metadata
#'   \item Downloads files named as \code{[Taxon_Name_]<record_id>.<ext>}
#'   \item Skips files that already exist in the output directory
#'   \item Automatically converts recordings to WAV if \code{as_wav = TRUE}
#' }
#'
#' @examples
#' \dontrun{
#' galah_config(email = "your-email@example.com")
#' # Search and download up to 10 sounds as WAV
#' get_ala_sounds("Teleogryllus commodus", target_n = 10, as_wav = TRUE)
#' }
#'
#' @export
#' @importFrom galah galah_call galah_identify galah_filter galah_select atlas_media atlas_counts
#' @importFrom httr GET write_disk stop_for_status user_agent timeout
#' @importFrom dplyr select slice_head
#' @importFrom utils head write.csv write.table read.csv
get_ala_sounds <- function(taxon_name,
                           target_n = 50,
                           download = TRUE,
                           out_dir = "sounds",
                           include_taxon_name = TRUE,
                           supplier = c("all", "CSIRO"),
                           as_wav = FALSE) {
  supplier <- match.arg(supplier)
  # Check API availability
  if (!is_api_reachable("ala")) {
    message("ALA API is unreachable. Returning 0.")
    return(0)
  }

  message(paste0("Searching for sounds for: ", taxon_name, "..."))

  # 1. Build the query
  # We use single-pass pipelines to avoid internal galah_filter evaluation errors.
  # We over-sample slightly to account for R-side MIME-type verification.

  if (supplier == "CSIRO") {
    # Specifically target CSIRO/ANWC Sounds collection on the server
    query <- galah_call() |>
      galah_identify(taxon_name) |>
      galah_filter(institutionCode == "ANWC", collectionCode == "Sounds") |>
      galah_select(
        recordID, scientificName,
        decimalLatitude, decimalLongitude, eventDate,
        institutionCode, collectionCode,
        group = "media"
      ) |>
      dplyr::slice_head(n = target_n * 10) # Over-sample for R-side MIME check
  } else {
    # Broad search for any sound or image (to catch misindexed sounds)
    query <- galah_call() |>
      galah_identify(taxon_name) |>
      galah_filter(multimedia == "Sound" | multimedia == "Image") |>
      galah_select(
        recordID, scientificName,
        decimalLatitude, decimalLongitude, eventDate,
        institutionCode, collectionCode,
        group = "media"
      ) |>
      dplyr::slice_head(n = target_n * 10) # Over-sample for R-side MIME check
  }

  # 2. Early exit check: counts
  # We check occurrences first. If 0, we avoid the heavier media query.
  # This prevents some HTTP 400 errors and saves significant time.
  occ_count <- tryCatch(
    {
      as.numeric(atlas_counts(query)$count)
    },
    error = function(e) {
      # If count fails, we proceed to try atlas_media anyway
      NA_real_
    }
  )

  if (!is.na(occ_count) && occ_count == 0) {
    message("No occurrences found for this query in ALA.")
    return(0)
  }

  # 3. Fetch media metadata
  media_data <- tryCatch(
    {
      atlas_media(query)
    },
    error = function(e) {
      if (grepl("media fields", e$message, ignore.case = TRUE)) {
        message("No media found (ALA returns records but no images/sounds).")
      } else if (grepl("400", e$message)) {
        message("ALA API returned a 400 error. This often happens for complex media queries with 0 results.")
      } else {
        message(paste("Error fetching media from ALA:", e$message))
      }
      return(data.frame())
    }
  )

  # --- Column Mapping for Metadata ---
  cols <- names(media_data)
  # Basic columns
  id_col <- intersect(c("recordID", "occurrenceID", "id"), cols)[1]
  url_col <- intersect(c("media_url", "image_url", "full_res"), cols)[1]
  name_col <- intersect(c("scientificName", "species", "name"), cols)[1]
  lic_col <- intersect(c("license", "dcterms:license", "multimediaLicence"), cols)[1]
  fmt_col <- intersect(c("format", "mimetype", "multimedia_format"), cols)[1]

  # --- R-side filtering for robustness ---

  # 1. Supplier filtering (R-side backup)
  if (supplier == "CSIRO") {
    # Match ANWC Sounds collection
    if ("institutionCode" %in% names(media_data) && "collectionCode" %in% names(media_data)) {
      is_csiro <- (media_data$institutionCode == "ANWC" & media_data$collectionCode == "Sounds")
      is_csiro[is.na(is_csiro)] <- FALSE
      media_data <- media_data[is_csiro, ]
    }
  }

  if (nrow(media_data) == 0) {
    message("No recordings found matching the specified filter criteria.")
    return(0)
  }

  # 2. R-side filtering for actual audio files
  # ALA media records can have audio URLs in many places.
  is_audio <- rep(FALSE, nrow(media_data))
  audio_urls <- rep(NA_character_, nrow(media_data))

  for (i in seq_len(nrow(media_data))) {
    row <- media_data[i, ]
    found_url <- NULL

    # Detect if any column contains a clear audio URL first
    for (col_name in names(row)) {
      val <- row[[col_name]]
      if (is.list(val)) val <- unlist(val)
      val <- as.character(val)
      audio_matches <- grep("https?://.*\\.(mp3|wav|m4a|ogg|aac)(\\?|$)", tolower(val), value = TRUE)
      if (length(audio_matches) > 0) {
        found_url <- val[grep(tolower(audio_matches[1]), tolower(val))[1]]
        break
      }
    }

    # If no URL with extension, check if the format/mimetype says audio
    if (is.null(found_url)) {
      if (!is.na(fmt_col) && fmt_col %in% names(row)) {
        fmt_val <- tolower(as.character(row[[fmt_col]]))
        if (grepl("audio|mpeg|wav", fmt_val) && !grepl("image|video", fmt_val)) {
          # Use the primary URL if it exists
          if (!is.na(url_col)) {
            primary_url <- as.character(row[[url_col]])
            if (!is.na(primary_url) && primary_url != "") {
              found_url <- primary_url
            }
          }
        }
      }
    }

    if (!is.null(found_url)) {
      is_audio[i] <- TRUE
      audio_urls[i] <- found_url
    }
  }

  # Filter and update
  media_data <- media_data[is_audio, ]
  audio_urls <- audio_urls[is_audio]

  if (nrow(media_data) > 0) {
    media_data$audio_url_verified <- audio_urls
    url_col <- "audio_url_verified"
  } else {
    return(0)
  }

  if (nrow(media_data) > target_n) {
    media_data <- media_data[1:target_n, ]
  }

  total_audio <- nrow(media_data)
  message(paste("Found", total_audio, "actual sound records in ALA (after robust URL verification)."))

  if (total_audio == 0) {
    return(0)
  }

  if (!download) {
    return(total_audio)
  }

  # Warning about MP3/WAV compatibility if needed
  if (!as_wav && !is.na(fmt_col) && !any(grepl("wav", media_data[[fmt_col]], ignore.case = TRUE))) {
    message("Note: All retrieved sounds appear to be compressed formats (MP3/M4A).")
    message("      If your downstream software requires WAV, use as_wav = TRUE or convert them manually.")
  }

  # 2. Download section
  audio_dir <- file.path(out_dir, "audio")
  if (!dir.exists(audio_dir)) dir.create(audio_dir, recursive = TRUE)

  csv_path <- file.path(out_dir, "metadata_ala.csv")
  if (!file.exists(csv_path)) {
    write.csv(
      data.frame(
        record_id = character(),
        taxon_name = character(),
        file_url = character(),
        file_path = character(),
        latitude = numeric(),
        longitude = numeric(),
        date = character(),
        license = character(),
        stringsAsFactors = FALSE
      ),
      csv_path,
      row.names = FALSE
    )
  }

  seen <- list.files(audio_dir)
  downloaded <- 0
  skipped <- 0
  failed <- 0

  # Limit to target_n
  to_download <- head(media_data, target_n)

  message(paste("Checking", nrow(to_download), "records for download..."))

  for (i in seq_len(nrow(to_download))) {
    row <- to_download[i, ]
    url <- row[[url_col]]

    if (is.null(url) || is.na(url) || url == "") {
      skipped <- skipped + 1
      next
    }

    # Safe ID extraction
    rec_id <- if (!is.na(id_col) && id_col %in% names(row)) as.character(row[[id_col]]) else NA_character_
    if (is.na(rec_id) || rec_id == "") rec_id <- paste0("ala_row_", i)

    # 1. Determine taxon string for filename
    tax_str <- ""
    scientific_name <- "unknown_species"
    if (!is.na(name_col) && name_col %in% names(row)) {
      scientific_name <- as.character(row[[name_col]])
      if (is.na(scientific_name)) scientific_name <- "unknown_species"
    }

    if (include_taxon_name) {
      cleaned_name <- gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", scientific_name))
      # Avoid doubling the name if the ID already contains it
      if (!is.na(cleaned_name) && !grepl(cleaned_name, rec_id, ignore.case = TRUE)) {
        tax_str <- paste0(cleaned_name, "_")
      }
    }

    # 2. Initial extension guess
    ext <- ".mp3"
    if (!is.na(fmt_col) && fmt_col %in% names(row)) {
      fmt_val <- tolower(as.character(row[[fmt_col]]))
      if (grepl("wav", fmt_val)) {
        ext <- ".wav"
      } else if (grepl("m4a|mp4|aac", fmt_val)) ext <- ".m4a"
    }
    # Fallback to URL extension if metadata is generic
    if (grepl("\\.(wav|m4a|mp3|mp4|aac|ogg)(\\?|$)", tolower(url))) {
      url_path <- tolower(url)
      if (grepl("\\.wav", url_path)) {
        ext <- ".wav"
      } else if (grepl("\\.m4a|\\.mp4|\\.aac", url_path)) {
        ext <- ".m4a"
      } else if (grepl("\\.mp3", url_path)) {
        ext <- ".mp3"
      } else if (grepl("\\.ogg", url_path)) ext <- ".ogg"
    }

    fname <- paste0(tax_str, rec_id, ext)

    if (fname %in% seen) {
      skipped <- skipped + 1
      next
    }

    dest <- file.path(audio_dir, fname)

    # Use httr::GET with write_disk for more robust binary downloads
    ok <- tryCatch(
      {
        ua <- httr::user_agent("ecoacoustic-utilities-ala-downloader/1.0")
        resp <- httr::GET(url, ua, httr::write_disk(dest, overwrite = TRUE), httr::timeout(120))
        httr::stop_for_status(resp)
        TRUE
      },
      error = function(e) {
        message(paste("Failed to download:", url, "-", e$message))
        if (file.exists(dest)) unlink(dest) # clean up partial/failed download
        FALSE
      }
    )

    if (ok) {
      downloaded <- downloaded + 1
      cat(sprintf("[%d/%d] %s\n", i, nrow(to_download), fname))

      # Append to metadata CSV
      # Safely extract lat, long, date
      lat <- if ("decimalLatitude" %in% names(row)) row[["decimalLatitude"]] else NA
      lon <- if ("decimalLongitude" %in% names(row)) row[["decimalLongitude"]] else NA
      dt <- if ("eventDate" %in% names(row)) as.character(row[["eventDate"]]) else NA

      # Calculate relative path for file_path
      dest_rel <- file.path("audio", fname)

      new_entry <- data.frame(
        record_id = rec_id,
        taxon_name = if (!is.na(name_col) && name_col %in% names(row)) row[[name_col]] else taxon_name,
        file_url = url,
        file_path = dest_rel,
        latitude = lat,
        longitude = lon,
        date = dt,
        license = if (!is.na(lic_col) && lic_col %in% names(row)) row[[lic_col]] else "Unknown",
        stringsAsFactors = FALSE
      )
      write.table(new_entry, csv_path, sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)
    } else {
      failed <- failed + 1
    }
  }

  message(sprintf(
    "Done. %d records checked: %d downloaded, %d skipped (included duplicates), %d failed.",
    nrow(to_download), downloaded, skipped, failed
  ))

  # 3. Post-download conversion to WAV
  if (as_wav && downloaded > 0) {
    message("Converting downloaded files to WAV format...")
    converted <- convert_to_wav(audio_dir, delete_original = TRUE)

    if (length(converted) > 0) {
      message(sprintf("Successfully converted %d files to WAV.", length(converted)))

      # Update metadata CSV to reflect new filenames
      meta <- read.csv(csv_path)
      # Extract just the basename of the converted files for matching
      conv_basenames_no_ext <- tools::file_path_sans_ext(basename(converted))

      updated <- FALSE
      for (i in seq_len(nrow(meta))) {
        meta_basename_no_ext <- tools::file_path_sans_ext(basename(meta$file_path[i]))
        if (meta_basename_no_ext %in% conv_basenames_no_ext) {
          # Update path and extension
          old_path <- meta$file_path[i]
          new_path <- paste0(tools::file_path_sans_ext(old_path), ".wav")
          meta$file_path[i] <- new_path
          updated <- TRUE
        }
      }

      if (updated) {
        write.csv(meta, csv_path, row.names = FALSE)
      }
    }
  }

  return(invisible(normalizePath(csv_path)))
}
