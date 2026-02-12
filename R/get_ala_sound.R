# Declare global variables for R CMD check
utils::globalVariables(c("multimedia", "recordID", "media_url", "scientificName", "format"))

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
#'
#' @return Integer. Total number of matching records found in ALA.
#'
#' @details
#' You must register your email with ALA first and configure it using
#' \code{galah_config(email = "your-email@example.com")}.
#'
#' The function uses \code{galah_select()} to limit retrieved columns, which
#' avoids known issues with \code{unnest_longer()} errors in the \code{galah} package
#' when dealing with complex media metadata.
#'
#' @examples
#' \dontrun{
#' galah_config(email = "your-email@example.com")
#' # Search and download up to 10 sounds
#' get_ala_sounds("Teleogryllus commodus", target_n = 10)
#' }
#'
#' @export
#' @importFrom galah galah_call galah_identify galah_filter galah_select atlas_media
#' @importFrom httr GET write_disk stop_for_status user_agent timeout
#' @importFrom dplyr select slice_head
#' @importFrom utils head write.csv write.table
get_ala_sounds <- function(taxon_name,
                           target_n = 50,
                           download = TRUE,
                           out_dir = "sounds",
                           include_taxon_name = TRUE) {
  # Check API availability
  if (!is_api_reachable("ala")) {
    message("ALA API is unreachable. Returning 0.")
    return(0)
  }

  message(paste0("Searching for sounds for: ", taxon_name, "..."))

  # 1. Build the query
  # We use slice_head(n = target_n) to stop the search early. This is much more
  # efficient and avoids errors occurring in the long tail of results (e.g., row 182).
  # We use galah_select with group = "media" to ensure URLs are returned,
  # but explicitly include scientificName, recordID and media_id to ensure
  # they aren't dropped and we have unique identifiers.
  query <- galah_call() |>
    galah_identify(taxon_name) |>
    galah_filter(multimedia == "Sound") |>
    galah_select(recordID, scientificName, media_id, group = "media") |>
    slice_head(n = target_n)

  # atlas_media() will return a data frame with media metadata
  media_data <- tryCatch(
    {
      query |> atlas_media()
    },
    error = function(e) {
      message("Error during ALA media search: ", e$message)
      return(NULL)
    }
  )

  if (is.null(media_data)) {
    return(0)
  }

  total_records <- nrow(media_data)

  # Check if any media was found
  if (total_records == 0) {
    message("No sound files found for this taxon.")
    return(0)
  }

  # --- Dynamic Column Detection ---
  # ALA/galah returns varying column names depending on the node, version, and query.
  # We detect the most important ones defensively.
  cols <- names(media_data)

  # 1. URL column (media_url is standard but sometimes image_url or others appear)
  url_col <- intersect(c("media_url", "image_url", "full_res", "multimedia"), cols)[1]

  # 2. Record ID column
  id_col <- intersect(c("recordID", "occurrenceID", "id"), cols)[1]

  # 3. Scientific Name column
  name_col <- intersect(c("scientificName", "species", "name"), cols)[1]

  # 4. License column (often becomes dcterms:license)
  lic_col <- intersect(c("license", "dcterms:license", "multimediaLicence"), cols)[1]

  # 5. Format/Extension column
  fmt_col <- intersect(c("format", "mimetype", "multimedia_format"), cols)[1]

  if (is.na(url_col)) {
    message("Could not find a URL column in ALA media results.")
    return(0)
  }

  message(paste("Found", total_records, "sound records in ALA."))

  if (!download) {
    return(total_records)
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
        license = character(),
        stringsAsFactors = FALSE
      ),
      csv_path,
      row.names = FALSE
    )
  }

  seen <- list.files(audio_dir)
  downloaded <- 0

  # Limit to target_n
  to_download <- head(media_data, target_n)

  message(paste("Starting download of up to", nrow(to_download), "files..."))

  for (i in seq_len(nrow(to_download))) {
    row <- to_download[i, ]
    url <- row[[url_col]]

    if (is.null(url) || is.na(url) || url == "") next

    # Safe ID extraction
    rec_id <- if (!is.na(id_col) && id_col %in% names(row)) as.character(row[[id_col]]) else NA_character_
    if (is.na(rec_id) || rec_id == "") rec_id <- paste0("ala_row_", i)

    # Determine extension safely
    ext <- ".mp3" # default
    if (!is.na(fmt_col) && fmt_col %in% names(row)) {
      fmt_val <- row[[fmt_col]]
      if (!is.null(fmt_val) && !is.na(fmt_val) && length(fmt_val) > 0) {
        if (grepl("wav", fmt_val, ignore.case = TRUE)) ext <- ".wav"
        if (grepl("m4a|mp4", fmt_val, ignore.case = TRUE)) ext <- ".m4a"
      }
    }

    # Construct filename
    tax_str <- ""
    if (include_taxon_name) {
      val <- if (!is.na(name_col) && name_col %in% names(row)) as.character(row[[name_col]]) else NA_character_
      if (is.na(val) || val == "") val <- "unknown_species"
      cleaned_name <- gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", val))
      tax_str <- paste0(cleaned_name, "_")
    }
    fname <- paste0(tax_str, rec_id, ext)

    if (fname %in% seen) {
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
      cat(sprintf("[%d/%d] %s\n", downloaded, nrow(to_download), fname))

      # Safe extraction for metadata
      lic_val <- if (!is.na(lic_col) && lic_col %in% names(row)) row[[lic_col]] else NA_character_
      tax_val <- if (!is.na(name_col) && name_col %in% names(row)) row[[name_col]] else NA_character_

      write.table(
        data.frame(
          record_id      = rec_id,
          taxon_name     = tax_val,
          file_url       = url,
          file_path      = dest,
          license        = lic_val
        ),
        csv_path,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        append = TRUE
      )
    }
  }

  message(sprintf("Done. Downloaded %d new files into %s", downloaded, audio_dir))

  return(total_records)
}
