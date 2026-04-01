#' Download Sound Recordings from Atlas of Living Australia (ALA)
#'
#' Queries the ALA for sound recordings of a specified taxon and downloads them.
#' This function improves upon basic media downloads by handling metadata
#' and limiting download counts.
#'
#' @param taxon_name Character. Scientific or common name of the taxon to
#'   search for.
#' @param target_n Numeric. Target number of sound files to download.
#'   Default is 50.
#' @param download Logical. If TRUE, downloads audio files. If FALSE, only
#'   returns the count of available recordings. Default is TRUE.
#' @param out_dir Character. Output directory for downloaded files. Will create
#'   subdirectories "audio" and "metadata_ala.csv". Default is "sounds".
#' @param include_taxon_name Logical. If TRUE, prepends the taxon name to the
#'   filename. Default is TRUE.
#' @param supplier Deprecated. ANWC/CSIRO recordings are not indexed under
#'   \code{multimedia == "Sound"} in ALA and cannot be retrieved via this
#'   function. Use \code{get_anwc_sounds()} instead.
#' @param as_wav Logical. If TRUE, converts downloaded audio files to 48 kHz
#'   16-bit PCM WAV (required for BirdNET and other bioacoustic pipelines).
#'   Default is TRUE.
#'
#' @return Character. The absolute path to the metadata CSV file
#'   created/updated, or 0 if no recordings were found/downloaded.
#'
#' @details
#' ALA sound records are retrieved via \code{atlas_media()} with a
#' \code{multimedia == "Sound"} filter. Because some ALA records tagged as
#' "Sound" contain non-audio media (e.g. \code{image/jpeg}), a secondary
#' \code{mimetype} check keeps only records with \code{audio/*} MIME types.
#'
#' The download URL for ALA sounds is the \code{image_url} field (ALA uses
#' this field for all media types). File extensions are derived from the
#' \code{mimetype} field, not from the URL (ALA URLs have no extension).
#'
#' A known galah 2.x bug causes a recycling error when fetching many sound
#' records in a single request. The function automatically falls back to
#' year-by-year fetching (most recent years first) to work around this.
#'
#' @examples
#' \dontrun{
#' galah_config(email = "your-email@example.com")
#' # Search and download up to 10 sounds as WAV
#' get_ala_sounds("Ninox boobook", target_n = 10, as_wav = TRUE)
#' }
#'
#' @export
#' @importFrom galah galah_call galah_identify galah_filter atlas_media
#' @importFrom httr GET write_disk stop_for_status user_agent timeout
#' @importFrom dplyr slice_head
#' @importFrom utils head write.csv write.table read.csv
get_ala_sounds <- function(taxon_name,
                           target_n = 50,
                           download = TRUE,
                           out_dir = "sounds",
                           include_taxon_name = TRUE,
                           supplier = c("all", "CSIRO"),
                           as_wav = TRUE) {
  supplier <- match.arg(supplier)
  if (supplier == "CSIRO") {
    warning(
      "supplier = 'CSIRO' is deprecated in get_ala_sounds(). ",
      "ANWC/CSIRO recordings are not indexed as multimedia == 'Sound' in ALA. ",
      "Use get_anwc_sounds() instead.",
      call. = FALSE
    )
  }

  if (!is_api_reachable("ala")) {
    message("ALA API is unreachable. Returning 0.")
    return(0)
  }

  message("Searching for sounds for: ", taxon_name, "...")

  # ── Fetch ──────────────────────────────────────────────────────────────────
  # galah 2.x has a recycling bug when many records have different numbers of
  # attached sounds/images. We try a bulk fetch first; on recycling error we
  # fall back to year-by-year (most recent first) and stop early once we have
  # enough records.

  message("Fetching sound media records...")
  result <- tryCatch(
    {
      galah_call() |>
        galah_identify(taxon_name) |>
        galah_filter(multimedia == "Sound") |> # nolint
        atlas_media()
    },
    error = function(e) list(error = e$message)
  )

  if (is.data.frame(result) && nrow(result) > 0) {
    media_data <- result
    message(sprintf("  Retrieved %d records.", nrow(media_data)))
  } else {
    # Recycling error or other failure — fall back to year-by-year
    if (is.list(result) && grepl("recycle|size", result$error, ignore.case = TRUE)) {
      message("  Bulk fetch hit a galah recycling error; switching to year-by-year fallback...")
    } else if (is.list(result)) {
      message("  Bulk fetch error: ", result$error, ". Trying year-by-year fallback...")
    } else {
      message("  No records returned. Trying year-by-year fallback...")
    }

    current_year <- as.integer(format(Sys.Date(), "%Y"))
    years <- seq(current_year, current_year - 40, by = -1)
    media_list <- list()
    total_so_far <- 0
    # Fetch 2x target to have headroom after mimetype filtering
    buffer_needed <- target_n * 2

    for (yr in years) {
      if (total_so_far >= buffer_needed) break
      yr_result <- tryCatch(
        galah_call() |>
          galah_identify(taxon_name) |>
          galah_filter(multimedia == "Sound", year == yr) |> # nolint
          atlas_media(),
        error = function(e) NULL
      )
      if (!is.null(yr_result) && is.data.frame(yr_result) && nrow(yr_result) > 0) {
        media_list[[length(media_list) + 1]] <- yr_result
        total_so_far <- total_so_far + nrow(yr_result)
        message(sprintf("  %d: %d records", yr, nrow(yr_result)))
      }
    }

    if (length(media_list) == 0) {
      message("No recordings found for: ", taxon_name)
      return(0)
    }
    media_data <- do.call(rbind, media_list)
    message(sprintf("  Year-by-year total: %d records.", nrow(media_data)))
  }

  # ── Filter to actual audio by mimetype ────────────────────────────────────
  # Even with multimedia == "Sound", some records have mimetype image/jpeg.
  # Keep only records whose mimetype starts with "audio/".
  if (!"mimetype" %in% names(media_data)) {
    message("No 'mimetype' column in results. Cannot verify audio records.")
    return(0)
  }

  mt <- as.character(media_data$mimetype)
  is_audio <- grepl("^audio/", mt, ignore.case = TRUE)
  n_dropped <- sum(!is_audio)
  if (n_dropped > 0) {
    message(sprintf(
      "  Dropped %d non-audio records (e.g. image/jpeg tagged as Sound).", n_dropped
    ))
  }
  media_data <- media_data[is_audio, ]

  if (nrow(media_data) == 0) {
    message("No audio records found for: ", taxon_name)
    return(0)
  }

  # ── Deduplicate by URL ─────────────────────────────────────────────────────
  if ("image_url" %in% names(media_data)) {
    dup <- duplicated(media_data$image_url)
    if (any(dup)) {
      message(sprintf("  Removed %d duplicate URLs.", sum(dup)))
      media_data <- media_data[!dup, ]
    }
  }

  # Trim to target
  if (nrow(media_data) > target_n) media_data <- media_data[seq_len(target_n), ]

  message(sprintf(
    "Found %d audio record(s) for %s.", nrow(media_data), taxon_name
  ))

  if (!download) return(nrow(media_data))

  # ── Download ───────────────────────────────────────────────────────────────
  audio_dir <- file.path(out_dir, "audio")
  if (!dir.exists(audio_dir)) dir.create(audio_dir, recursive = TRUE)

  csv_path <- file.path(out_dir, "metadata_ala.csv")
  if (!file.exists(csv_path)) {
    write.csv(
      data.frame(
        record_id   = character(),
        taxon_name  = character(),
        data_source = character(),
        file_url    = character(),
        file_path   = character(),
        latitude    = numeric(),
        longitude   = numeric(),
        date        = character(),
        license     = character(),
        stringsAsFactors = FALSE
      ),
      csv_path,
      row.names = FALSE
    )
  }

  seen           <- list.files(audio_dir)
  seen_this_batch <- character(0)
  downloaded     <- 0L
  skipped        <- 0L
  failed         <- 0L

  message("Downloading ", nrow(media_data), " records...")

  for (i in seq_len(nrow(media_data))) {
    row <- media_data[i, ]

    # URL — ALA uses image_url for all media types
    url <- if ("image_url" %in% names(row)) as.character(row$image_url) else NA_character_
    if (is.na(url) || url == "") {
      skipped <- skipped + 1L
      next
    }

    # Media ID — prefer media_id (clean UUID) over recordID (can embed species name)
    rec_id <- NA_character_
    if ("media_id" %in% names(row)) rec_id <- as.character(row$media_id)
    if (is.na(rec_id) || rec_id == "") {
      if ("recordID" %in% names(row)) rec_id <- as.character(row$recordID)
    }
    if (is.na(rec_id) || rec_id == "") {
      uuid_match <- regmatches(
        url,
        regexpr(
          "[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}",
          url, ignore.case = TRUE
        )
      )
      rec_id <- if (length(uuid_match) > 0) uuid_match else paste0("ala_row_", i)
    }

    # Scientific name for filename prefix
    scientific_name <- taxon_name
    if ("scientificName" %in% names(row)) {
      sn <- as.character(row$scientificName)
      if (!is.na(sn) && sn != "") scientific_name <- sn
    }

    tax_str <- ""
    if (include_taxon_name) {
      cleaned <- gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", scientific_name))
      tax_str <- paste0(cleaned, "_")
    }

    # Source label from dataResourceName
    drn <- if ("dataResourceName" %in% names(row)) as.character(row$dataResourceName) else NA_character_
    source_label <- if (is.na(drn) || drn == "") {
      "ALA"
    } else {
      drn_low <- tolower(drn)
      if (grepl("inaturalist", drn_low)) {
        "iNat"
      } else if (grepl("anwc|australian national wildlife|csiro", drn_low)) {
        "CSIRO"
      } else if (grepl("xeno.canto", drn_low)) {
        "XC"
      } else if (grepl("macaulay", drn_low)) {
        "ML"
      } else {
        # First alphanumeric word of the resource name
        gsub("[^A-Za-z0-9]", "", strsplit(drn, " ")[[1]][1])
      }
    }

    # Extension from mimetype (ALA URLs have no extension)
    mime <- tolower(as.character(row$mimetype))
    ext <- if (grepl("vnd\\.wave|wav|x-wav", mime)) {
      ".wav"
    } else if (grepl("mp4|m4a|aac", mime)) {
      ".m4a"
    } else if (grepl("ogg", mime)) {
      ".ogg"
    } else {
      ".mp3"  # audio/mpeg and unknown audio types
    }

    fname <- paste0(tax_str, rec_id, "_", source_label, ext)

    if (fname %in% seen || fname %in% seen_this_batch) {
      skipped <- skipped + 1L
      next
    }
    seen_this_batch <- c(seen_this_batch, fname)
    dest <- file.path(audio_dir, fname)

    ok <- tryCatch(
      {
        ua   <- httr::user_agent("ecoacoustic-utilities-ala-downloader/1.0")
        resp <- httr::GET(
          url, ua, httr::write_disk(dest, overwrite = TRUE), httr::timeout(120)
        )
        httr::stop_for_status(resp)
        TRUE
      },
      error = function(e) {
        message("Failed to download: ", url, " - ", e$message)
        if (file.exists(dest)) unlink(dest)
        FALSE
      }
    )

    if (ok) {
      downloaded <- downloaded + 1L
      cat(sprintf("[%d/%d] %s\n", downloaded, nrow(media_data), fname))

      lat <- if ("decimalLatitude"  %in% names(row)) row$decimalLatitude  else NA
      lon <- if ("decimalLongitude" %in% names(row)) row$decimalLongitude else NA
      dt  <- if ("eventDate"        %in% names(row)) as.character(row$eventDate) else NA
      lic <- if ("license"          %in% names(row)) as.character(row$license) else "Unknown"

      write.table(
        data.frame(
          record_id   = rec_id,
          taxon_name  = scientific_name,
          data_source = source_label,
          file_url    = url,
          file_path   = file.path("audio", fname),
          latitude    = lat,
          longitude   = lon,
          date        = dt,
          license     = lic,
          stringsAsFactors = FALSE
        ),
        csv_path, sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE
      )
    } else {
      failed <- failed + 1L
    }
  }

  message(sprintf(
    "Done. %d downloaded, %d skipped (duplicates/missing URL), %d failed.",
    downloaded, skipped, failed
  ))

  # ── WAV conversion ─────────────────────────────────────────────────────────
  if (as_wav && downloaded > 0) {
    message("Converting downloaded files to WAV format...")
    converted <- convert_to_wav(audio_dir, delete_original = TRUE)

    if (length(converted) > 0) {
      message(sprintf("Successfully converted %d files to WAV.", length(converted)))
      meta <- read.csv(csv_path)
      conv_stems <- tools::file_path_sans_ext(basename(converted))
      for (i in seq_len(nrow(meta))) {
        if (tools::file_path_sans_ext(basename(meta$file_path[i])) %in% conv_stems) {
          meta$file_path[i] <- paste0(
            tools::file_path_sans_ext(meta$file_path[i]), ".wav"
          )
        }
      }
      write.csv(meta, csv_path, row.names = FALSE)
    }
  }

  invisible(normalizePath(csv_path))
}
