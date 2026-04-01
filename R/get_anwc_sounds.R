#' Download Sound Recordings from the ANWC (CSIRO) Sound Archive via ALA
#'
#' Searches the Australian National Wildlife Collection (ANWC / CSIRO) sound
#' archive as exposed through ALA (data resource \code{dr341},
#' \code{collectionCode == "Sounds"}) and downloads confirmed audio files.
#'
#' @param taxon_name Character. Scientific or common name of the taxon.
#' @param target_n Numeric. Target number of audio files to download.
#'   Default is 50.
#' @param download Logical. If \code{FALSE}, returns the count of confirmed
#'   audio records without downloading. Default is \code{TRUE}.
#' @param out_dir Character. Output directory. Creates \code{out_dir/audio/}
#'   and \code{out_dir/metadata_anwc.csv}. Default is \code{"sounds"}.
#' @param include_taxon_name Logical. Prepend taxon name to filenames.
#'   Default is \code{TRUE}.
#' @param as_wav Logical. Convert downloaded files to 48 kHz 16-bit PCM WAV
#'   after download (required for BirdNET and other bioacoustic pipelines).
#'   Default is \code{TRUE}.
#'
#' @return The absolute path to the metadata CSV, or \code{0} if no audio
#'   was found / downloaded.
#'
#' @details
#' The ANWC archive is stored in ALA under \code{dataResourceUid == "dr341"}
#' with \code{collectionCode == "Sounds"}. Crucially, these records are
#' \emph{not} indexed with \code{multimedia == "Sound"}, so they are invisible
#' to \code{atlas_media()} and \code{get_ala_sounds()}. Not all ANWC records
#' have audio uploaded; this function checks each occurrence individually
#' (via the ALA biocache and images APIs) to confirm audio before downloading.
#'
#' Because of the per-record API checks, this function is substantially slower
#' than \code{get_ala_sounds()}, especially for taxa with many ANWC records.
#' A progress counter is printed as audio files are confirmed.
#'
#' @examples
#' \dontrun{
#' galah_config(email = "your-email@example.com")
#' get_anwc_sounds("Vanellus miles", target_n = 10)
#' }
#'
#' @export
#' @importFrom galah galah_call galah_identify galah_filter atlas_occurrences
#' @importFrom httr GET content status_code add_headers timeout
#' @importFrom httr write_disk stop_for_status user_agent
#' @importFrom dplyr slice_head
#' @importFrom utils write.csv write.table read.csv head
get_anwc_sounds <- function(taxon_name,
                             target_n   = 50,
                             download   = TRUE,
                             out_dir    = "sounds",
                             include_taxon_name = TRUE,
                             as_wav     = TRUE) {

  if (!is_api_reachable("ala")) {
    message("ALA API is unreachable. Returning 0.")
    return(0)
  }

  message("Searching ANWC sound archive for: ", taxon_name, "...")

  # -- Fetch occurrences from ANWC Sounds collection 
  occ <- tryCatch(
    galah_call() |>
      galah_identify(taxon_name) |>
      galah_filter(collectionCode == "Sounds", dataResourceUid == "dr341") |> # nolint
      atlas_occurrences(),
    error = function(e) {
      message("Error fetching ANWC occurrences: ", e$message)
      NULL
    }
  )

  if (is.null(occ) || nrow(occ) == 0) {
    message("No ANWC sound records found for: ", taxon_name)
    return(0)
  }

  message(sprintf(
    "Found %d ANWC occurrence record(s). Checking for uploaded audio...",
    nrow(occ)
  ))

  # -- Per-record audio check 
  # ANWC records store the audio URL in the biocache `images` field, not in
  # atlas_media(). We fetch each record's biocache page to get the imageId,
  # then call the images API for mimeType and the actual download URL
  # (originalFileName). This is two HTTP calls per record.

  extract_uuid <- function(s) {
    m <- regmatches(
      s,
      regexpr(
        "[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}",
        s, ignore.case = TRUE
      )
    )
    if (length(m)) m else NA_character_
  }

  mime_to_ext <- function(mime) {
    mime <- tolower(mime)
    if (grepl("vnd\\.wave|x-wav|/wav", mime)) return(".wav")
    if (grepl("mp4|m4a|aac",           mime)) return(".m4a")
    if (grepl("ogg",                   mime)) return(".ogg")
    if (grepl("aiff|aif",              mime)) return(".aif")
    ".mp3"
  }

  audio_records <- list()
  checked       <- 0L

  for (i in seq_len(nrow(occ))) {

    if (length(audio_records) >= target_n) break

    rec_id <- if ("recordID" %in% names(occ)) occ$recordID[i] else NA_character_
    if (is.na(rec_id) || rec_id == "") next

    checked <- checked + 1L

    # 1. Biocache occurrence record -> images field
    bc <- tryCatch(
      httr::GET(
        paste0("https://biocache.ala.org.au/ws/occurrence/", rec_id),
        httr::add_headers(Accept = "application/json"),
        httr::timeout(20)
      ),
      error = function(e) NULL
    )
    if (is.null(bc) || httr::status_code(bc) != 200) next

    bc_data  <- httr::content(bc, as = "parsed")
    img_field <- bc_data$raw$occurrence$images
    if (is.null(img_field) || length(img_field) == 0) next

    img_id <- extract_uuid(as.character(img_field))
    if (is.na(img_id)) next

    # 2. Images API -> mimeType + originalFileName (actual download URL)
    meta_r <- tryCatch(
      httr::GET(
        paste0("https://images.ala.org.au/ws/image/", img_id),
        httr::timeout(15)
      ),
      error = function(e) NULL
    )
    if (is.null(meta_r) || httr::status_code(meta_r) != 200) next

    meta <- httr::content(meta_r, as = "parsed")
    mime <- as.character(meta$mimeType %||% "")
    dl_url <- as.character(meta$originalFileName %||% "")

    if (!grepl("^audio/", mime, ignore.case = TRUE)) next
    if (dl_url == "") next

    sn <- if ("scientificName" %in% names(occ)) occ$scientificName[i] else taxon_name
    audio_records[[length(audio_records) + 1]] <- list(
      record_id       = rec_id,
      image_id        = img_id,
      scientific_name = as.character(sn),
      download_url    = dl_url,
      mime            = mime,
      latitude        = if ("decimalLatitude"  %in% names(occ)) occ$decimalLatitude[i]  else NA,
      longitude       = if ("decimalLongitude" %in% names(occ)) occ$decimalLongitude[i] else NA,
      date            = if ("eventDate"        %in% names(occ)) as.character(occ$eventDate[i]) else NA,
      license         = "ALL-RIGHTS-RESERVED"  # standard for ANWC
    )

    message(sprintf(
      "  [%d/%d confirmed | %d checked] %s",
      length(audio_records), target_n, checked, sn
    ))
  }

  if (length(audio_records) == 0) {
    message(sprintf(
      "No audio files found in %d ANWC record(s) checked for: %s",
      checked, taxon_name
    ))
    return(0)
  }

  message(sprintf(
    "Confirmed %d audio file(s) from %d record(s) checked (%.0f%% hit rate).",
    length(audio_records), checked,
    100 * length(audio_records) / checked
  ))

  if (!download) return(length(audio_records))

  # -- Download 
  audio_dir <- file.path(out_dir, "audio")
  if (!dir.exists(audio_dir)) dir.create(audio_dir, recursive = TRUE)

  csv_path <- file.path(out_dir, "metadata_anwc.csv")
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

  seen            <- list.files(audio_dir)
  seen_this_batch <- character(0)
  downloaded      <- 0L
  skipped         <- 0L
  failed          <- 0L

  message("Downloading ", length(audio_records), " confirmed audio file(s)...")

  for (rec in audio_records) {

    sn      <- rec$scientific_name
    # Strip subgenus notation e.g. "Vanellus (Lobipluvia) miles" -> "Vanellus_miles"
    sn_clean <- gsub("\\s*\\([^)]*\\)\\s*", " ", sn)
    sn_clean <- trimws(sn_clean)

    tax_str <- ""
    if (include_taxon_name) {
      tax_str <- paste0(
        gsub("[^A-Za-z0-9_]", "", gsub(" ", "_", sn_clean)), "_"
      )
    }

    ext   <- mime_to_ext(rec$mime)
    fname <- paste0(tax_str, rec$image_id, "_CSIRO", ext)

    if (fname %in% seen || fname %in% seen_this_batch) {
      skipped <- skipped + 1L
      next
    }
    seen_this_batch <- c(seen_this_batch, fname)
    dest <- file.path(audio_dir, fname)

    ok <- tryCatch(
      {
        ua   <- httr::user_agent("ecoacoustic-utilities-anwc-downloader/1.0")
        resp <- httr::GET(
          rec$download_url, ua,
          httr::write_disk(dest, overwrite = TRUE),
          httr::timeout(120)
        )
        httr::stop_for_status(resp)
        TRUE
      },
      error = function(e) {
        message("Failed: ", rec$download_url, " - ", e$message)
        if (file.exists(dest)) unlink(dest)
        FALSE
      }
    )

    if (ok) {
      downloaded <- downloaded + 1L
      cat(sprintf("[%d/%d] %s\n", downloaded, length(audio_records), fname))

      write.table(
        data.frame(
          record_id   = rec$record_id,
          taxon_name  = sn,
          data_source = "CSIRO",
          file_url    = rec$download_url,
          file_path   = file.path("audio", fname),
          latitude    = rec$latitude,
          longitude   = rec$longitude,
          date        = rec$date,
          license     = rec$license,
          stringsAsFactors = FALSE
        ),
        csv_path, sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE
      )
    } else {
      failed <- failed + 1L
    }
  }

  message(sprintf(
    "Done. %d downloaded, %d skipped, %d failed.",
    downloaded, skipped, failed
  ))

  # -- WAV conversion 
  if (as_wav && downloaded > 0) {
    message("Converting to WAV...")
    converted <- convert_to_wav(audio_dir, delete_original = TRUE)
    if (length(converted) > 0) {
      message(sprintf("Converted %d file(s) to WAV.", length(converted)))
      meta <- read.csv(csv_path)
      stems <- tools::file_path_sans_ext(basename(converted))
      for (i in seq_len(nrow(meta))) {
        if (tools::file_path_sans_ext(basename(meta$file_path[i])) %in% stems) {
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
