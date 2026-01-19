# install.packages(c("httr","jsonlite"))


get_inat_sounds <- function(
    taxon_name,
    place_name = NULL,
    target_n = 300,
    download = FALSE,
    out_dir = "sounds",
    allowed_licenses = tolower(c("cc0","cc-by","cc-by-sa","cc-by-nc","cc-by-nc-sa")),
    use_place_filter = TRUE,          # set FALSE for global queries
    quality = c("research", "all")    # NEW: "research" or "all"
) {
  library(httr)
  library(jsonlite)
  quality <- match.arg(quality)
  
  ua   <- user_agent("inat-audio-downloader/1.0 (your_email@example.com)")
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
  
  message("Found ", total_records,
          " records with sounds for taxon '", taxon_name,
          "' ", loc_msg, " (", qual_msg, ").")
  
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
        sound_id       = integer(),
        file_url       = character(),
        file_path      = character(),
        license_code   = character(),
        stringsAsFactors = FALSE
      ),
      csv_path,
      row.names = FALSE
    )
  }
  
  audio_dir  <- file.path(out_dir, "audio")
  if (!dir.exists(audio_dir)) dir.create(audio_dir, recursive = TRUE, showWarnings = FALSE)
  
  seen        <- list.files(audio_dir)
  already_had <- length(seen)
  downloaded  <- 0
  page        <- 1
  
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
      
      for (s in obs$sounds) {
        if (downloaded >= target_n) break
        url <- s$file_url
        if (is.null(url) || !nzchar(url)) next
        
        lic <- tolower(ifelse(is.null(s$license_code), "", s$license_code))
        if (nzchar(lic) && !(lic %in% allowed_licenses)) next
        
        ext <- tolower(sub(".*(\\.mp3|\\.wav|\\.m4a|\\.ogg).*", "\\1", url))
        if (!grepl("^\\.", ext)) ext <- ".mp3"
        fname <- sprintf("%s_%s%s", obs$id, s$id, ext)
        if (fname %in% seen) next
        
        dest <- file.path(audio_dir, fname)
        ok <- tryCatch({
          bin <- content(GET(url, ua, timeout(120)), "raw")
          writeBin(bin, dest)
          TRUE
        }, error = function(e) FALSE)
        
        if (ok) {
          downloaded <- downloaded + 1
          seen <- c(seen, fname)
          cat(sprintf("[%d/%d] %s\n", downloaded, target_n, fname))
          write.table(
            data.frame(
              observation_id = obs$id,
              sound_id       = s$id,
              file_url       = url,
              file_path      = dest,
              license_code   = lic
            ),
            csv_path,
            sep       = ",",
            row.names = FALSE,
            col.names = FALSE,
            append    = TRUE
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

taxa<-"Turnix maculosus"
# Just get the number of records, no download
n <- get_inat_sounds(taxa, use_place_filter=FALSE, download = FALSE)
n

# Get number of records AND download up to 300 audio files
n2 <- get_inat_sounds(taxa, place_name = "Australia", target_n = 900, download = TRUE,quality="research")


ç# n_global_all <- get_inat_sounds("Phaps chalcopter",
#                                 place_name = "Australia",
#                                 quality = "research",
#                                 target_n = 300,
#                                 download = TRUE)
