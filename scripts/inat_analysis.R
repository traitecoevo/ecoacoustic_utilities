library(httr)
library(jsonlite)

orthoptera_species_with_real_sounds <- function(
    min_recordings   = 100,
    taxon_id         = 47651,   # Orthoptera
    taxon_name       = NULL,    # optional: resolve instead of hardcoding
    place_name       = "Australia",
    place_id         = NULL,    # if NULL, look up
    quality_grade    = "research",
    per_page         = 200,
    max_pages        = Inf,
    allowed_licenses = tolower(c("cc0","cc-by","cc-by-sa","cc-by-nc","cc-by-nc-sa"))
) {
  ua <- user_agent("inat-orthoptera-sounds/1.0 (your_email@example.com)")
  
  ## --- Resolve taxon_id from name if requested ---
  if (!is.null(taxon_name) && is.null(taxon_id)) {
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
  }
  
  ## --- Resolve place_id from name if needed ---
  if (is.null(place_id)) {
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
  
  base <- "https://api.inaturalist.org/v1/observations"
  
  page        <- 1L
  pages_done  <- 0L
  
  # per-species counters (taxon_id -> n_recordings)
  counts   <- list()
  tax_info <- list()  # store names by taxon_id
  
  repeat {
    if (pages_done >= max_pages) {
      message("Reached max_pages = ", max_pages, ", stopping early.")
      break
    }
    
    params <- list(
      taxon_id      = taxon_id,
      place_id      = place_id,
      sounds        = "true",         # SAME trick as your downloader
      quality_grade = quality_grade,  # research-grade filter
      per_page      = per_page,
      page          = page,
      order         = "desc",
      order_by      = "created_at"
    )
    
    resp <- GET(base, query = params, ua, timeout(60))
    stop_for_status(resp)
    j <- content(resp, as = "parsed", type = "application/json")
    
    res <- j$results
    if (length(res) == 0) {
      message("No more results at page ", page, ".")
      break
    }
    
    message("Processing page ", page, " (", length(res), " observations)")
    pages_done <- pages_done + 1L
    
    for (obs in res) {
      # Taxon sanity check
      tx <- obs$taxon
      if (is.null(tx)) next
      if (!identical(tx$rank, "species")) next
      
      tax_id_chr <- as.character(tx$id)
      
      # Must have sounds list
      if (is.null(obs$sounds) || length(obs$sounds) == 0) next
      
      # Count sounds in this observation that pass your license rules
      n_valid_sounds <- 0L
      for (s in obs$sounds) {
        url <- s$file_url
        if (is.null(url) || !nzchar(url)) next
        
        lic <- tolower(ifelse(is.null(s$license_code), "", s$license_code))
        
        # EXACT same semantics as your downloader:
        # - if license_code is non-empty and not in allowed_licenses -> skip
        # - if license_code is empty (NA/"") -> accept
        if (nzchar(lic) && !(lic %in% allowed_licenses)) next
        
        n_valid_sounds <- n_valid_sounds + 1L
      }
      
      if (n_valid_sounds == 0L) next
      
      # init record if first time we see this species
      if (is.null(counts[[tax_id_chr]])) {
        counts[[tax_id_chr]] <- 0L
        tax_info[[tax_id_chr]] <- list(
          scientific_name = tx$name,
          common_name     = ifelse(
            is.null(tx$preferred_common_name),
            NA_character_,
            tx$preferred_common_name
          )
        )
      }
      
      counts[[tax_id_chr]] <- counts[[tax_id_chr]] + n_valid_sounds
    }
    
    page <- page + 1L
    Sys.sleep(1.1)  # be kind to the API
  }
  
  if (length(counts) == 0) {
    message("No species with valid sounds found under these filters.")
    return(data.frame())
  }
  
  # Build output data.frame
  tax_ids <- names(counts)
  df <- do.call(
    rbind,
    lapply(tax_ids, function(id) {
      info <- tax_info[[id]]
      data.frame(
        taxon_id        = as.integer(id),
        scientific_name = info$scientific_name,
        common_name     = info$common_name,
        n_recordings    = counts[[id]],
        stringsAsFactors = FALSE
      )
    })
  )
  
  # Filter & sort
  df <- df[df$n_recordings >= min_recordings, ]
  df[order(-df$n_recordings), ]
}

# Species of Orthoptera with > 100 "real" sound files from Australia
orth_aus_100_real <- orthoptera_species_with_real_sounds(
  min_recordings   = 3,
  place_name       = "Australia",
  allowed_licenses = tolower(c("cc0","cc-by","cc-by-sa","cc-by-nc","cc-by-nc-sa"))
)

orth_aus_100_real
