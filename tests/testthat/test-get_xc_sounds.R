library(testthat)

# ---------------------------------------------------------------------------
# Helpers for building mock XC API responses
# ---------------------------------------------------------------------------

make_xc_recording <- function(
    id     = "111111",
    q      = "A",
    lic    = "//creativecommons.org/licenses/by-nc-sa/4.0/",
    file   = "//xeno-canto.org/111111/download",
    rec    = "Test Recordist",
    cnt    = "Australia",
    loc    = "Test Location",
    lat    = "-33.8",
    lng    = "151.2",
    date   = "2023-01-15",
    type   = "call",
    length = "0:30") {
  list(
    id = id, q = q, lic = lic, file = file, rec = rec,
    cnt = cnt, loc = loc, lat = lat, lng = lng,
    date = date, type = type, length = length
  )
}

make_xc_page <- function(recordings = list(), num_recordings = NULL,
                         num_pages = 1, page = 1) {
  if (is.null(num_recordings)) num_recordings <- length(recordings)
  list(
    numRecordings = as.character(num_recordings),
    numSpecies    = "1",
    page          = page,
    numPages      = num_pages,
    recordings    = recordings
  )
}

# A minimal fake httr response object (only needs to satisfy stop_for_status)
fake_resp <- list(status_code = 200L)

# Convenience: a dummy key used across all mocked tests
TEST_KEY <- "test_key_123"

# ---------------------------------------------------------------------------
# 1. Parameter structure & defaults
# ---------------------------------------------------------------------------

test_that("get_xc_sounds has correct parameter structure", {
  params   <- names(formals(get_xc_sounds))
  expected <- c(
    "taxon_name", "country", "api_key", "target_n", "download",
    "out_dir", "allowed_licenses", "quality",
    "sound_type", "include_taxon_name", "as_wav"
  )
  for (p in expected) {
    expect_true(p %in% params, info = paste("missing param:", p))
  }
})

test_that("get_xc_sounds defaults are correct", {
  d <- formals(get_xc_sounds)
  expect_equal(d$target_n, 300)
  expect_equal(d$download, TRUE)
  expect_equal(d$out_dir, "sounds")
  expect_null(d$country)
  expect_null(d$sound_type)
  expect_equal(d$include_taxon_name, TRUE)
  expect_equal(d$as_wav, FALSE)
  expect_equal(eval(d$quality), c("A", "B"))
})

# ---------------------------------------------------------------------------
# 2. Missing API key raises a clear error
# ---------------------------------------------------------------------------

test_that("get_xc_sounds errors with a helpful message when api_key is missing", {
  withr::local_envvar(XC_API_KEY = "")
  expect_error(
    get_xc_sounds("Turnix maculosus"),
    regexp = "xeno-canto.org/account"
  )
})

# ---------------------------------------------------------------------------
# 3. .xc_license_url_to_code helper
# ---------------------------------------------------------------------------

test_that(".xc_license_url_to_code maps CC URLs to short codes", {
  cc <- "//creativecommons.org"
  expect_equal(
    .xc_license_url_to_code(paste0(cc, "/publicdomain/zero/1.0/")),
    "cc0"
  )
  expect_equal(
    .xc_license_url_to_code(paste0(cc, "/licenses/by/4.0/")),
    "cc-by"
  )
  expect_equal(
    .xc_license_url_to_code(paste0(cc, "/licenses/by-sa/4.0/")),
    "cc-by-sa"
  )
  expect_equal(
    .xc_license_url_to_code(paste0(cc, "/licenses/by-nc/4.0/")),
    "cc-by-nc"
  )
  expect_equal(
    .xc_license_url_to_code(paste0(cc, "/licenses/by-nc-sa/4.0/")),
    "cc-by-nc-sa"
  )
  expect_equal(
    .xc_license_url_to_code(paste0(cc, "/licenses/by-nd/4.0/")),
    "cc-by-nd"
  )
  expect_equal(
    .xc_license_url_to_code(paste0(cc, "/licenses/by-nc-nd/4.0/")),
    "cc-by-nc-nd"
  )
  expect_equal(.xc_license_url_to_code(""), "")
  expect_equal(.xc_license_url_to_code("//some-unknown-license.org/"), "")
})

# ---------------------------------------------------------------------------
# 4. API unreachable → returns 0
# ---------------------------------------------------------------------------

test_that("get_xc_sounds returns 0 when API is unreachable", {
  local_mocked_bindings(
    is_api_reachable = function(...) FALSE,
    .package = "EcoacousticUtilities"
  )
  expect_equal(get_xc_sounds("Turnix maculosus", api_key = TEST_KEY), 0)
})

# ---------------------------------------------------------------------------
# 5. download = FALSE returns total count without writing files
# ---------------------------------------------------------------------------

test_that("get_xc_sounds returns total count when download = FALSE", {
  page1 <- make_xc_page(num_recordings = 42, num_pages = 1)

  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    GET              = function(...) fake_resp,
    stop_for_status  = function(...) invisible(NULL),
    content          = function(...) page1,
    .package = "EcoacousticUtilities"
  )

  result <- get_xc_sounds("Turnix maculosus", api_key = TEST_KEY, download = FALSE)
  expect_equal(result, 42L)
})

# ---------------------------------------------------------------------------
# 6. API key is included in every query
# ---------------------------------------------------------------------------

test_that("get_xc_sounds passes api_key in every request", {
  page1 <- make_xc_page(num_recordings = 0, num_pages = 1)

  state <- new.env(parent = emptyenv())
  state$queries <- list()

  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    GET              = function(url, query = list(), ...) {
      state$queries <- c(state$queries, list(query))
      fake_resp
    },
    stop_for_status  = function(...) invisible(NULL),
    content          = function(...) page1,
    .package = "EcoacousticUtilities"
  )

  get_xc_sounds("Turnix maculosus", api_key = TEST_KEY, download = FALSE)

  for (q in state$queries) {
    expect_equal(q$key, TEST_KEY)
  }
})

# ---------------------------------------------------------------------------
# 7. Quality filtering — only requested grades are downloaded
# ---------------------------------------------------------------------------

test_that("get_xc_sounds filters recordings by quality grade", {
  recs <- list(
    make_xc_recording(id = "001", q = "A"),
    make_xc_recording(id = "002", q = "C"),  # should be skipped
    make_xc_recording(id = "003", q = "B"),
    make_xc_recording(id = "004", q = "D")   # should be skipped
  )
  page1 <- make_xc_page(recordings = recs, num_pages = 1)
  tmp   <- withr::local_tempdir()

  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    GET              = function(...) fake_resp,
    stop_for_status  = function(...) invisible(NULL),
    content          = function(resp, as = "parsed", ...) {
      if (identical(as, "raw")) as.raw(0L) else page1
    },
    .package = "EcoacousticUtilities"
  )

  get_xc_sounds(
    "Turnix maculosus", api_key = TEST_KEY,
    target_n = 10, download = TRUE,
    out_dir = tmp, quality = c("A", "B")
  )

  audio_files <- list.files(file.path(tmp, "audio"))
  expect_true(
    any(grepl("XC001", audio_files)),
    info = "A-grade should be downloaded"
  )
  expect_true(
    any(grepl("XC003", audio_files)),
    info = "B-grade should be downloaded"
  )
  expect_false(
    any(grepl("XC002", audio_files)),
    info = "C-grade should be skipped"
  )
  expect_false(
    any(grepl("XC004", audio_files)),
    info = "D-grade should be skipped"
  )
})

# ---------------------------------------------------------------------------
# 8. License filtering — disallowed licenses are skipped
# ---------------------------------------------------------------------------

test_that("get_xc_sounds skips recordings with disallowed licenses", {
  cc <- "//creativecommons.org"
  recs <- list(
    make_xc_recording(
      id  = "010",
      lic = paste0(cc, "/licenses/by-nc-sa/4.0/")  # allowed
    ),
    make_xc_recording(
      id  = "011",
      lic = paste0(cc, "/licenses/by-nd/4.0/")      # disallowed
    ),
    make_xc_recording(
      id  = "012",
      lic = paste0(cc, "/licenses/by-nc-nd/4.0/")   # disallowed
    ),
    make_xc_recording(
      id  = "013",
      lic = paste0(cc, "/publicdomain/zero/1.0/")   # cc0 — allowed
    )
  )
  page1 <- make_xc_page(recordings = recs, num_pages = 1)
  tmp   <- withr::local_tempdir()

  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    GET              = function(...) fake_resp,
    stop_for_status  = function(...) invisible(NULL),
    content          = function(resp, as = "parsed", ...) {
      if (identical(as, "raw")) as.raw(0L) else page1
    },
    .package = "EcoacousticUtilities"
  )

  get_xc_sounds(
    "Turnix maculosus", api_key = TEST_KEY,
    target_n = 10, download = TRUE, out_dir = tmp,
    allowed_licenses = c(
      "cc0", "cc-by", "cc-by-sa", "cc-by-nc", "cc-by-nc-sa"
    )
  )

  audio_files <- list.files(file.path(tmp, "audio"))
  expect_true(any(grepl("XC010", audio_files)))
  expect_true(any(grepl("XC013", audio_files)))
  expect_false(any(grepl("XC011", audio_files)))
  expect_false(any(grepl("XC012", audio_files)))
})

# ---------------------------------------------------------------------------
# 9. Skip existing files — already-present files are not re-downloaded
# ---------------------------------------------------------------------------

test_that("get_xc_sounds skips files that already exist", {
  recs <- list(
    make_xc_recording(id = "020"),
    make_xc_recording(id = "021")
  )
  page1 <- make_xc_page(recordings = recs, num_pages = 1)

  tmp       <- withr::local_tempdir()
  audio_dir <- file.path(tmp, "audio")
  dir.create(audio_dir, recursive = TRUE)

  # Pre-create one file to simulate an existing download
  existing <- file.path(audio_dir, "Turnix_maculosus_XC020.mp3")
  writeLines("placeholder", existing)
  pre_mtime <- file.info(existing)$mtime

  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    GET              = function(...) fake_resp,
    stop_for_status  = function(...) invisible(NULL),
    content          = function(resp, as = "parsed", ...) {
      if (identical(as, "raw")) as.raw(0L) else page1
    },
    .package = "EcoacousticUtilities"
  )

  Sys.sleep(1.1)  # ensure mtime would differ if file were re-written
  get_xc_sounds(
    "Turnix maculosus", api_key = TEST_KEY,
    target_n = 10, download = TRUE, out_dir = tmp
  )

  audio_files <- list.files(audio_dir)
  expect_true(any(grepl("XC021", audio_files)))
  # XC020 should not have been re-written (mtime unchanged)
  expect_equal(file.info(existing)$mtime, pre_mtime)
})

# ---------------------------------------------------------------------------
# 10. target_n respected — stops after enough records checked
# ---------------------------------------------------------------------------

test_that("get_xc_sounds respects target_n", {
  recs  <- lapply(as.character(100:109), function(id) make_xc_recording(id = id))
  page1 <- make_xc_page(recordings = recs, num_recordings = 100, num_pages = 2)
  tmp   <- withr::local_tempdir()

  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    GET              = function(...) fake_resp,
    stop_for_status  = function(...) invisible(NULL),
    content          = function(resp, as = "parsed", ...) {
      if (identical(as, "raw")) as.raw(0L) else page1
    },
    .package = "EcoacousticUtilities"
  )

  get_xc_sounds(
    "Turnix maculosus", api_key = TEST_KEY,
    target_n = 5, download = TRUE, out_dir = tmp
  )
  audio_files <- list.files(file.path(tmp, "audio"))
  expect_lte(length(audio_files), 5L)
})

# ---------------------------------------------------------------------------
# 11. Pagination — second page is fetched when first page is exhausted
# ---------------------------------------------------------------------------

test_that("get_xc_sounds fetches page 2 when target_n exceeds page 1", {
  recs_p1 <- lapply(as.character(200:204), function(id) make_xc_recording(id = id))
  recs_p2 <- lapply(as.character(205:209), function(id) make_xc_recording(id = id))

  page_responses <- list(
    make_xc_page(
      recordings = recs_p1, num_recordings = 10, num_pages = 2, page = 1
    ),
    make_xc_page(
      recordings = recs_p2, num_recordings = 10, num_pages = 2, page = 2
    )
  )

  state <- new.env(parent = emptyenv())
  state$call_count <- 0L

  tmp <- withr::local_tempdir()

  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    GET              = function(...) fake_resp,
    stop_for_status  = function(...) invisible(NULL),
    content          = function(resp, as = "parsed", ...) {
      if (identical(as, "raw")) return(as.raw(0L))
      state$call_count <- state$call_count + 1L
      if (state$call_count <= 1L) page_responses[[1]] else page_responses[[2]]
    },
    .package = "EcoacousticUtilities"
  )

  get_xc_sounds(
    "Turnix maculosus", api_key = TEST_KEY,
    target_n = 8, download = TRUE, out_dir = tmp
  )

  audio_files <- list.files(file.path(tmp, "audio"))
  expect_true(
    any(grepl("XC20[0-4]", audio_files)),
    info = "page 1 files present"
  )
  expect_true(
    any(grepl("XC20[5-9]", audio_files)),
    info = "page 2 files present"
  )
})

# ---------------------------------------------------------------------------
# 12. Country and sound_type are included in the XC query string
# ---------------------------------------------------------------------------

test_that("get_xc_sounds includes country filter in query", {
  page1 <- make_xc_page(num_recordings = 0, num_pages = 1)

  state <- new.env(parent = emptyenv())
  state$captured_query <- NULL

  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    GET              = function(url, query = list(), ...) {
      state$captured_query <- query
      fake_resp
    },
    stop_for_status  = function(...) invisible(NULL),
    content          = function(...) page1,
    .package = "EcoacousticUtilities"
  )

  get_xc_sounds(
    "Turnix maculosus", api_key = TEST_KEY,
    country = "Australia", download = FALSE
  )
  expect_true(
    grepl('sp:"Turnix maculosus"', state$captured_query$query, fixed = TRUE)
  )
  expect_true(
    grepl('cnt:"Australia"', state$captured_query$query, fixed = TRUE)
  )
})

test_that("get_xc_sounds includes sound_type filter in query", {
  page1 <- make_xc_page(num_recordings = 0, num_pages = 1)

  state <- new.env(parent = emptyenv())
  state$captured_query <- NULL

  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    GET              = function(url, query = list(), ...) {
      state$captured_query <- query
      fake_resp
    },
    stop_for_status  = function(...) invisible(NULL),
    content          = function(...) page1,
    .package = "EcoacousticUtilities"
  )

  get_xc_sounds(
    "Turnix maculosus", api_key = TEST_KEY,
    sound_type = "call", download = FALSE
  )
  expect_true(
    grepl('sp:"Turnix maculosus"', state$captured_query$query, fixed = TRUE)
  )
  expect_true(
    grepl('type:"call"', state$captured_query$query, fixed = TRUE)
  )
})

# ---------------------------------------------------------------------------
# 13. Taxon name prefix in filename controlled by include_taxon_name
# ---------------------------------------------------------------------------

test_that("get_xc_sounds respects include_taxon_name = FALSE", {
  recs  <- list(make_xc_recording(id = "300"))
  page1 <- make_xc_page(recordings = recs, num_pages = 1)
  tmp   <- withr::local_tempdir()

  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    GET              = function(...) fake_resp,
    stop_for_status  = function(...) invisible(NULL),
    content          = function(resp, as = "parsed", ...) {
      if (identical(as, "raw")) as.raw(0L) else page1
    },
    .package = "EcoacousticUtilities"
  )

  get_xc_sounds(
    "Turnix maculosus", api_key = TEST_KEY,
    target_n = 1, download = TRUE,
    out_dir = tmp, include_taxon_name = FALSE
  )

  audio_files <- list.files(file.path(tmp, "audio"))
  expect_true(any(grepl("^XC300\\.mp3$", audio_files)))
  expect_false(any(grepl("Turnix", audio_files)))
})

test_that("get_xc_sounds prepends taxon name when include_taxon_name = TRUE", {
  recs  <- list(make_xc_recording(id = "301"))
  page1 <- make_xc_page(recordings = recs, num_pages = 1)
  tmp   <- withr::local_tempdir()

  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    GET              = function(...) fake_resp,
    stop_for_status  = function(...) invisible(NULL),
    content          = function(resp, as = "parsed", ...) {
      if (identical(as, "raw")) as.raw(0L) else page1
    },
    .package = "EcoacousticUtilities"
  )

  get_xc_sounds(
    "Turnix maculosus", api_key = TEST_KEY,
    target_n = 1, download = TRUE,
    out_dir = tmp, include_taxon_name = TRUE
  )

  audio_files <- list.files(file.path(tmp, "audio"))
  expect_true(any(grepl("^Turnix_maculosus_XC301\\.mp3$", audio_files)))
})

# ---------------------------------------------------------------------------
# 14. Metadata CSV is created with correct columns
# ---------------------------------------------------------------------------

test_that("get_xc_sounds creates metadata CSV with correct columns", {
  recs  <- list(make_xc_recording(id = "400"))
  page1 <- make_xc_page(recordings = recs, num_pages = 1)
  tmp   <- withr::local_tempdir()

  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    GET              = function(...) fake_resp,
    stop_for_status  = function(...) invisible(NULL),
    content          = function(resp, as = "parsed", ...) {
      if (identical(as, "raw")) as.raw(0L) else page1
    },
    .package = "EcoacousticUtilities"
  )

  get_xc_sounds(
    "Turnix maculosus", api_key = TEST_KEY,
    target_n = 1, download = TRUE, out_dir = tmp
  )

  csv_path <- file.path(tmp, "metadata.csv")
  expect_true(file.exists(csv_path))

  meta <- read.csv(csv_path)
  expected_cols <- c(
    "xc_id", "taxon_name", "recordist", "country",
    "locality", "latitude", "longitude", "date",
    "sound_type", "quality", "length", "file_url",
    "file_path", "license_code"
  )
  for (col in expected_cols) {
    expect_true(col %in% names(meta), info = paste("missing column:", col))
  }
})

# ---------------------------------------------------------------------------
# 15. Recordings with missing / empty file URL are skipped gracefully
# ---------------------------------------------------------------------------

test_that("get_xc_sounds skips recordings with no file URL", {
  bad_rec  <- make_xc_recording(id = "500", file = "")
  good_rec <- make_xc_recording(id = "501")
  page1    <- make_xc_page(recordings = list(bad_rec, good_rec), num_pages = 1)
  tmp      <- withr::local_tempdir()

  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    GET              = function(...) fake_resp,
    stop_for_status  = function(...) invisible(NULL),
    content          = function(resp, as = "parsed", ...) {
      if (identical(as, "raw")) as.raw(0L) else page1
    },
    .package = "EcoacousticUtilities"
  )

  expect_no_error(
    get_xc_sounds(
      "Turnix maculosus", api_key = TEST_KEY,
      target_n = 5, download = TRUE, out_dir = tmp
    )
  )

  audio_files <- list.files(file.path(tmp, "audio"))
  expect_false(any(grepl("XC500", audio_files)))
  expect_true(any(grepl("XC501", audio_files)))
})
