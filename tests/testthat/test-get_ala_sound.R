library(testthat)
library(galah)


test_that("get_ala_sounds has correct parameter structure", {
  params <- names(formals(get_ala_sounds))

  expect_true("taxon_name" %in% params)
  expect_true("target_n" %in% params)
  expect_true("download" %in% params)
  expect_true("out_dir" %in% params)
  expect_true("include_taxon_name" %in% params)
  expect_true("supplier" %in% params)
  expect_true("as_wav" %in% params)

  defaults <- formals(get_ala_sounds)
  expect_equal(defaults$target_n, 50)
  expect_equal(defaults$download, TRUE)
  expect_equal(defaults$out_dir, "sounds")
  expect_equal(defaults$include_taxon_name, TRUE)
  expect_equal(defaults$as_wav, TRUE)
})

test_that("get_ala_sounds supplier parameter has valid options", {
  defaults <- formals(get_ala_sounds)
  supplier_options <- eval(defaults$supplier)
  expect_true("all" %in% supplier_options)
  expect_true("CSIRO" %in% supplier_options)
  expect_equal(supplier_options[1], "all")
})

# --- Helpers ---

mock_audio_media <- function(n = 10) {
  data.frame(
    media_id         = paste0("media_", seq_len(n)),
    recordID         = paste0("rec_",   seq_len(n)),
    scientificName   = rep("Artamus superciliosus", n),
    mimetype         = rep("audio/mpeg", n),
    image_url        = paste0("http://example.com/", seq_len(n), ".mp3"),
    dataResourceName = rep("xeno-canto", n),
    decimalLatitude  = rep(-33.8, n),
    decimalLongitude = rep(151.2, n),
    eventDate        = rep("2023-01-01", n),
    license          = rep("CC-BY", n),
    stringsAsFactors = FALSE
  )
}

# --- Logic Tests with Mocks ---

test_that("get_ala_sounds limits results to target_n on bulk fetch success", {
  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    galah_call       = function(...) list(),
    galah_identify   = function(x, ...) x,
    galah_filter     = function(x, ...) x,
    atlas_media      = function(...) mock_audio_media(60),
    .package = "EcoacousticUtilities"
  )
  messages <- capture.output({
    res <- get_ala_sounds("Test Taxon", target_n = 10, download = FALSE)
  }, type = "message")

  expect_equal(res, 10)
  expect_true(any(grepl("Retrieved 60 records", messages)))
})

test_that("get_ala_sounds filters out non-audio mimetypes", {
  mixed_media <- data.frame(
    media_id         = paste0("media_", 1:5),
    recordID         = paste0("rec_",   1:5),
    scientificName   = rep("Artamus superciliosus", 5),
    mimetype         = c("audio/mpeg", "image/jpeg", "audio/mpeg", "image/jpeg", "audio/mpeg"),
    image_url        = paste0("http://example.com/", 1:5),
    dataResourceName = rep("ALA", 5),
    decimalLatitude  = rep(-33.8, 5),
    decimalLongitude = rep(151.2, 5),
    eventDate        = rep("2023-01-01", 5),
    license          = rep("CC-BY", 5),
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    galah_call       = function(...) list(),
    galah_identify   = function(x, ...) x,
    galah_filter     = function(x, ...) x,
    atlas_media      = function(...) mixed_media,
    .package = "EcoacousticUtilities"
  )
  messages <- capture.output({
    res <- get_ala_sounds("Test Taxon", target_n = 10, download = FALSE)
  }, type = "message")

  expect_equal(res, 3)
  expect_true(any(grepl("Dropped 2 non-audio", messages)))
})

test_that("get_ala_sounds falls back to year-by-year on bulk fetch error", {
  call_count <- 0L
  local_mocked_bindings(
    is_api_reachable = function(...) TRUE,
    galah_call       = function(...) list(),
    galah_identify   = function(x, ...) x,
    galah_filter     = function(x, ...) x,
    atlas_media      = function(...) {
      call_count <<- call_count + 1L
      if (call_count == 1L) stop("recycle error")
      mock_audio_media(5)
    },
    .package = "EcoacousticUtilities"
  )
  messages <- capture.output({
    res <- get_ala_sounds("Test Taxon", target_n = 5, download = FALSE)
  }, type = "message")

  expect_equal(res, 5)
  expect_true(any(grepl("year-by-year", messages, ignore.case = TRUE)))
  expect_true(call_count > 1L)
})
