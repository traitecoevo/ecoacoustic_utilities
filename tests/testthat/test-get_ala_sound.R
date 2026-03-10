library(testthat)
library(galah)


test_that("get_ala_sounds has correct parameter structure", {
  # Test that function has expected parameters
  params <- names(formals(get_ala_sounds))

  expect_true("taxon_name" %in% params)
  expect_true("target_n" %in% params)
  expect_true("download" %in% params)
  expect_true("out_dir" %in% params)
  expect_true("include_taxon_name" %in% params)
  expect_true("supplier" %in% params)
  expect_true("as_wav" %in% params)

  # Test defaults
  defaults <- formals(get_ala_sounds)
  expect_equal(defaults$target_n, 50)
  expect_equal(defaults$download, TRUE)
  expect_equal(defaults$out_dir, "sounds")
  expect_equal(defaults$include_taxon_name, TRUE)
  expect_equal(defaults$as_wav, FALSE)
})

test_that("get_ala_sounds supplier parameter has valid options", {
  defaults <- formals(get_ala_sounds)
  supplier_options <- eval(defaults$supplier)
  expect_true("all" %in% supplier_options)
  expect_true("CSIRO" %in% supplier_options)
  expect_equal(supplier_options[1], "all") # "all" is the default
})

# --- Logic Tests with Mocks ---

# Mock data creators
mock_occ_data <- function(n = 65) {
  data.frame(
    recordID = paste0("rec_", 1:n),
    scientificName = "Artamus superciliosus",
    stringsAsFactors = FALSE
  )
}

mock_media_data <- function(occ) {
  data.frame(
    recordID = occ$recordID,
    media_url = paste0("http://example.com/", occ$recordID, ".mp3"),
    scientificName = "Artamus superciliosus",
    format = "audio/mpeg",
    stringsAsFactors = FALSE
  )
}

test_that("get_ala_sounds handles batch success, failures, and early stopping", {
  # Mocking inside the test using local_mocked_bindings (testthat 3e)
  # This correctly intercepts calls WITHIN the package functions
  
  # We need to mock the exported functions that get_ala_sounds calls
  test_occ_data_60 <- mock_occ_data(60)
  test_occ_data_120 <- mock_occ_data(120)
  test_occ_data_150 <- mock_occ_data(150)

  # Case 1: Pure success with early stopping
  # target_n = 10, buffer = 10*1.5 + 20 = 35. 1 chunk of 50.
  local_mocked_bindings(
    galah_call = function(...) list(),
    galah_identify = function(x, ...) x,
    galah_filter = function(x, multimedia, ...) x,
    slice_head = function(x, n, ...) x,
    is_api_reachable = function(...) TRUE,
    atlas_occurrences = function(...) test_occ_data_60,
    atlas_media = mock_media_data,
    .package = "EcoacousticUtilities"
  )
  
  messages <- capture.output({
    res <- get_ala_sounds("Test Taxon", target_n = 10, download = FALSE)
  }, type = "message")
  
  expect_equal(res, 10) # Limited to target_n
  expect_true(any(grepl("Processing in 2 chunks", messages))) # 60 records / 50 = 2 chunks
  
  # Case 2: Failure in first chunk, fallback to isolated
  local_mocked_bindings(
    galah_call = function(...) list(),
    galah_identify = function(x, ...) x,
    galah_filter = function(x, multimedia, ...) x,
    slice_head = function(x, n, ...) x,
    is_api_reachable = function(...) TRUE,
    atlas_occurrences = function(...) test_occ_data_120,
    atlas_media = function(occ) {
      if (any(grepl("rec_1$", occ$recordID))) stop("Simulated recycling error")
      mock_media_data(occ)
    },
    .package = "EcoacousticUtilities"
  )
  
  messages <- capture.output({
    res <- get_ala_sounds("Test Taxon", target_n = 30, download = FALSE)
  }, type = "message")
  
  expect_equal(res, 30)
  expect_true(any(grepl("Batch fetch failed for chunk 1", messages)))
  expect_true(any(grepl("Processing in 3 chunks", messages))) # 120 records / 50 = 3 chunks
  
  # Case 3: Early stopping across chunks
  # target_n = 60. buffer = 60*1.5 + 20 = 110. 3 chunks of 50.
  # verified_audio_count >= 60 * 1.2 = 72.
  # First chunk (50) + Second chunk (50) = 100 > 72. Should stop after 2nd chunk.
  local_mocked_bindings(
    galah_call = function(...) list(),
    galah_identify = function(x, ...) x,
    galah_filter = function(x, multimedia, ...) x,
    slice_head = function(x, n, ...) x,
    is_api_reachable = function(...) TRUE,
    atlas_occurrences = function(...) test_occ_data_150,
    atlas_media = mock_media_data,
    .package = "EcoacousticUtilities"
  )
  
  messages <- capture.output({
    res <- get_ala_sounds("Test Taxon", target_n = 60, download = FALSE)
  }, type = "message")
  
  expect_equal(res, 60)
  expect_true(any(grepl("Processing in 3 chunks", messages)))
  expect_true(any(grepl("Reached target buffer. Stopping early", messages)))
})
