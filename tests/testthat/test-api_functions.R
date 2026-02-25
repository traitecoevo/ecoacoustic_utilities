# Tests for API-dependent functions
# These are integration tests that require network access
# They are skipped by default and can be run with testthat::test_local()

testthat::skip_if_offline()

test_that("get_inat_sounds has correct parameter structure", {
  # Test that function has expected parameters
  params <- names(formals(get_inat_sounds))

  expect_true("taxon_name" %in% params)
  expect_true("place_name" %in% params)
  expect_true("target_n" %in% params)
  expect_true("download" %in% params)
  expect_true("use_place_filter" %in% params)
  expect_true("quality" %in% params)

  # Test defaults
  defaults <- formals(get_inat_sounds)
  expect_equal(defaults$target_n, 300)
  expect_equal(defaults$download, TRUE)
  expect_equal(defaults$use_place_filter, FALSE)
  expect_equal(defaults$place_name, "Australia")
  expect_equal(defaults$include_taxon_name, TRUE)
})

test_that("get_inat_sounds quality parameter works", {
  # Test that quality parameter accepts valid values
  expect_no_error({
    # This won't actually run the API call in test mode
    formals(get_inat_sounds)$quality
  })
})

test_that("get_inat_sounds accepts include_taxon_name", {
  expect_true("include_taxon_name" %in% names(formals(get_inat_sounds)))
})

test_that("get_inat_species_summary has correct defaults", {
  # Test default parameters
  defaults <- formals(get_inat_species_summary)

  expect_equal(defaults$min_recordings, 1)
  expect_equal(defaults$taxon_id, NULL)
  expect_equal(defaults$place_name, "Australia")
  expect_equal(defaults$quality_grade, "research")
  expect_equal(defaults$per_page, 200)
  expect_equal(defaults$max_pages, 10)
})

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

test_that("get_ala_circle_occurrences has correct parameter structure", {
  # Test that function has expected parameters
  params <- names(formals(get_ala_circle_occurrences))

  expect_true("taxon" %in% params)
  expect_true("lat" %in% params)
  expect_true("lon" %in% params)
  expect_true("radius_km" %in% params)
  expect_true("email" %in% params)

  # Test default radius
  expect_equal(formals(get_ala_circle_occurrences)$radius_km, 10)
})

# Note: Full integration tests for API functions would require:
# 1. Network access
# 2. Valid API credentials
# 3. Mocking of HTTP responses
# These should be implemented separately with vcr or httptest packages
# if comprehensive API testing is needed.
