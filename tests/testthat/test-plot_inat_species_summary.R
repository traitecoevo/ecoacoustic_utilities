test_that("plot_inat_species_summary creates a ggplot object", {
  # Skip if ggplot2 not available
  skip_if_not_installed("ggplot2")
  
  # Create mock data
  test_data <- data.frame(
    taxon_id = 1:5,
    scientific_name = c("Species A", "Species B", "Species C", "Species D", "Species E"),
    common_name = c("Common A", "Common B", NA, "Common D", "Common E"),
    n_recordings = c(100, 80, 60, 40, 20),
    stringsAsFactors = FALSE
  )
  
  # Test basic plot creation
  p <- plot_inat_species_summary(test_data, top_n = 5)
  
  expect_s3_class(p, "ggplot")
})

test_that("plot_inat_species_summary handles common names correctly", {
  skip_if_not_installed("ggplot2")
  
  test_data <- data.frame(
    scientific_name = c("Sp A", "Sp B", "Sp C"),
    common_name = c("Common A", NA, "Common C"),
    n_recordings = c(100, 80, 60),
    stringsAsFactors = FALSE
  )
  
  # With common names
  p1 <- plot_inat_species_summary(test_data, use_common_names = TRUE)
  expect_s3_class(p1, "ggplot")
  
  # Without common names
  p2 <- plot_inat_species_summary(test_data, use_common_names = FALSE)
  expect_s3_class(p2, "ggplot")
})

test_that("plot_inat_species_summary respects top_n parameter", {
  skip_if_not_installed("ggplot2")
  
  test_data <- data.frame(
    scientific_name = paste("Species", 1:20),
    n_recordings = 100:81,
    stringsAsFactors = FALSE
  )
  
  p <- plot_inat_species_summary(test_data, top_n = 10)
  expect_s3_class(p, "ggplot")
})

test_that("plot_inat_species_summary validates input", {
  skip_if_not_installed("ggplot2")
  
  # Not a data.frame
  expect_error(
    plot_inat_species_summary(list(a = 1)),
    "must be a data.frame"
  )
  
  # Empty data.frame
  expect_error(
    plot_inat_species_summary(data.frame()),
    "is empty"
  )
  
  # Missing n_recordings column
  expect_error(
    plot_inat_species_summary(data.frame(name = "test")),
    "must contain 'n_recordings'"
  )
})

test_that("plot_inat_species_summary accepts custom parameters", {
  skip_if_not_installed("ggplot2")
  
  test_data <- data.frame(
    scientific_name = c("Sp A", "Sp B"),
    n_recordings = c(100, 80),
    stringsAsFactors = FALSE
  )
  
  # Custom color and title
  p <- plot_inat_species_summary(
    test_data,
    color = "#FF0000",
    title = "Custom Title"
  )
  
  expect_s3_class(p, "ggplot")
})
