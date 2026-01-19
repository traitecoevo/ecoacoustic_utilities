test_that("biggest_files returns correct structure", {
  # Create a temporary directory with test files
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_biggest_files")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create test files of different sizes
  file1 <- file.path(test_dir, "small.txt")
  file2 <- file.path(test_dir, "medium.txt")
  file3 <- file.path(test_dir, "large.txt")
  
  writeLines(rep("x", 10), file1)
  writeLines(rep("x", 100), file2)
  writeLines(rep("x", 1000), file3)
  
  # Test basic functionality
  result <- biggest_files(test_dir, n = 3)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_true(all(c("size_human", "size", "mtime", "path") %in% names(result)))
  
  # Check that files are sorted by size (largest first)
  expect_true(result$size[1] >= result$size[2])
  expect_true(result$size[2] >= result$size[3])
  
  # Clean up
  unlink(test_dir, recursive = TRUE)
})

test_that("biggest_files respects n parameter", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_n_param")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create 5 files
  for (i in 1:5) {
    writeLines(rep("x", i * 10), file.path(test_dir, paste0("file", i, ".txt")))
  }
  
  result <- biggest_files(test_dir, n = 2)
  expect_equal(nrow(result), 2)
  
  unlink(test_dir, recursive = TRUE)
})

test_that("biggest_files handles pattern filtering", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_pattern")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create different file types
  writeLines(rep("x", 100), file.path(test_dir, "file1.txt"))
  writeLines(rep("x", 200), file.path(test_dir, "file2.csv"))
  writeLines(rep("x", 300), file.path(test_dir, "file3.txt"))
  
  # Filter for .txt files only
  result <- biggest_files(test_dir, n = 10, pattern = "\\.txt$")
  
  expect_true(all(grepl("\\.txt$", result$path)))
  expect_equal(nrow(result), 2)
  
  unlink(test_dir, recursive = TRUE)
})

test_that("biggest_files returns empty data.frame for empty directory", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_empty")
  dir.create(test_dir, showWarnings = FALSE)
  
  result <- biggest_files(test_dir, n = 10)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_true(all(c("size_human", "size", "mtime", "path") %in% names(result)))
  
  unlink(test_dir, recursive = TRUE)
})

test_that("biggest_files errors on non-existent directory", {
  expect_error(
    biggest_files("/this/path/does/not/exist"),
    "dir.exists\\(root_dir\\) is not TRUE"
  )
})

test_that("biggest_files handles recursive search", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_recursive")
  sub_dir <- file.path(test_dir, "subdir")
  dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create files in both directories
  writeLines(rep("x", 100), file.path(test_dir, "root.txt"))
  writeLines(rep("x", 200), file.path(sub_dir, "sub.txt"))
  
  result <- biggest_files(test_dir, n = 10)
  
  # Should find both files
  expect_equal(nrow(result), 2)
  
  unlink(test_dir, recursive = TRUE)
})
