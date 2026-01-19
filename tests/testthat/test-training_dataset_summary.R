test_that("training_dataset_summary works with basic audio files", {
  # Create temporary directory structure
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_training_dataset")
  class1_dir <- file.path(test_dir, "class1")
  class2_dir <- file.path(test_dir, "class2")
  
  dir.create(class1_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(class2_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create mock audio files (just binary data)
  writeBin(as.raw(1:1000), file.path(class1_dir, "audio1.wav"))
  writeBin(as.raw(1:2000), file.path(class1_dir, "audio2.wav"))
  writeBin(as.raw(1:1500), file.path(class2_dir, "audio3.mp3"))
  writeBin(as.raw(1:1200), file.path(class2_dir, "audio4.wav"))
  
  # Run summary (without tuneR)
  result <- training_dataset_summary(test_dir, use_tuneR = FALSE)
  
  # Test structure
  expect_type(result, "list")
  expect_s3_class(result, "training_dataset_summary")
  expect_true(all(c("summary", "class_distribution", "file_type_distribution", 
                    "size_stats", "duration_stats", "outliers") %in% names(result)))
  
  # Test summary
  expect_equal(result$summary$total_files, 4)
  expect_equal(result$summary$total_classes, 2)
  expect_equal(result$summary$n_file_types, 2)
  
  # Test class distribution
  expect_equal(nrow(result$class_distribution), 2)
  expect_true(all(c("class", "n_files") %in% names(result$class_distribution)))
  
  # Test file type distribution
  expect_equal(nrow(result$file_type_distribution), 2)
  expect_true("wav" %in% result$file_type_distribution$extension)
  expect_true("mp3" %in% result$file_type_distribution$extension)
  
  # Clean up
  unlink(test_dir, recursive = TRUE)
})

test_that("training_dataset_summary handles empty directory", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_empty_dataset")
  dir.create(test_dir, showWarnings = FALSE)
  
  expect_warning(
    result <- training_dataset_summary(test_dir),
    "No audio files found"
  )
  
  expect_equal(result$summary$total_files, 0)
  expect_equal(result$summary$total_classes, 0)
  
  unlink(test_dir, recursive = TRUE)
})

test_that("training_dataset_summary detects outliers", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_outliers")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create files with normal sizes (around 1000 bytes)
  for (i in 1:10) {
    writeBin(rep(as.raw(1), 1000), file.path(test_dir, paste0("normal", i, ".wav")))
  }
  
  # Create an unusually large file (100x larger)
  writeBin(rep(as.raw(1), 100000), file.path(test_dir, "huge.wav"))
  
  result <- training_dataset_summary(test_dir, outlier_threshold = 2, use_tuneR = FALSE)
  
  # Should detect at least the huge outlier
  expect_true(nrow(result$outliers) > 0)
  expect_true(any(grepl("huge.wav", result$outliers$filename)))
  
  unlink(test_dir, recursive = TRUE)
})

test_that("training_dataset_summary handles custom extensions", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_custom_ext")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create files with different extensions
  writeBin(as.raw(1:1000), file.path(test_dir, "audio.wav"))
  writeBin(as.raw(1:1000), file.path(test_dir, "audio.flac"))
  writeBin(as.raw(1:1000), file.path(test_dir, "audio.txt"))  # Not audio
  
  result <- training_dataset_summary(
    test_dir, 
    audio_extensions = c("wav", "flac"),
    use_tuneR = FALSE
  )
  
  # Should only find wav and flac
  expect_equal(result$summary$total_files, 2)
  expect_false("txt" %in% result$file_type_distribution$extension)
  
  unlink(test_dir, recursive = TRUE)
})

test_that("training_dataset_summary errors on non-existent directory", {
  expect_error(
    training_dataset_summary("/this/path/does/not/exist"),
    "Directory does not exist"
  )
})

test_that("training_dataset_summary print method works", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_print")
  dir.create(test_dir, showWarnings = FALSE)
  
  writeBin(as.raw(1:1000), file.path(test_dir, "audio.wav"))
  
  result <- training_dataset_summary(test_dir, use_tuneR = FALSE)
  
  # Test that print doesn't error
  expect_output(print(result), "Training Dataset Summary")
  expect_output(print(result), "Overall Statistics")
  
  unlink(test_dir, recursive = TRUE)
})

test_that("training_dataset_summary handles nested subdirectories", {
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_nested")
  sub1 <- file.path(test_dir, "class1", "subclass1")
  sub2 <- file.path(test_dir, "class2")
  
  dir.create(sub1, recursive = TRUE, showWarnings = FALSE)
  dir.create(sub2, recursive = TRUE, showWarnings = FALSE)
  
  writeBin(as.raw(1:1000), file.path(sub1, "audio1.wav"))
  writeBin(as.raw(1:1000), file.path(sub2, "audio2.wav"))
  writeBin(as.raw(1:1000), file.path(test_dir, "audio3.wav"))
  
  result <- training_dataset_summary(test_dir, use_tuneR = FALSE)
  
  # Should find all files
  expect_equal(result$summary$total_files, 3)
  
  # Should have multiple classes
  expect_true(result$summary$total_classes >= 2)
  
  unlink(test_dir, recursive = TRUE)
})
