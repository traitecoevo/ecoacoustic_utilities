test_that("find_duplicate_wavs finds duplicates correctly", {
  skip_if_not_installed("digest")
  
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_dups")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create two identical WAV files (same content)
  wav_content <- as.raw(c(1:100))
  file1 <- file.path(test_dir, "sound1.wav")
  file2 <- file.path(test_dir, "sound2.wav")
  file3 <- file.path(test_dir, "sound3.wav")
  
  writeBin(wav_content, file1)
  writeBin(wav_content, file2)  # Duplicate of file1
  writeBin(as.raw(c(1:50)), file3)  # Different content
  
  result <- find_duplicate_wavs(test_dir)
  
  # Should find one set of duplicates (file1 and file2)
  expect_type(result, "character")
  expect_equal(length(result), 1)
  expect_true(names(result)[1] != "")  # Should have hash as name
  
  unlink(test_dir, recursive = TRUE)
})

test_that("find_duplicate_wavs handles no duplicates", {
  skip_if_not_installed("digest")
  
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_no_dups")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create unique WAV files
  writeBin(as.raw(c(1:100)), file.path(test_dir, "sound1.wav"))
  writeBin(as.raw(c(1:50)), file.path(test_dir, "sound2.wav"))
  
  result <- find_duplicate_wavs(test_dir)
  
  expect_type(result, "character")
  expect_equal(length(result), 0)
  
  unlink(test_dir, recursive = TRUE)
})

test_that("find_duplicate_wavs handles subdirectories", {
  skip_if_not_installed("digest")
  
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_subdirs")
  sub_dir <- file.path(test_dir, "subdir")
  dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create duplicate files in different directories
  wav_content <- as.raw(c(1:100))
  writeBin(wav_content, file.path(test_dir, "sound1.wav"))
  writeBin(wav_content, file.path(sub_dir, "sound2.wav"))
  
  result <- find_duplicate_wavs(test_dir)
  
  expect_equal(length(result), 1)
  
  unlink(test_dir, recursive = TRUE)
})

test_that("find_duplicate_wavs is case-insensitive for extensions", {
  skip_if_not_installed("digest")
  
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_case")
  dir.create(test_dir, showWarnings = FALSE)
  
  wav_content <- as.raw(c(1:100))
  writeBin(wav_content, file.path(test_dir, "sound1.WAV"))
  writeBin(wav_content, file.path(test_dir, "sound2.Wav"))
  
  result <- find_duplicate_wavs(test_dir)
  
  expect_equal(length(result), 1)
  
  unlink(test_dir, recursive = TRUE)
})

test_that("find_duplicate_wavs returns NULL for no WAV files", {
  skip_if_not_installed("digest")
  
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_no_wavs")
  dir.create(test_dir, showWarnings = FALSE)
  
  # Create non-WAV files
  writeLines("test", file.path(test_dir, "file.txt"))
  
  result <- find_duplicate_wavs(test_dir)
  
  expect_null(result)
  
  unlink(test_dir, recursive = TRUE)
})

test_that("find_duplicate_wavs errors on non-existent directory", {
  expect_error(
    find_duplicate_wavs("/this/path/does/not/exist"),
    "Directory does not exist"
  )
})
