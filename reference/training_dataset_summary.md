# Summarize Audio Training Dataset

Analyzes a directory structure containing audio files (typically
organized by class in subdirectories) and provides comprehensive
statistics including class distribution, file types, duration
statistics, file sizes, and outlier detection.

## Usage

``` r
training_dataset_summary(
  root_dir,
  audio_extensions = c("wav", "mp3", "flac", "m4a", "ogg", "aiff", "aif"),
  outlier_threshold = 3,
  use_tuneR = TRUE,
  ignore_classes = NULL,
  sample_size = NULL,
  parallel = TRUE,
  n_cores = NULL,
  show_progress = TRUE
)
```

## Arguments

- root_dir:

  Character. Path to the root directory containing audio files.
  Typically organized with subdirectories representing different
  classes.

- audio_extensions:

  Character vector. File extensions to consider as audio files. Default
  includes common formats: wav, mp3, flac, m4a, ogg, aiff.

- outlier_threshold:

  Numeric. Number of standard deviations from the mean to flag as
  outliers. Default is 3.

- use_tuneR:

  Logical. If TRUE and tuneR package is available, extract detailed
  audio metadata (duration, sample rate, etc.). Default is TRUE.

- ignore_classes:

  Character vector. Names of class subdirectories to ignore (e.g.
  "noise"). Default is NULL.

- sample_size:

  Integer. If not NULL, randomly sample this many files for duration
  analysis instead of processing all files. Useful for large datasets.
  Default is NULL (process all files).

- parallel:

  Logical. If TRUE, use parallel processing for audio metadata
  extraction (requires parallel package). Default is TRUE.

- n_cores:

  Integer. Number of cores to use for parallel processing. If NULL, uses
  half of available cores. Default is NULL.

- show_progress:

  Logical. If TRUE, show progress bar during audio metadata extraction.
  Default is TRUE.

## Value

A list with the following components:

- summary:

  Data.frame with overall statistics

- class_distribution:

  Data.frame with per-class file counts

- file_type_distribution:

  Data.frame with counts by file extension

- duration_stats:

  Data.frame with duration statistics (if available)

- size_stats:

  Data.frame with file size statistics

- outliers:

  Data.frame with flagged unusual files

- imbalance_stats:

  Data.frame with class imbalance metrics

- recommendations:

  Character vector of transfer learning advice

## Details

The function recursively scans the directory structure and:

- Counts files per subdirectory (classes)

- Identifies file types by extension

- Calculates file size distributions

- Extracts audio duration (if tuneR is available)

- Flags outliers based on size and duration

Outliers are identified as files that deviate from the mean by more than
`outlier_threshold` standard deviations in either size or duration.

## Examples

``` r
if (FALSE) { # \dontrun{
# Analyze a training dataset
summary <- training_dataset_summary("/path/to/audio/dataset")

# Ignore noise class
summary <- training_dataset_summary(
  "/path/to/dataset",
  ignore_classes = c("noise", "background")
)

# Print summary statistics
print(summary$summary)
print(summary$class_distribution)

# Check for outliers
if (nrow(summary$outliers) > 0) {
  print(summary$outliers)
}

# For large datasets, use sampling for faster analysis
summary <- training_dataset_summary(
  "/path/to/large/dataset",
  sample_size = 1000,  # Only analyze 1000 random files
  parallel = TRUE,     # Use parallel processing
  n_cores = 4          # Use 4 CPU cores
)

# Skip duration extraction for even faster analysis
summary <- training_dataset_summary(
  "/path/to/dataset",
  use_tuneR = FALSE  # Only file sizes, no duration data
)
} # }
```
