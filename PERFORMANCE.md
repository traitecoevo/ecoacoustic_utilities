# Performance Optimization Guide for `training_dataset_summary()`

## Overview

The `training_dataset_summary()` function has been optimized for large audio datasets with several performance-enhancing features.

## Performance Options

### 1. Parallel Processing (Default: ON)

Uses multiple CPU cores to extract audio metadata simultaneously.

```r
# Use parallel processing with automatic core detection (default)
summary <- training_dataset_summary("/path/to/dataset")

# Specify number of cores
summary <- training_dataset_summary(
  "/path/to/dataset",
  parallel = TRUE,
  n_cores = 4
)

# Disable parallel processing
summary <- training_dataset_summary(
  "/path/to/dataset",
  parallel = FALSE
)
```

**Performance gain**: ~2-4x faster on multi-core systems (depends on number of cores)

### 2. Sampling

Process only a random subset of files for duration analysis.

```r
# Analyze only 1000 random files instead of all files
summary <- training_dataset_summary(
  "/path/to/large/dataset",
  sample_size = 1000
)
```

**Performance gain**: Proportional to sampling ratio (e.g., 1000 files from 10,000 = 10x faster)

**Use case**: Quick exploratory analysis of very large datasets

### 3. Skip Duration Extraction

File size analysis is fast; duration extraction is slow. Skip it if not needed.

```r
# Only analyze file sizes, skip audio duration extraction
summary <- training_dataset_summary(
  "/path/to/dataset",
  use_tuneR = FALSE
)
```

**Performance gain**: ~100-1000x faster (no audio file reading)

**Use case**: When you only need class distribution and file size statistics

### 4. Progress Bar (Default: ON)

Shows progress during sequential processing.

```r
# Disable progress bar for cleaner output
summary <- training_dataset_summary(
  "/path/to/dataset",
  show_progress = FALSE
)
```

## Recommended Strategies

### For Small Datasets (< 1,000 files)
```r
# Use defaults - full analysis with parallel processing
summary <- training_dataset_summary("/path/to/dataset")
```

### For Medium Datasets (1,000 - 10,000 files)
```r
# Full analysis with parallel processing
summary <- training_dataset_summary(
  "/path/to/dataset",
  parallel = TRUE,
  n_cores = 4  # Adjust based on your CPU
)
```

### For Large Datasets (10,000 - 100,000 files)
```r
# Use sampling for quick analysis
summary <- training_dataset_summary(
  "/path/to/dataset",
  sample_size = 5000,  # Sample 5000 files
  parallel = TRUE,
  n_cores = 8
)
```

### For Very Large Datasets (> 100,000 files)
```r
# Option 1: Skip duration extraction entirely
summary <- training_dataset_summary(
  "/path/to/dataset",
  use_tuneR = FALSE  # Very fast, file sizes only
)

# Option 2: Aggressive sampling
summary <- training_dataset_summary(
  "/path/to/dataset",
  sample_size = 1000,  # Small sample
  parallel = TRUE,
  n_cores = 8
)
```

## Performance Comparison

Approximate processing times for a dataset with 10,000 WAV files:

| Configuration | Estimated Time |
|---------------|----------------|
| Default (parallel, all files) | 5-10 minutes |
| Parallel OFF, all files | 15-30 minutes |
| Sample 1000 files, parallel | 30-60 seconds |
| use_tuneR = FALSE | 1-5 seconds |

*Times vary based on file sizes, CPU, and disk speed*

## Technical Details

### Parallel Processing
- Uses R's `parallel` package
- Creates a cluster of worker processes
- Each worker processes files independently
- Automatically uses half of available CPU cores by default
- Minimum 10 files required to activate parallel mode

### Sampling
- Random sampling without replacement
- Ensures representative distribution across classes
- Duration statistics based on sampled files only
- File size and class statistics always use all files

### Progress Reporting
- Text progress bar during sequential processing
- Disabled automatically during parallel processing
- Shows current file number and percentage complete
