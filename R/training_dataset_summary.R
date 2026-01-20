#' Summarize Audio Training Dataset
#'
#' Analyzes a directory structure containing audio files (typically organized by
#' class in subdirectories) and provides comprehensive statistics including class
#' distribution, file types, duration statistics, file sizes, and outlier detection.
#'
#' @param root_dir Character. Path to the root directory containing audio files.
#'   Typically organized with subdirectories representing different classes.
#' @param audio_extensions Character vector. File extensions to consider as audio
#'   files. Default includes common formats: wav, mp3, flac, m4a, ogg, aiff.
#' @param outlier_threshold Numeric. Number of standard deviations from the mean
#'   to flag as outliers. Default is 3.
#' @param use_tuneR Logical. If TRUE and tuneR package is available, extract
#'   detailed audio metadata (duration, sample rate, etc.). Default is TRUE.
#' @param ignore_classes Character vector. Names of class subdirectories to ignore
#'   (e.g. "noise"). Default is NULL.
#' @param sample_size Integer. If not NULL, randomly sample this many files for
#'   duration analysis instead of processing all files. Useful for large datasets.
#'   Default is NULL (process all files).
#' @param parallel Logical. If TRUE, use parallel processing for audio metadata
#'   extraction (requires parallel package). Default is TRUE.
#' @param n_cores Integer. Number of cores to use for parallel processing. If NULL,
#'   uses half of available cores. Default is NULL.
#' @param show_progress Logical. If TRUE, show progress bar during audio metadata
#'   extraction. Default is TRUE.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{summary}{Data.frame with overall statistics}
#'     \item{class_distribution}{Data.frame with per-class file counts}
#'     \item{file_type_distribution}{Data.frame with counts by file extension}
#'     \item{duration_stats}{Data.frame with duration statistics (if available)}
#'     \item{size_stats}{Data.frame with file size statistics}
#'     \item{outliers}{Data.frame with flagged unusual files}
#'     \item{imbalance_stats}{Data.frame with class imbalance metrics}
#'     \item{recommendations}{Character vector of transfer learning advice}
#'   }
#'
#' @details
#' The function recursively scans the directory structure and:
#' \itemize{
#'   \item Counts files per subdirectory (classes)
#'   \item Identifies file types by extension
#'   \item Calculates file size distributions
#'   \item Extracts audio duration (if tuneR is available)
#'   \item Flags outliers based on size and duration
#' }
#'
#' Outliers are identified as files that deviate from the mean by more than
#' \code{outlier_threshold} standard deviations in either size or duration.
#'
#' @examples
#' \dontrun{
#' # Analyze a training dataset
#' summary <- training_dataset_summary("/path/to/audio/dataset")
#' 
#' # Ignore noise class
#' summary <- training_dataset_summary(
#'   "/path/to/dataset",
#'   ignore_classes = c("noise", "background")
#' )
#' 
#' # Print summary statistics
#' print(summary$summary)
#' print(summary$class_distribution)
#' 
#' # Check for outliers
#' if (nrow(summary$outliers) > 0) {
#'   print(summary$outliers)
#' }
#' 
#' # For large datasets, use sampling for faster analysis
#' summary <- training_dataset_summary(
#'   "/path/to/large/dataset",
#'   sample_size = 1000,  # Only analyze 1000 random files
#'   parallel = TRUE,     # Use parallel processing
#'   n_cores = 4          # Use 4 CPU cores
#' )
#' 
#' # Skip duration extraction for even faster analysis
#' summary <- training_dataset_summary(
#'   "/path/to/dataset",
#'   use_tuneR = FALSE  # Only file sizes, no duration data
#' )
#' }
#'
#' @export
#' @importFrom utils head tail
#' @importFrom stats median sd quantile
training_dataset_summary <- function(
    root_dir,
    audio_extensions = c("wav", "mp3", "flac", "m4a", "ogg", "aiff", "aif"),
    outlier_threshold = 3,
    use_tuneR = TRUE,
    ignore_classes = NULL,
    sample_size = NULL,
    parallel = TRUE,
    n_cores = NULL,
    show_progress = TRUE
) {
  
  # Validate inputs
  if (!dir.exists(root_dir)) {
    stop("Directory does not exist: ", root_dir)
  }
  
  # Check if tuneR is available
  has_tuneR <- use_tuneR && requireNamespace("tuneR", quietly = TRUE)
  
  # Build pattern for audio files
  pattern <- paste0("\\.(", paste(audio_extensions, collapse = "|"), ")$")
  
  # Find all audio files recursively
  audio_files <- list.files(
    path = root_dir,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(audio_files) == 0) {
    warning("No audio files found in ", root_dir)
    return(list(
      summary = data.frame(total_files = 0, total_classes = 0),
      class_distribution = data.frame(),
      file_type_distribution = data.frame(),
      duration_stats = data.frame(),
      size_stats = data.frame(),
      outliers = data.frame(),
      imbalance_stats = data.frame(),
      recommendations = character()
    ))
  }
  
  # Get file info
  file_info <- file.info(audio_files)
  file_info$path <- rownames(file_info)
  file_info$relative_path <- gsub(paste0("^", root_dir, "/?"), "", file_info$path)
  
  # Extract class (immediate subdirectory)
  file_info$class <- dirname(file_info$relative_path)
  file_info$class[file_info$class == "."] <- "root"
  
  # Filter out ignored classes
  if (!is.null(ignore_classes)) {
    ignored_indices <- file_info$class %in% ignore_classes
    n_ignored <- sum(ignored_indices)
    
    if (n_ignored > 0) {
      if (show_progress) {
        message("Ignoring ", n_ignored, " files from classes: ", 
                paste(intersect(unique(file_info$class), ignore_classes), collapse = ", "))
      }
      file_info <- file_info[!ignored_indices, ]
      audio_files <- file_info$path # Update audio_files vector too
      
      if (nrow(file_info) == 0) {
        warning("All files were filtered out by ignore_classes.")
        return(list(
          summary = data.frame(total_files = 0, total_classes = 0),
          class_distribution = data.frame(),
          file_type_distribution = data.frame(),
          duration_stats = data.frame(),
          size_stats = data.frame(),
          outliers = data.frame(),
          imbalance_stats = data.frame(),
          recommendations = character()
        ))
      }
    }
  }
  
  # Extract file extension
  file_info$extension <- tolower(sub(".*\\.", "", file_info$path))
  
  # Extract filename
  file_info$filename <- basename(file_info$path)
  
  # Initialize duration column
  file_info$duration_sec <- NA_real_
  
  # Extract audio metadata if tuneR is available
  if (has_tuneR) {
    # Determine which files to process
    files_to_process <- audio_files
    indices_to_process <- seq_along(audio_files)
    
    if (!is.null(sample_size) && sample_size < length(audio_files)) {
      message("Sampling ", sample_size, " files from ", length(audio_files), " total files...")
      indices_to_process <- sample(seq_along(audio_files), sample_size)
      files_to_process <- audio_files[indices_to_process]
    }
    
    message("Extracting audio metadata from ", length(files_to_process), " files...")
    
    # Function to extract duration from a single file
    extract_duration <- function(filepath) {
      tryCatch({
        audio <- tuneR::readWave(filepath)
        length(audio@left) / audio@samp.rate
      }, error = function(e) {
        NA_real_
      })
    }
    
    # Use parallel processing if requested and available
    use_parallel <- parallel && requireNamespace("parallel", quietly = TRUE) && 
                    length(files_to_process) > 10
    
    if (use_parallel) {
      # Determine number of cores
      if (is.null(n_cores)) {
        n_cores <- max(1, floor(parallel::detectCores() / 2))
      }
      n_cores <- min(n_cores, parallel::detectCores())
      
      message("Using parallel processing with ", n_cores, " cores...")
      
      # Try to use pbapply for progress bar with parallel processing
      if (show_progress && requireNamespace("pbapply", quietly = TRUE)) {
        message("Processing files (progress bar enabled)...")
        cl <- parallel::makeCluster(n_cores)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        
        # Export necessary objects
        parallel::clusterExport(cl, c("extract_duration"), envir = environment())
        
        # Use pbapply for progress bar
        durations <- pbapply::pblapply(
          files_to_process, 
          extract_duration,
          cl = cl
        )
        durations <- unlist(durations)
        
      } else {
        # Fallback to regular parallel without progress bar
        if (show_progress) {
          message("Processing files (install 'pbapply' package for progress bar)...")
        }
        
        cl <- parallel::makeCluster(n_cores)
        on.exit(parallel::stopCluster(cl), add = TRUE)
        
        # Export necessary objects
        parallel::clusterExport(cl, c("extract_duration"), envir = environment())
        
        # Process in parallel without progress
        durations <- parallel::parLapply(cl, files_to_process, extract_duration)
        durations <- unlist(durations)
      }
      
    } else {
      # Sequential processing with progress bar
      if (show_progress && requireNamespace("utils", quietly = TRUE)) {
        pb <- utils::txtProgressBar(min = 0, max = length(files_to_process), style = 3)
        durations <- vapply(seq_along(files_to_process), function(i) {
          utils::setTxtProgressBar(pb, i)
          extract_duration(files_to_process[i])
        }, numeric(1))
        close(pb)
      } else {
        durations <- vapply(files_to_process, extract_duration, numeric(1))
      }
    }
    
    # Assign durations to the correct rows
    file_info$duration_sec[indices_to_process] <- durations
  }
  
  # Class distribution
  class_dist <- as.data.frame(table(file_info$class))
  names(class_dist) <- c("class", "n_files")
  class_dist <- class_dist[order(-class_dist$n_files), ]
  
  # File type distribution
  type_dist <- as.data.frame(table(file_info$extension))
  names(type_dist) <- c("extension", "n_files")
  type_dist <- type_dist[order(-type_dist$n_files), ]
  
  # Size statistics
  size_stats <- data.frame(
    metric = c("mean_mb", "median_mb", "min_mb", "max_mb", "sd_mb", "total_mb"),
    value = c(
      mean(file_info$size) / 1024^2,
      median(file_info$size) / 1024^2,
      min(file_info$size) / 1024^2,
      max(file_info$size) / 1024^2,
      sd(file_info$size) / 1024^2,
      sum(file_info$size) / 1024^2
    )
  )
  
  # Duration statistics (if available)
  duration_stats <- data.frame()
  if (has_tuneR && any(!is.na(file_info$duration_sec))) {
    valid_durations <- file_info$duration_sec[!is.na(file_info$duration_sec)]
    duration_stats <- data.frame(
      metric = c("mean_sec", "median_sec", "min_sec", "max_sec", "sd_sec", "total_hours"),
      value = c(
        mean(valid_durations),
        median(valid_durations),
        min(valid_durations),
        max(valid_durations),
        sd(valid_durations),
        sum(valid_durations) / 3600
      )
    )
  }
  
  # Detect outliers
  outliers <- data.frame()
  
  # Size outliers
  size_mean <- mean(file_info$size)
  size_sd <- sd(file_info$size)
  size_threshold_low <- size_mean - outlier_threshold * size_sd
  size_threshold_high <- size_mean + outlier_threshold * size_sd
  
  size_outliers <- file_info[
    file_info$size < size_threshold_low | file_info$size > size_threshold_high,
    c("filename", "class", "size", "extension", "relative_path")
  ]
  
  if (nrow(size_outliers) > 0) {
    size_outliers$reason <- ifelse(
      size_outliers$size < size_threshold_low,
      "unusually_small",
      "unusually_large"
    )
    size_outliers$size_mb <- size_outliers$size / 1024^2
    outliers <- size_outliers
  }
  
  # Duration outliers (if available)
  if (has_tuneR && any(!is.na(file_info$duration_sec))) {
    valid_dur <- file_info[!is.na(file_info$duration_sec), ]
    dur_mean <- mean(valid_dur$duration_sec)
    dur_sd <- sd(valid_dur$duration_sec)
    dur_threshold_low <- dur_mean - outlier_threshold * dur_sd
    dur_threshold_high <- dur_mean + outlier_threshold * dur_sd
    
    dur_outliers <- valid_dur[
      valid_dur$duration_sec < dur_threshold_low | valid_dur$duration_sec > dur_threshold_high,
      c("filename", "class", "duration_sec", "extension", "relative_path")
    ]
    
    if (nrow(dur_outliers) > 0) {
      dur_outliers$reason <- ifelse(
        dur_outliers$duration_sec < dur_threshold_low,
        "unusually_short",
        "unusually_long"
      )
      
      # Merge with size outliers
      if (nrow(outliers) > 0) {
        outliers <- merge(
          outliers,
          dur_outliers[, c("filename", "duration_sec", "reason")],
          by = "filename",
          all = TRUE,
          suffixes = c("_size", "_duration")
        )
      } else {
        outliers <- dur_outliers
      }
    }
  }
  
  # Overall summary
  summary_df <- data.frame(
    total_files = nrow(file_info),
    total_classes = length(unique(file_info$class)),
    total_size_mb = sum(file_info$size) / 1024^2,
    n_file_types = length(unique(file_info$extension)),
    n_outliers = nrow(outliers),
    has_duration_data = has_tuneR && any(!is.na(file_info$duration_sec))
  )
  
  if (has_tuneR && any(!is.na(file_info$duration_sec))) {
    summary_df$total_duration_hours <- sum(file_info$duration_sec, na.rm = TRUE) / 3600
    summary_df$files_with_duration <- sum(!is.na(file_info$duration_sec))
    # BirdNET uses 3-second segments. Estimate total segments.
    summary_df$est_3s_segments <- floor(sum(file_info$duration_sec, na.rm = TRUE) / 3)
  }
  
  # Imbalance statistics
  n_files <- class_dist$n_files
  imbalance_stats <- data.frame(
    min_files = min(n_files),
    max_files = max(n_files),
    median_files = median(n_files),
    mean_files = mean(n_files),
    imbalance_ratio = max(n_files) / min(n_files),
    n_small_classes_lt_50 = sum(n_files < 50),
    n_small_classes_lt_100 = sum(n_files < 100)
  )
  
  # Heuristic Recommendations
  recommendations <- character()
  
  if (imbalance_stats$imbalance_ratio > 10) {
    recommendations <- c(recommendations, 
      "High class imbalance detected (ratio > 10). Consider using --focal-loss with --focal-loss-gamma 2.0 or higher.")
  } else if (imbalance_stats$imbalance_ratio > 5) {
     recommendations <- c(recommendations, 
      "Moderate class imbalance detected. --focal-loss may be beneficial.")
  }
  
  if (imbalance_stats$n_small_classes_lt_50 > 0) {
    recommendations <- c(recommendations,
      paste0(imbalance_stats$n_small_classes_lt_50, " classes have < 50 samples. Consider using --upsampling_ratio (e.g., 0.5 - 1.0) to balance training."))
  }
  
  if (summary_df$total_files < 1000) {
    recommendations <- c(recommendations,
      "Small dataset (< 1000 files). Use a lower learning rate (e.g., 0.0001) and fewer epochs to prevent overfitting.")
  }
  
  if (nrow(outliers) > 0) {
    recommendations <- c(recommendations,
      paste0(nrow(outliers), " outliers detected (unusual size/duration). Review 'outliers' to ensure data quality."))
  }

  # Return comprehensive summary
  result <- list(
    summary = summary_df,
    imbalance_stats = imbalance_stats,
    class_distribution = class_dist,
    file_type_distribution = type_dist,
    size_stats = size_stats,
    duration_stats = duration_stats,
    outliers = outliers,
    recommendations = recommendations
  )
  
  class(result) <- c("training_dataset_summary", "list")
  return(result)
}

#' Print method for training_dataset_summary
#'
#' @param x A training_dataset_summary object
#' @param ... Additional arguments (ignored)
#' @export
print.training_dataset_summary <- function(x, ...) {
  cat("=== Training Dataset Summary ===\n\n")
  
  cat("Overall Statistics:\n")
  print(x$summary, row.names = FALSE)
  cat("\n")
  
  cat("Class Distribution (top 10):\n")
  print(head(x$class_distribution, 10), row.names = FALSE)
  cat("\n")
  
  cat("File Type Distribution:\n")
  print(x$file_type_distribution, row.names = FALSE)
  cat("\n")
  
  cat("File Size Statistics:\n")
  print(x$size_stats, row.names = FALSE)
  cat("\n")
  
  if (nrow(x$duration_stats) > 0) {
    cat("Duration Statistics:\n")
    print(x$duration_stats, row.names = FALSE)
    cat("\n")
  }
  
  if (nrow(x$outliers) > 0) {
    cat("Outliers Detected (", nrow(x$outliers), " files):\n", sep = "")
    print(head(x$outliers, 10), row.names = FALSE)
    if (nrow(x$outliers) > 10) {
      cat("... and", nrow(x$outliers) - 10, "more\n")
    }
  } else {
    cat("No outliers detected.\n")
  }
  
  cat("\nClass Imbalance Statistics:\n")
  print(x$imbalance_stats, row.names = FALSE)
  cat("\n")
  
  if (length(x$recommendations) > 0) {
    cat("=== Recommendations for Transfer Learning ===\n")
    cat(paste("-", x$recommendations, collapse = "\n"), "\n")
  }
  
  invisible(x)
}
