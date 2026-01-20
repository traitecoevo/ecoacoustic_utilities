#' Find Duplicate WAV Files by Content Hash
#'
#' Searches a directory and its immediate subdirectories for duplicate WAV files
#' based on MD5 hash of file contents. Returns one representative file path for
#' each set of duplicates found.
#'
#' @param dir Character. Path to the directory to search. Must exist.
#'
#' @return A named character vector where:
#'   \itemize{
#'     \item Names are MD5 hashes of duplicate file contents
#'     \item Values are representative file paths (first occurrence) for each duplicate set
#'   }
#'   Returns empty character vector if no duplicates found.
#'   Returns NULL invisibly if no WAV files found.
#'
#' @details
#' The function searches the specified directory and its immediate subdirectories
#' (one level deep) for WAV files. File matching is case-insensitive. Duplicates
#' are identified by computing MD5 hashes of the binary file contents.
#'
#' Requires the \code{digest} package to be installed.
#'
#' @examples
#' \dontrun{
#' # Find duplicate WAV files
#' dups <- find_duplicate_wavs("/path/to/audio/library")
#'
#' # Print duplicate sets
#' if (length(dups) > 0) {
#'   print(dups)
#' }
#' }
#'
#' @export
#' @importFrom digest digest
#' @param parallel Logical. If TRUE, use parallel processing for hashing. Default is TRUE.
#' @param n_cores Integer. Number of cores to use. Default is NULL (auto-detect).
#'
#' @export
#' @importFrom digest digest
find_duplicate_wavs <- function(dir, parallel = TRUE, n_cores = NULL) {
  # Check that directory exists
  if (!dir.exists(dir)) {
    stop("Directory does not exist: ", dir)
  }
  
  # Build a vector of directories to search:
  # the main dir + its immediate subdirectories (one level down)
  search_dirs <- c(
    dir,
    list.dirs(dir, recursive = FALSE, full.names = TRUE)
  )
  
  # List all .wav files (case-insensitive) in those directories
  wav_files <- list.files(
    path       = search_dirs,
    pattern    = "(?i)\\.wav$",  # regex, case-insensitive .wav
    full.names = TRUE
  )
  
  if (length(wav_files) == 0L) {
    message("No .wav files found in ", dir, " or its immediate subfolders")
    return(invisible(NULL))
  }
  
  # Need the 'digest' package for hashing
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required. Install it with install.packages('digest').")
  }
  
  message("Found ", length(wav_files), " total .wav files. Checking file sizes...")
  
  # 1. PRE-FILTER BY SIZE
  # Only files with identical sizes CAN be duplicates.
  file_sizes <- file.size(wav_files)
  
  # Identify sizes that appear more than once
  dup_sizes <- file_sizes[duplicated(file_sizes) | duplicated(file_sizes, fromLast = TRUE)]
  dup_sizes <- unique(dup_sizes)
  
  # Filter candidates
  candidate_indices <- which(file_sizes %in% dup_sizes)
  
  if (length(candidate_indices) == 0) {
    message("No files share the same size. No content duplicates found.")
    return(invisible(character(0)))
  }
  
  candidates <- wav_files[candidate_indices]
  message("Hashing ", length(candidates), " candidate files (sharing same size)...")
  
  # 2. HASH CANDIDATES (Parallel Option)
  
  compute_hash <- function(f) {
    # Use xxhash64 for speed if available, fallback to md5
    tryCatch({
      digest::digest(f, file = TRUE, algo = "xxhash64", serialize = FALSE)
    }, error = function(e) {
      digest::digest(f, file = TRUE, algo = "md5", serialize = FALSE)
    })
  }
  
  hashes <- character(length(candidates))
  
  use_parallel <- parallel && length(candidates) > 50 && requireNamespace("parallel", quietly = TRUE)
  
  if (use_parallel) {
    if (is.null(n_cores)) {
      n_cores <- max(1, floor(parallel::detectCores() / 2))
    }
    # Cap cores
    n_cores <- min(n_cores, length(candidates))
    
    if (n_cores > 1) {
      message("Using parallel hashing with ", n_cores, " cores...")
      cl <- parallel::makeCluster(n_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      
      # Export digest if needed (usually loaded on workers if package matches)
      # parallel::clusterEvalQ(cl, library(digest))
      
      hashes <- unlist(parallel::parLapply(cl, candidates, function(f) {
        digest::digest(f, file = TRUE, algo = "xxhash64", serialize = FALSE)
      }))
    } else {
      hashes <- vapply(candidates, compute_hash, character(1))
    }
  } else {
    # Sequential (with progress bar if many files?)
    hashes <- vapply(candidates, compute_hash, character(1))
  }
  
  # Create data frame of candidates
  df <- data.frame(
    file = candidates,
    hash = hashes,
    stringsAsFactors = FALSE
  )
  
  # Find hashes that appear more than once
  dup_hashes <- unique(df$hash[duplicated(df$hash)])
  
  if (length(dup_hashes) == 0L) {
    message("No duplicate .wav contents found after hashing candidates.")
    return(invisible(character(0)))
  }
  
  # For each duplicate hash, pick ONE representative file path
  dup_representatives <- vapply(
    dup_hashes,
    FUN = function(h) {
      files <- df$file[df$hash == h]
      files[1L]
    },
    FUN.VALUE = character(1)
  )
  
  names(dup_representatives) <- dup_hashes
  
  message("Found ", length(dup_representatives), " sets of duplicate .wav files.")
  return(dup_representatives)
}
