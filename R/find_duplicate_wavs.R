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
find_duplicate_wavs <- function(dir) {
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
  
  # Compute a hash of each file's *binary* content
  hashes <- vapply(
    wav_files,
    FUN       = digest::digest,
    FUN.VALUE = character(1),
    file      = TRUE,     # hash file on disk, not R object
    algo      = "md5",    # or "sha1" etc.
    serialize = FALSE
  )
  
  df <- data.frame(
    file = wav_files,
    hash = hashes,
    stringsAsFactors = FALSE
  )
  
  # Find hashes that appear more than once
  dup_hashes <- unique(df$hash[duplicated(df$hash)])
  
  if (length(dup_hashes) == 0L) {
    message("No duplicate .wav contents found in ", dir, " or its immediate subfolders")
    # return empty character vector for consistency
    return(invisible(character(0)))
  }
  
  # For each duplicate hash, pick ONE representative file path
  dup_representatives <- vapply(
    dup_hashes,
    FUN = function(h) {
      files <- df$file[df$hash == h]
      # pick the first one (or use sort(files)[1L] if you want deterministic ordering)
      files[1L]
    },
    FUN.VALUE = character(1)
  )
  
  names(dup_representatives) <- dup_hashes
  
  message("Found ", length(dup_representatives), " sets of duplicate .wav files.")
  return(dup_representatives)
}
