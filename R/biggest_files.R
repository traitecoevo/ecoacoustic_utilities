#' Find the Largest Files in a Directory
#'
#' Recursively searches a directory and returns the N largest files by size,
#' with human-readable size formatting.
#'
#' @param root_dir Character. Path to the root directory to search. Must exist.
#' @param n Numeric. Number of largest files to return. Default is 10.
#' @param pattern Character. Optional regex pattern to filter files by name.
#'   Default is NULL (no filtering).
#' @param ignore_case Logical. Whether pattern matching should be case-insensitive.
#'   Default is TRUE.
#'
#' @return A data.frame with columns:
#'   \describe{
#'     \item{size_human}{Human-readable file size (e.g., "1.5 MB")}
#'     \item{size}{File size in bytes}
#'     \item{mtime}{Last modification time as POSIXct}
#'     \item{path}{Full file path}
#'   }
#'   Returns empty data.frame if no files are found.
#'
#' @examples
#' \dontrun{
#' # Find 20 largest files in a directory
#' big_files <- biggest_files("/path/to/directory", n = 20)
#'
#' # Find largest WAV files
#' big_wavs <- biggest_files(
#'   "/path/to/audio",
#'   n = 10,
#'   pattern = "\\.wav$"
#' )
#' }
#'
#' @export
#' @importFrom utils head
biggest_files <- function(root_dir, n = 10, pattern = NULL, ignore_case = TRUE) {
  stopifnot(dir.exists(root_dir))
  stopifnot(is.numeric(n), length(n) == 1, n >= 1)
  
  # List all files recursively (full paths)
  files <- list.files(
    path = root_dir,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE,
    include.dirs = FALSE,
    ignore.case = ignore_case,
    pattern = pattern
  )
  
  if (length(files) == 0) {
    return(data.frame(
      size_human = character(),
      size = numeric(),
      mtime = as.POSIXct(character()),
      path = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Get file info; keep only real files that exist and have size
  info <- file.info(files)
  info$path <- rownames(info)
  info <- info[!is.na(info$size) & file.exists(info$path), , drop = FALSE]
  
  if (nrow(info) == 0) {
    return(data.frame(
      size_human = character(),
      size = numeric(),
      mtime = as.POSIXct(character()),
      path = character(),
      stringsAsFactors = FALSE
    ))
  }
  
  # Top N by size
  topn <- info[order(info$size, decreasing = TRUE), c("path", "size", "mtime"), drop = FALSE]
  topn <- head(topn, n)
  
  # Pretty print sizes
  pretty_size <- function(bytes) {
    units <- c("B", "KB", "MB", "GB", "TB")
    i <- if (bytes > 0) floor(log(bytes, 1024)) + 1 else 1
    i <- pmin(i, length(units))
    value <- bytes / (1024^(i - 1))
    sprintf("%.2f %s", value, units[i])
  }
  
  topn$size_human <- vapply(topn$size, pretty_size, character(1))
  topn <- topn[, c("size_human", "size", "mtime", "path"), drop = FALSE]
  
  rownames(topn) <- NULL
  topn
}
