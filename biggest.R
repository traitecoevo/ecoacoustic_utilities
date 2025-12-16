# Return the N biggest files under a folder (recursive)
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

# Example
root_dir <- "/Users/z3484779/Library/CloudStorage/OneDrive-UNSW/call_library/reallybig"
print(biggest_files(root_dir, n = 20), row.names = FALSE)
