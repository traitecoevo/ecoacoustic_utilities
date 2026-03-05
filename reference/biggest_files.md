# Find the Largest Files in a Directory

Recursively searches a directory and returns the N largest files by
size, with human-readable size formatting.

## Usage

``` r
biggest_files(root_dir, n = 10, pattern = NULL, ignore_case = TRUE)
```

## Arguments

- root_dir:

  Character. Path to the root directory to search. Must exist.

- n:

  Numeric. Number of largest files to return. Default is 10.

- pattern:

  Character. Optional regex pattern to filter files by name. Default is
  NULL (no filtering).

- ignore_case:

  Logical. Whether pattern matching should be case-insensitive. Default
  is TRUE.

## Value

A data.frame with columns:

- size_human:

  Human-readable file size (e.g., "1.5 MB")

- size:

  File size in bytes

- mtime:

  Last modification time as POSIXct

- path:

  Full file path

Returns empty data.frame if no files are found.

## Examples

``` r
if (FALSE) { # \dontrun{
# Find 20 largest files in a directory
big_files <- biggest_files("/path/to/directory", n = 20)

# Find largest WAV files
big_wavs <- biggest_files(
  "/path/to/audio",
  n = 10,
  pattern = "\\.wav$"
)
} # }
```
