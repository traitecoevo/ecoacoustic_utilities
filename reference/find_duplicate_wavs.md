# Find Duplicate WAV Files by Content Hash

Searches a directory and its immediate subdirectories for duplicate WAV
files based on MD5 hash of file contents. Returns one representative
file path for each set of duplicates found.

## Usage

``` r
find_duplicate_wavs(dir, parallel = TRUE, n_cores = NULL)
```

## Arguments

- dir:

  Character. Path to the directory to search. Must exist.

- parallel:

  Logical. If TRUE, use parallel processing for hashing. Default is
  TRUE.

- n_cores:

  Integer. Number of cores to use. Default is NULL (auto-detect).

## Value

A named character vector where:

- Names are MD5 hashes of duplicate file contents

- Values are representative file paths (first occurrence) for each
  duplicate set

Returns empty character vector if no duplicates found. Returns NULL
invisibly if no WAV files found.

## Details

The function searches the specified directory and its immediate
subdirectories (one level deep) for WAV files. File matching is
case-insensitive. Duplicates are identified by computing MD5 hashes of
the binary file contents.

Requires the `digest` package to be installed.

## See also

Other audio:
[`analyze_call_properties()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/analyze_call_properties.md),
[`biggest_files()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/biggest_files.md),
[`convert_to_wav()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/convert_to_wav.md),
[`print.training_dataset_summary()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/print.training_dataset_summary.md),
[`training_dataset_summary()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/training_dataset_summary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Find duplicate WAV files
dups <- find_duplicate_wavs("/path/to/audio/library")

# Print duplicate sets
if (length(dups) > 0) {
  print(dups)
}
} # }
```
