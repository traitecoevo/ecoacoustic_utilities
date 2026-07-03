# Check Whether Referenced Clips Still Exist on Disk

Reconciles a table of clip references (for example a near-duplicate
removal plan produced from BirdNET embeddings) against a clip library
laid out as one sub-directory per class, i.e.
\`dir/\<species\>/\<clip\>\`. Embeddings can drift out of sync with the
library over time, so a clip named in the reference may no longer be
present on disk. Use this before deleting or moving files so you never
act on a stale path.

## Usage

``` r
check_clips_on_disk(df, dir, species_col = "species", clip_col = "clip")
```

## Arguments

- df:

  A data frame containing at least the species and clip columns named by
  \`species_col\` and \`clip_col\`.

- dir:

  Character. Path to the library root, whose immediate sub-directories
  are the class folders. Must exist.

- species_col:

  Character. Name of the column holding the class / species-directory
  name. Default \`"species"\`.

- clip_col:

  Character. Name of the column holding the clip file name. Default
  \`"clip"\`.

## Value

\`df\` unchanged except for two appended columns:

- \`path\`:

  Full expected path, \`file.path(dir, species, clip)\`.

- \`exists\`:

  Logical; \`TRUE\` when that file is present on disk.

Row order is preserved. A message reports how many of the referenced
clips were found.

## Details

Existence is tested with \[file.exists()\] on the composed path; no
hashing or content comparison is done (see \[find_duplicate_wavs()\] for
content-level duplicate detection). Only the exact \`species\`/\`clip\`
combination is checked — a clip that was moved to a different class
folder counts as missing.

## See also

Other audio:
[`analyze_call_properties()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/analyze_call_properties.md),
[`biggest_files()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/biggest_files.md),
[`convert_to_wav()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/convert_to_wav.md),
[`find_duplicate_wavs()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/find_duplicate_wavs.md),
[`print.training_dataset_summary()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/print.training_dataset_summary.md),
[`quarantine_clips()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/quarantine_clips.md),
[`training_dataset_summary()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/training_dataset_summary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# plan produced by birdnetEmbed::near_duplicate_removal_plan()
checked <- check_clips_on_disk(plan, "OneDrive-UNSW/call_library/reallybig")

# Only remove files that are both flagged and actually present:
to_delete <- checked[checked$action == "remove" & checked$exists, "path"]
} # }
```
