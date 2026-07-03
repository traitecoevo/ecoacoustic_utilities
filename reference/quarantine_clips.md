# Quarantine (Reversibly Remove) Clips Named in a Removal Plan

Moves the clips flagged for removal in a reconciled plan (for example
the output of \`birdnetEmbed::near_duplicate_removal_plan()\` passed
through \[check_clips_on_disk()\]) into a quarantine directory that
mirrors the \`\<species\>/\<clip\>\` layout of the source library.
Moving rather than deleting keeps the operation fully reversible:
inspect the quarantine, then either delete it to reclaim space or move
files back. A manifest CSV is written into the quarantine root recording
every move.

## Usage

``` r
quarantine_clips(plan, quarantine_dir, action = "remove", dry_run = FALSE)
```

## Arguments

- plan:

  A reconciled removal plan: a data frame with columns \`species\`,
  \`clip\`, \`path\`, \`exists\`, and \`action\` (as produced by
  \`near_duplicate_removal_plan()\` then \[check_clips_on_disk()\]).

- quarantine_dir:

  Character. Destination root. Created if it does not exist; per-species
  sub-directories are created as needed.

- action:

  Character. Which \`action\` value marks a clip for removal. Default
  \`"remove"\`.

- dry_run:

  Logical. If \`TRUE\`, report what would move without touching any
  files (and without writing a manifest). Default \`FALSE\`.

## Value

Invisibly, the subset of \`plan\` that was (or would be) moved, with an
added \`dest\` column (the quarantine path) and, unless \`dry_run\`, a
\`moved\` logical column reporting per-file success. Only rows with
\`action == \<action\>\` \*\*and\*\* \`exists == TRUE\` are considered;
rows whose files are already missing on disk are skipped.

## Details

Files are moved with \[file.rename()\], falling back to copy-then-delete
when the rename fails (e.g. across file systems). The manifest
\`quarantine_manifest.csv\` in \`quarantine_dir\` lets you restore later
by moving each \`dest\` back to its \`path\`. Reconcile the plan against
disk immediately before calling this, since embeddings can drift from
the library.

## See also

Other audio:
[`analyze_call_properties()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/analyze_call_properties.md),
[`biggest_files()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/biggest_files.md),
[`check_clips_on_disk()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/check_clips_on_disk.md),
[`convert_to_wav()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/convert_to_wav.md),
[`find_duplicate_wavs()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/find_duplicate_wavs.md),
[`print.training_dataset_summary()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/print.training_dataset_summary.md),
[`training_dataset_summary()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/training_dataset_summary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
plan <- birdnetEmbed::near_duplicate_removal_plan(dups)
plan <- check_clips_on_disk(plan, "call_library/reallybig")
quarantine_clips(plan, "call_library/reallybig_quarantine", dry_run = TRUE)
quarantine_clips(plan, "call_library/reallybig_quarantine")
} # }
```
