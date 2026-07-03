#' Check Whether Referenced Clips Still Exist on Disk
#'
#' Reconciles a table of clip references (for example a near-duplicate removal
#' plan produced from BirdNET embeddings) against a clip library laid out as one
#' sub-directory per class, i.e. `dir/<species>/<clip>`. Embeddings can drift
#' out of sync with the library over time, so a clip named in the reference may
#' no longer be present on disk. Use this before deleting or moving files so you
#' never act on a stale path.
#'
#' @param df A data frame containing at least the species and clip columns named
#'   by `species_col` and `clip_col`.
#' @param dir Character. Path to the library root, whose immediate
#'   sub-directories are the class folders. Must exist.
#' @param species_col Character. Name of the column holding the class /
#'   species-directory name. Default `"species"`.
#' @param clip_col Character. Name of the column holding the clip file name.
#'   Default `"clip"`.
#'
#' @return `df` unchanged except for two appended columns:
#'   \describe{
#'     \item{`path`}{Full expected path, `file.path(dir, species, clip)`.}
#'     \item{`exists`}{Logical; `TRUE` when that file is present on disk.}
#'   }
#'   Row order is preserved. A message reports how many of the referenced clips
#'   were found.
#'
#' @details
#' Existence is tested with [file.exists()] on the composed path; no hashing or
#' content comparison is done (see [find_duplicate_wavs()] for content-level
#' duplicate detection). Only the exact `species`/`clip` combination is checked —
#' a clip that was moved to a different class folder counts as missing.
#'
#' @examples
#' \dontrun{
#' # plan produced by birdnetEmbed::near_duplicate_removal_plan()
#' checked <- check_clips_on_disk(plan, "OneDrive-UNSW/call_library/reallybig")
#'
#' # Only remove files that are both flagged and actually present:
#' to_delete <- checked[checked$action == "remove" & checked$exists, "path"]
#' }
#'
#' @family audio
#' @export
check_clips_on_disk <- function(df,
                                dir,
                                species_col = "species",
                                clip_col    = "clip") {
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame.", call. = FALSE)
  }
  missing_cols <- setdiff(c(species_col, clip_col), colnames(df))
  if (length(missing_cols) > 0L) {
    stop(
      "'df' is missing column(s): ", paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (!dir.exists(dir)) {
    stop("Directory does not exist: ", dir, call. = FALSE)
  }

  df$path <- file.path(dir, df[[species_col]], df[[clip_col]])
  df$exists <- file.exists(df$path)

  message(
    "Found ", sum(df$exists), " of ", nrow(df),
    " referenced clips on disk (", sum(!df$exists), " missing)."
  )
  df
}
