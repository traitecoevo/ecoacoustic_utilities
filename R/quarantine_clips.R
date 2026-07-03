#' Quarantine (Reversibly Remove) Clips Named in a Removal Plan
#'
#' Moves the clips flagged for removal in a reconciled plan (for example the
#' output of `birdnetEmbed::near_duplicate_removal_plan()` passed through
#' [check_clips_on_disk()]) into a quarantine directory that mirrors the
#' `<species>/<clip>` layout of the source library. Moving rather than deleting
#' keeps the operation fully reversible: inspect the quarantine, then either
#' delete it to reclaim space or move files back. A manifest CSV is written into
#' the quarantine root recording every move.
#'
#' @param plan A reconciled removal plan: a data frame with columns `species`,
#'   `clip`, `path`, `exists`, and `action` (as produced by
#'   `near_duplicate_removal_plan()` then [check_clips_on_disk()]).
#' @param quarantine_dir Character. Destination root. Created if it does not
#'   exist; per-species sub-directories are created as needed.
#' @param action Character. Which `action` value marks a clip for removal.
#'   Default `"remove"`.
#' @param dry_run Logical. If `TRUE`, report what would move without touching
#'   any files (and without writing a manifest). Default `FALSE`.
#'
#' @return Invisibly, the subset of `plan` that was (or would be) moved, with an
#'   added `dest` column (the quarantine path) and, unless `dry_run`, a `moved`
#'   logical column reporting per-file success. Only rows with
#'   `action == <action>` **and** `exists == TRUE` are considered; rows whose
#'   files are already missing on disk are skipped.
#'
#' @details
#' Files are moved with [file.rename()], falling back to copy-then-delete when
#' the rename fails (e.g. across file systems). The manifest
#' `quarantine_manifest.csv` in `quarantine_dir` lets you restore later by
#' moving each `dest` back to its `path`. Reconcile the plan against disk
#' immediately before calling this, since embeddings can drift from the library.
#'
#' @examples
#' \dontrun{
#' plan <- birdnetEmbed::near_duplicate_removal_plan(dups)
#' plan <- check_clips_on_disk(plan, "call_library/reallybig")
#' quarantine_clips(plan, "call_library/reallybig_quarantine", dry_run = TRUE)
#' quarantine_clips(plan, "call_library/reallybig_quarantine")
#' }
#'
#' @family audio
#' @export
quarantine_clips <- function(plan,
                             quarantine_dir,
                             action  = "remove",
                             dry_run = FALSE) {
  needed <- c("species", "clip", "path", "exists", "action")
  if (!is.data.frame(plan) || !all(needed %in% colnames(plan))) {
    stop(
      "'plan' must be a reconciled removal plan with columns: ",
      paste(needed, collapse = ", "),
      call. = FALSE
    )
  }

  sel <- plan[plan$action == action & plan$exists, , drop = FALSE]
  sel$dest <- file.path(quarantine_dir, sel$species, sel$clip)

  if (nrow(sel) == 0L) {
    message("Nothing to quarantine (no matching, on-disk clips).")
    return(invisible(sel))
  }

  if (dry_run) {
    message("Dry run: would move ", nrow(sel), " clip(s) into ", quarantine_dir)
    return(invisible(sel))
  }

  for (d in unique(dirname(sel$dest))) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  }

  moved <- logical(nrow(sel))
  for (i in seq_len(nrow(sel))) {
    ok <- tryCatch(
      file.rename(sel$path[i], sel$dest[i]),
      warning = function(w) FALSE,
      error   = function(e) FALSE
    )
    if (!ok) {
      # Cross-device or other rename failure: copy then remove the original.
      if (file.copy(sel$path[i], sel$dest[i], overwrite = FALSE)) {
        ok <- suppressWarnings(file.remove(sel$path[i]))
        # If the source can't be removed (e.g. a read-only directory), roll the
        # copy back so we never leave a duplicate posing as a moved file.
        if (!ok) suppressWarnings(file.remove(sel$dest[i]))
      }
    }
    moved[i] <- isTRUE(ok)
  }
  sel$moved <- moved

  # Record only successful moves, so the manifest is a faithful restore log.
  # Append to any existing manifest (a quarantine dir may be filled over several
  # calls) rather than clobbering it.
  manifest <- file.path(quarantine_dir, "quarantine_manifest.csv")
  log_rows <- sel[sel$moved, , drop = FALSE]
  if (file.exists(manifest) && nrow(log_rows) > 0L) {
    prior <- utils::read.csv(manifest, stringsAsFactors = FALSE,
                             check.names = FALSE)
    shared <- intersect(colnames(prior), colnames(log_rows))
    log_rows <- rbind(prior[, shared, drop = FALSE],
                      log_rows[, shared, drop = FALSE])
  }
  utils::write.csv(log_rows, manifest, row.names = FALSE)

  message(
    "Quarantined ", sum(moved), " of ", nrow(sel), " clip(s) into ",
    quarantine_dir,
    if (any(!moved)) paste0(" (", sum(!moved), " failed)") else "",
    ". Manifest: ", manifest
  )
  invisible(sel)
}
