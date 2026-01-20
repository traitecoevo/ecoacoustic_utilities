#' Plot iNaturalist Species Summary
#'
#' Creates an informative bar plot showing the number of sound recordings per
#' species from the output of \code{get_inat_species_summary()}.
#'
#' @param species_summary Data.frame. Output from \code{get_inat_species_summary()}.
#'   Must contain columns: scientific_name (or common_name) and n_recordings.
#' @param top_n Integer. Number of top species to display. Default is 20.
#' @param use_common_names Logical. If TRUE and common_name column exists, use
#'   common names instead of scientific names. Default is TRUE.
#' @param color Character. Bar fill color. Default is "#2E86AB" (blue).
#' @param title Character. Plot title. If NULL, generates automatic title.
#'   Default is NULL.
#'
#' @return A ggplot2 object that can be further customized or saved.
#'
#' @details
#' The function creates a horizontal bar plot with species names on the y-axis
#' and recording counts on the x-axis. Species are ordered by recording count
#' (highest at top). If a species has no common name, the scientific name is
#' used as fallback.
#'
#' @examples
#' \dontrun{
#' # Get species summary
#' summary <- get_inat_species_summary(
#'   taxon_name = "Orthoptera",
#'   place_name = "Australia",
#'   min_recordings = 50
#' )
#'
#' # Plot top 15 species
#' plot_inat_species_summary(summary, top_n = 15)
#'
#' # Use scientific names and custom color
#' plot_inat_species_summary(
#'   summary,
#'   top_n = 10,
#'   use_common_names = FALSE,
#'   color = "#E63946"
#' )
#'
#' # Save plot
#' p <- plot_inat_species_summary(summary)
#' ggsave("species_summary.png", p, width = 10, height = 8)
#' }
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_col coord_flip labs theme_minimal theme element_text
plot_inat_species_summary <- function(
    species_summary,
    top_n = 20,
    use_common_names = TRUE,
    color = "#2E86AB",
    title = NULL
) {
  
  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plotting. Install it with: install.packages('ggplot2')")
  }
  
  # Validate input
  if (!is.data.frame(species_summary)) {
    stop("species_summary must be a data.frame")
  }
  
  if (nrow(species_summary) == 0) {
    stop("species_summary is empty")
  }
  
  if (!"n_recordings" %in% names(species_summary)) {
    stop("species_summary must contain 'n_recordings' column")
  }
  
  # Determine which name to use
  if (use_common_names && "common_name" %in% names(species_summary)) {
    # Use common name if available, fallback to scientific name
    species_summary$display_name <- ifelse(
      is.na(species_summary$common_name) | species_summary$common_name == "",
      species_summary$scientific_name,
      species_summary$common_name
    )
  } else if ("scientific_name" %in% names(species_summary)) {
    species_summary$display_name <- species_summary$scientific_name
  } else {
    stop("species_summary must contain 'scientific_name' or 'common_name' column")
  }
  
  # Select top N species
  top_species <- head(species_summary, top_n)
  
  # Reverse order for plotting (highest at top)
  top_species$display_name <- factor(
    top_species$display_name,
    levels = rev(top_species$display_name)
  )
  
  # Generate title if not provided
  if (is.null(title)) {
    title <- sprintf(
      "Top %d Species by Sound Recording Count",
      min(top_n, nrow(species_summary))
    )
  }
  
  # Create plot
  p <- ggplot2::ggplot(
    top_species,
    ggplot2::aes(x = n_recordings, y = display_name)
  ) +
    ggplot2::geom_col(fill = color) +
    ggplot2::labs(
      title = title,
      x = "Number of Sound Recordings",
      y = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(size = 10),
      panel.grid.major.y = ggplot2::element_blank()
    )
  
  return(p)
}
