# Plot iNaturalist Species Summary

Creates an informative bar plot showing the number of sound recordings
per species from the output of
[`get_inat_species_summary()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/get_inat_species_summary.md).

## Usage

``` r
plot_inat_species_summary(
  species_summary,
  top_n = 20,
  use_common_names = TRUE,
  color = "#2E86AB",
  title = NULL
)
```

## Arguments

- species_summary:

  Data.frame. Output from
  [`get_inat_species_summary()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/get_inat_species_summary.md).
  Must contain columns: scientific_name (or common_name) and
  n_recordings.

- top_n:

  Integer. Number of top species to display. Default is 20.

- use_common_names:

  Logical. If TRUE and common_name column exists, use common names
  instead of scientific names. Default is TRUE.

- color:

  Character. Bar fill color. Default is "#2E86AB" (blue).

- title:

  Character. Plot title. If NULL, generates automatic title. Default is
  NULL.

## Value

A ggplot2 object that can be further customized or saved.

## Details

The function creates a horizontal bar plot with species names on the
y-axis and recording counts on the x-axis. Species are ordered by
recording count (highest at top). If a species has no common name, the
scientific name is used as fallback.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get species summary
summary <- get_inat_species_summary(
  taxon_name = "Orthoptera",
  place_name = "Australia",
  min_recordings = 50
)

# Plot top 15 species
plot_inat_species_summary(summary, top_n = 15)

# Use scientific names and custom color
plot_inat_species_summary(
  summary,
  top_n = 10,
  use_common_names = FALSE,
  color = "#E63946"
)

# Save plot
p <- plot_inat_species_summary(summary)
ggsave("species_summary.png", p, width = 10, height = 8)
} # }
```
