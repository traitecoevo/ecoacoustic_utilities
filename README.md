# EcoacousticUtilities

<!-- badges: start -->
[![R-CMD-check](https://github.com/wcornwell/ecoacoustic_utilities/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wcornwell/ecoacoustic_utilities/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://codecov.io/gh/wcornwell/ecoacoustic_utilities/branch/main/graph/badge.svg)](https://app.codecov.io/gh/wcornwell/ecoacoustic_utilities?branch=main)
<!-- badges: end -->

Utilities for downloading and analyzing ecoacoustic data from iNaturalist and the Atlas of Living Australia (ALA).

## Installation

You can install the development version of EcoacousticUtilities from GitHub:

```r
# install.packages("devtools")
devtools::install_github("wcornwell/ecoacoustic_utilities")
```

## Functions

### iNaturalist Functions

- **`get_inat_sounds()`** - Download sound recordings from iNaturalist
  - **Use case**: Download actual audio files for a specific taxon
  - With `download=FALSE`: Get quick count of total observations with sounds
  - With `download=TRUE`: Download up to N audio files to local directory
  
- **`get_inat_species_summary()`** - Get species-level summary of sound recordings
  - **Use case**: Identify which species have the most recordings in a taxon/region
  - Returns data.frame with counts per species (taxon_id, scientific_name, common_name, n_recordings)
  - Useful for finding well-documented species before downloading

### ALA Functions

- **`get_ala_circle_occurrences()`** - Download occurrence records within a circular area

### Utility Functions

- **`biggest_files()`** - Find the largest files in a directory
- **`find_duplicate_wavs()`** - Detect duplicate WAV files by content hash
- **`training_dataset_summary()`** - Comprehensive analysis of audio training datasets

## Example Usage

```r
library(EcoacousticUtilities)

# Find largest files in a directory
big_files <- biggest_files("/path/to/directory", n = 20)

# Find duplicate WAV files
duplicates <- find_duplicate_wavs("/path/to/audio/library")

# === iNaturalist workflows ===

# 1. Quick check: How many sound recordings exist?
n_sounds <- get_inat_sounds(
  "Turnix maculosus",
  place_name = "Australia",
  download = FALSE  # Just get the count
)
print(n_sounds)  # e.g., 450

# 2. Which species have the most recordings?
species_summary <- get_inat_species_summary(
  taxon_name = "Orthoptera",  # All crickets/grasshoppers
  place_name = "Australia",
  min_recordings = 50  # Only species with 50+ recordings
)
print(species_summary)
# Returns data.frame with species ranked by recording count

# 3. Download actual audio files for a specific species
get_inat_sounds(
  "Turnix maculosus",
  place_name = "Australia",
  target_n = 100,
  download = TRUE,  # Download files
  quality = "research"
)

# === ALA workflow ===

# Get ALA occurrences within a radius
birds <- get_ala_circle_occurrences(
  taxon = "Aves",
  lat = -33.7,
  lon = 151.3,
  radius_km = 10,
  email = "your.email@example.com"
)

# === Training dataset analysis ===

# Analyze an audio training dataset
summary <- training_dataset_summary("/path/to/audio/dataset")
print(summary)  # Shows class distribution, file types, sizes, durations, outliers
```

## License

MIT © Cornwell 2026
