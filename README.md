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
- **`orthoptera_species_with_real_sounds()`** - Query iNaturalist for species with sound recordings

### ALA Functions

- **`get_ala_circle_occurrences()`** - Download occurrence records within a circular area

### Utility Functions

- **`biggest_files()`** - Find the largest files in a directory
- **`find_duplicate_wavs()`** - Detect duplicate WAV files by content hash

## Example Usage

```r
library(EcoacousticUtilities)

# Find largest files in a directory
big_files <- biggest_files("/path/to/directory", n = 20)

# Find duplicate WAV files
duplicates <- find_duplicate_wavs("/path/to/audio/library")

# Check how many iNaturalist recordings are available
n_sounds <- get_inat_sounds(
  "Turnix maculosus",
  place_name = "Australia",
  download = FALSE
)

# Download sound recordings
get_inat_sounds(
  "Turnix maculosus",
  place_name = "Australia",
  target_n = 100,
  download = TRUE,
  quality = "research"
)

# Get ALA occurrences within a radius
birds <- get_ala_circle_occurrences(
  taxon = "Aves",
  lat = -33.7,
  lon = 151.3,
  radius_km = 10,
  email = "your.email@example.com"
)
```

## License

MIT © Cornwell 2026
