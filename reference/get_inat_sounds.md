# Download Sound Recordings from iNaturalist

Queries the iNaturalist API to find and optionally download sound
recordings for a specified taxon. Can filter by geographic location and
observation quality.

## Usage

``` r
get_inat_sounds(
  taxon_name,
  place_name = "Australia",
  target_n = 300,
  download = TRUE,
  out_dir = "sounds",
  allowed_licenses = tolower(c("cc0", "cc-by", "cc-by-sa", "cc-by-nc", "cc-by-nc-sa")),
  use_place_filter = FALSE,
  quality = c("research", "all"),
  include_taxon_name = TRUE,
  as_wav = FALSE
)
```

## Arguments

- taxon_name:

  Character. Scientific or common name of the taxon to search for.

- place_name:

  Character. Name of the geographic place to filter by. Required if
  `use_place_filter = TRUE`. Default is "Australia".

- target_n:

  Numeric. Target number of sound files to download. Default is 300.

- download:

  Logical. If TRUE, downloads audio files. If FALSE, only returns the
  count of available recordings. Default is TRUE

- out_dir:

  Character. Output directory for downloaded files. Will create
  subdirectories "audio" and "metadata.csv". Default is "sounds".

- allowed_licenses:

  Character vector. Lowercase license codes to accept. Default includes
  Creative Commons licenses: cc0, cc-by, cc-by-sa, cc-by-nc,
  cc-by-nc-sa. Empty license codes are also accepted.

- use_place_filter:

  Logical. If TRUE, filters observations by place. If FALSE, searches
  globally, UNLESS `place_name` is explicitly provided. Default is
  FALSE.

- quality:

  Character. Quality grade filter: "research" for research-grade
  observations only, or "all" for all quality grades. Default is
  "research".

- include_taxon_name:

  Logical. If TRUE, prepends the taxon name to the filename (e.g.,
  "Genus_species_ObsID_SoundID.ext"). Default is TRUE.

- as_wav:

  Logical. If TRUE, converts downloaded audio files to WAV format.
  Default is FALSE.

## Value

Integer. Total number of matching records found in iNaturalist.

## Details

When `download = TRUE`, the function:

- Creates `out_dir/audio/` directory for audio files

- Creates `out_dir/metadata.csv` with observation metadata

- Downloads files named as `[Taxon_Name_]ObsID_SoundID.ext`

- Skips files that already exist in the output directory

- Respects API rate limits with 1.1 second delays between requests

- Automatically converts recordings to WAV if `as_wav = TRUE`

## Examples

``` r
if (FALSE) { # \dontrun{
# Check how many recordings are available
n <- get_inat_sounds(
  "Turnix maculosus",
  place_name = "Australia",
  download = FALSE
)

# Download up to 300 research-grade recordings
get_inat_sounds(
  "Turnix maculosus",
  place_name = "Australia",
  target_n = 300,
  download = TRUE,
  quality = "research"
)

# Global search (no place filter)
get_inat_sounds(
  "Gryllus bimaculatus",
  use_place_filter = FALSE,
  target_n = 100,
  download = TRUE,
  as_wav = TRUE
)
} # }
```
