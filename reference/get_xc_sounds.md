# Download Sound Recordings from Xeno-Canto

Queries the Xeno-Canto API (v3) to find and optionally download sound
recordings for a specified taxon. Can filter by country and quality
grade.

## Usage

``` r
get_xc_sounds(
  taxon_name,
  country = NULL,
  api_key = Sys.getenv("XC_API_KEY"),
  target_n = 300,
  download = TRUE,
  out_dir = "sounds",
  allowed_licenses = tolower(c("cc0", "cc-by", "cc-by-sa", "cc-by-nc", "cc-by-nc-sa",
    "cc-by-nd", "cc-by-nc-nd")),
  quality = c("A", "B"),
  sound_type = NULL,
  include_taxon_name = TRUE,
  as_wav = FALSE
)
```

## Arguments

- taxon_name:

  Character. Scientific name of the taxon to search for.

- country:

  Character. Country name to filter by (e.g. "Australia"). If provided,
  restricts results to that country. Default is NULL (global).

- api_key:

  Character. Your Xeno-Canto API key. Retrieve one from
  <https://xeno-canto.org/account>. Defaults to the environment variable
  `XC_API_KEY` if set.

- target_n:

  Numeric. Maximum number of recordings to download. Default is 300.

- download:

  Logical. If TRUE, downloads audio files. If FALSE, only returns the
  count of available recordings. Default is TRUE.

- out_dir:

  Character. Output directory for downloaded files. Will create
  subdirectories "audio" and "metadata.csv". Default is "sounds".

- allowed_licenses:

  Character vector. Lowercase CC license codes to accept. Default
  accepts all Creative Commons licenses: cc0, cc-by, cc-by-sa, cc-by-nc,
  cc-by-nc-sa, cc-by-nd, cc-by-nc-nd. The recorded license is written to
  the metadata CSV, so the more restrictive NoDerivatives (cc-by-nd /
  cc-by-nc-nd) records can be filtered out afterwards if redistribution
  matters.

- quality:

  Character vector. Xeno-Canto quality grades to include. Grades run
  from "A" (best) to "E" (worst). Default is c("A", "B").

- sound_type:

  Character or NULL. Filter by sound type, e.g. "song", "call", "alarm
  call". NULL means no filter. Default is NULL.

- include_taxon_name:

  Logical. If TRUE, prepends the taxon name to the filename (e.g.
  "Genus_species_XC123456.mp3"). Default is TRUE.

- as_wav:

  Logical. If TRUE, converts downloaded audio files to WAV format.
  Default is FALSE.

## Value

Integer. Total number of matching recordings found on Xeno-Canto.

## Details

When `download = TRUE`, the function:

- Creates `out_dir/audio/` directory for audio files

- Creates `out_dir/metadata.csv` with recording metadata

- Downloads files named as `[Taxon_Name_]XC{id}.mp3`

- Skips files that already exist in the output directory

- Respects API rate limits with 1.1 second delays between page requests

- Automatically converts recordings to WAV if `as_wav = TRUE`

## Examples

``` r
if (FALSE) { # \dontrun{
# Check how many recordings are available globally
n <- get_xc_sounds("Turnix maculosus", download = FALSE)

# Download up to 200 A/B-quality recordings from Australia
get_xc_sounds(
  "Turnix maculosus",
  country = "Australia",
  target_n = 200,
  quality = c("A", "B")
)

# Download calls only, convert to WAV
get_xc_sounds(
  "Gryllus bimaculatus",
  sound_type = "call",
  target_n = 100,
  as_wav = TRUE
)
} # }
```
