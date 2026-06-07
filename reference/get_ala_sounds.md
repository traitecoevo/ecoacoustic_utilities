# Download Sound Recordings from Atlas of Living Australia (ALA)

Queries the ALA for sound recordings of a specified taxon and downloads
them. This function improves upon basic media downloads by handling
metadata and limiting download counts.

## Usage

``` r
get_ala_sounds(
  taxon_name,
  target_n = 50,
  download = TRUE,
  out_dir = "sounds",
  include_taxon_name = TRUE,
  supplier = c("all", "CSIRO"),
  as_wav = TRUE
)
```

## Arguments

- taxon_name:

  Character. Scientific or common name of the taxon to search for.

- target_n:

  Numeric. Target number of sound files to download. Default is 50.

- download:

  Logical. If TRUE, downloads audio files. If FALSE, only returns the
  count of available recordings. Default is TRUE.

- out_dir:

  Character. Output directory for downloaded files. Will create
  subdirectories "audio" and "metadata_ala.csv". Default is "sounds".

- include_taxon_name:

  Logical. If TRUE, prepends the taxon name to the filename. Default is
  TRUE.

- supplier:

  Deprecated. ANWC/CSIRO recordings are not indexed under
  `multimedia == "Sound"` in ALA and cannot be retrieved via this
  function. Use
  [`get_anwc_sounds()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/get_anwc_sounds.md)
  instead.

- as_wav:

  Logical. If TRUE, converts downloaded audio files to 48 kHz 16-bit PCM
  WAV (required for BirdNET and other bioacoustic pipelines). Default is
  TRUE.

## Value

Character. The absolute path to the metadata CSV file created/updated,
or 0 if no recordings were found/downloaded.

## Details

ALA sound records are retrieved via
[`atlas_media()`](https://galah.ala.org.au/R/reference/atlas_.html) with
a `multimedia == "Sound"` filter. Because some ALA records tagged as
"Sound" contain non-audio media (e.g. `image/jpeg`), a secondary
`mimetype` check keeps only records with `audio/*` MIME types.

The download URL for ALA sounds is the `image_url` field (ALA uses this
field for all media types). File extensions are derived from the
`mimetype` field, not from the URL (ALA URLs have no extension).

A known galah 2.x bug causes a recycling error when fetching many sound
records in a single request. The function automatically falls back to
year-by-year fetching (most recent years first) to work around this.

## Examples

``` r
if (FALSE) { # \dontrun{
galah_config(email = "your-email@example.com")
# Search and download up to 10 sounds as WAV
get_ala_sounds("Ninox boobook", target_n = 10, as_wav = TRUE)
} # }
```
