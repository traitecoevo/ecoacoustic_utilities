# Download Sound Recordings from the ANWC (CSIRO) Sound Archive via ALA

Searches the Australian National Wildlife Collection (ANWC / CSIRO)
sound archive as exposed through ALA (data resource `dr341`,
`collectionCode == "Sounds"`) and downloads confirmed audio files.

## Usage

``` r
get_anwc_sounds(
  taxon_name,
  target_n = 50,
  download = TRUE,
  out_dir = "sounds",
  include_taxon_name = TRUE,
  as_wav = TRUE
)
```

## Arguments

- taxon_name:

  Character. Scientific or common name of the taxon.

- target_n:

  Numeric. Target number of audio files to download. Default is 50.

- download:

  Logical. If `FALSE`, returns the count of confirmed audio records
  without downloading. Default is `TRUE`.

- out_dir:

  Character. Output directory. Creates `out_dir/audio/` and
  `out_dir/metadata_anwc.csv`. Default is `"sounds"`.

- include_taxon_name:

  Logical. Prepend taxon name to filenames. Default is `TRUE`.

- as_wav:

  Logical. Convert downloaded files to 48 kHz 16-bit PCM WAV after
  download (required for BirdNET and other bioacoustic pipelines).
  Default is `TRUE`.

## Value

The absolute path to the metadata CSV, or `0` if no audio was found /
downloaded.

## Details

The ANWC archive is stored in ALA under `dataResourceUid == "dr341"`
with `collectionCode == "Sounds"`. Crucially, these records are *not*
indexed with `multimedia == "Sound"`, so they are invisible to
[`atlas_media()`](https://galah.ala.org.au/R/reference/atlas_.html) and
[`get_ala_sounds()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/get_ala_sounds.md).
Not all ANWC records have audio uploaded; this function checks each
occurrence individually (via the ALA biocache and images APIs) to
confirm audio before downloading.

Because of the per-record API checks, this function is substantially
slower than
[`get_ala_sounds()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/get_ala_sounds.md),
especially for taxa with many ANWC records. A progress counter is
printed as audio files are confirmed.

## See also

Other ala:
[`get_ala_circle_occurrences()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/get_ala_circle_occurrences.md),
[`get_ala_sounds()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/get_ala_sounds.md)

## Examples

``` r
if (FALSE) { # \dontrun{
galah_config(email = "your-email@example.com")
get_anwc_sounds("Vanellus miles", target_n = 10)
} # }
```
