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
  as_wav = FALSE
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
  subdirectories "audio" and "metadata.csv". Default is "sounds".

- include_taxon_name:

  Logical. If TRUE, prepends the taxon name to the filename. Default is
  TRUE.

- supplier:

  Character. Data supplier to filter for. Options are "all" or "CSIRO".
  If "CSIRO", filters for records with institutionCode "ANWC" and
  collectionCode "Sounds". A lightweight count check is performed first;
  if no CSIRO records exist, the function returns 0 immediately without
  downloading the full media metadata. Default is "all".

- as_wav:

  Logical. If TRUE, converts downloaded audio files to WAV format.
  Default is FALSE.

## Value

Character. The absolute path to the metadata CSV file created/updated.

## Details

When `download = TRUE`, the function:

- Creates `out_dir/audio/` directory for audio files

- Creates `out_dir/metadata_ala.csv` with record metadata

- Downloads files named as `[Taxon_Name_]<record_id>.<ext>`

- Deduplicates media records by URL before downloading (ALA can return
  multiple rows pointing to the same audio file)

- Skips files that already exist in the output directory or have been
  downloaded in the current batch

- Uses the ALA `recordID` for filenames; when missing, extracts a UUID
  from the audio URL for traceability back to the source record

- Falls back to the searched `taxon_name` when `scientificName` is
  missing from a record

- Automatically converts recordings to WAV if `as_wav = TRUE`

## Examples

``` r
if (FALSE) { # \dontrun{
galah_config(email = "your-email@example.com")
# Search and download up to 10 sounds as WAV
get_ala_sounds("Teleogryllus commodus", target_n = 10, as_wav = TRUE)
} # }
```
