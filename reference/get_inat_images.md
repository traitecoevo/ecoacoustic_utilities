# Download Images from iNaturalist

Queries the iNaturalist API to find and optionally download photos. Two
modes are supported: (1) search by `taxon_name` (optionally filtered by
geographic location and observation quality), or (2) fetch a specific
set of observations by passing `observation_ids`.

## Usage

``` r
get_inat_images(
  taxon_name = NULL,
  place_name = "Australia",
  observation_ids = NULL,
  target_n = 300,
  download = TRUE,
  out_dir = "images",
  allowed_licenses = tolower(c("cc0", "cc-by", "cc-by-sa", "cc-by-nc", "cc-by-nc-sa")),
  use_place_filter = FALSE,
  quality = c("research", "all"),
  include_taxon_name = TRUE,
  image_size = c("original", "large", "medium", "small")
)
```

## Arguments

- taxon_name:

  Character. Scientific or common name of the taxon to search for.
  Required unless `observation_ids` is supplied. Default is NULL.

- place_name:

  Character. Name of the geographic place to filter by. Required if
  `use_place_filter = TRUE`. Default is "Australia". Ignored when
  `observation_ids` is supplied.

- observation_ids:

  Vector. iNaturalist observation IDs to download. May be numeric,
  character, or full observation URLs (e.g.
  "https://www.inaturalist.org/observations/54040926"); trailing IDs are
  extracted automatically. When supplied, the function fetches exactly
  these observations and ignores `taxon_name`, `place_name`, and
  `quality`. Default is NULL.

- target_n:

  Numeric. Target number of photo records to check for download. Default
  is 300. When `observation_ids` is supplied and `target_n` is left at
  its default, all photos for all listed observations are downloaded;
  set it explicitly to cap the number of photos checked.

- download:

  Logical. If TRUE, downloads image files. If FALSE, only returns the
  count of available photos. Default is TRUE.

- out_dir:

  Character. Output directory for downloaded files. Will create
  subdirectories "images" and "metadata.csv". Default is "images".

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
  "research". Ignored when `observation_ids` is supplied.

- include_taxon_name:

  Logical. If TRUE, prepends the taxon name to the filename (e.g.,
  "Genus_species_ObsID_PhotoID.ext"). Default is TRUE.

- image_size:

  Character. Size of the image to download. One of "original" (up to
  2048px), "large" (up to 1024px), "medium" (up to 500px), or "small"
  (up to 240px). Default is "original".

## Value

Integer. Total number of matching records found in iNaturalist (in
taxon-search mode) or the number of observation IDs requested (in
`observation_ids` mode).

## Details

When `download = TRUE`, the function:

- Creates `out_dir/images/` directory for image files

- Creates `out_dir/metadata.csv` with observation metadata

- Downloads files named as `[Taxon_Name_]ObsID_PhotoID.ext`

- Skips files that already exist in the output directory

- Respects API rate limits with 1.1 second delays between requests

iNaturalist stores photos in several predefined sizes. The default URL
returned by the API uses the "square" (75px) size. This function
replaces the size component in the URL with the requested `image_size`.

In `observation_ids` mode the observations are fetched in batches of up
to 200 IDs per API request. The taxon name used for filenames and
metadata is taken from each observation's own identification rather than
a single searched taxon.

## See also

Other inaturalist:
[`get_inat_sounds()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/get_inat_sounds.md),
[`get_inat_species_summary()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/get_inat_species_summary.md),
[`plot_inat_species_summary()`](https://traitecoevo.github.io/ecoacoustic_utilities/reference/plot_inat_species_summary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Check how many photos are available
n <- get_inat_images(
    "Cacatua galerita",
    place_name = "Australia",
    download = FALSE
)

# Download up to 300 research-grade photos
get_inat_images(
    "Cacatua galerita",
    place_name = "Australia",
    target_n = 300,
    download = TRUE,
    quality = "research"
)

# Global search with medium-sized images
get_inat_images(
    "Danaus plexippus",
    use_place_filter = FALSE,
    target_n = 100,
    download = TRUE,
    image_size = "medium"
)

# Download a specific set of observations (e.g. from a CSV export)
obs <- read.csv("burnt_only_observations.csv")
get_inat_images(
    observation_ids = obs$occurrenceID,
    image_size = "large"
)
} # }
```
