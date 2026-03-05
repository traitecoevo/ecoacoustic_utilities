# Download Images from iNaturalist

Queries the iNaturalist API to find and optionally download photos for a
specified taxon. Can filter by geographic location and observation
quality.

## Usage

``` r
get_inat_images(
  taxon_name,
  place_name = "Australia",
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

- place_name:

  Character. Name of the geographic place to filter by. Required if
  `use_place_filter = TRUE`. Default is "Australia".

- target_n:

  Numeric. Target number of image files to download. Default is 300.

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
  "research".

- include_taxon_name:

  Logical. If TRUE, prepends the taxon name to the filename (e.g.,
  "Genus_species_ObsID_PhotoID.ext"). Default is TRUE.

- image_size:

  Character. Size of the image to download. One of "original" (up to
  2048px), "large" (up to 1024px), "medium" (up to 500px), or "small"
  (up to 240px). Default is "original".

## Value

Integer. Total number of matching records found in iNaturalist.

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
} # }
```
