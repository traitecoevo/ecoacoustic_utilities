# Get iNaturalist Species Summary by Sound Recording Counts

Queries the iNaturalist API to find species-level taxa with sound
recordings that meet specified criteria. Counts valid sound recordings
per species based on license requirements.

## Usage

``` r
get_inat_species_summary(
  min_recordings = 1,
  taxon_name = NULL,
  taxon_id = NULL,
  place_name = "Australia",
  place_id = NULL,
  quality_grade = "research",
  per_page = 200,
  max_pages = 10,
  allowed_licenses = tolower(c("cc0", "cc-by", "cc-by-sa", "cc-by-nc", "cc-by-nc-sa"))
)
```

## Arguments

- min_recordings:

  Numeric. Minimum number of valid recordings required to include a
  species in results. Default is 1.

- taxon_name:

  Character. Optional taxon name to resolve to an ID. If provided,
  overrides `taxon_id`. Default is NULL.

- taxon_id:

  Integer. The iNaturalist taxon ID to search within. Default is NULL
  (defaults to 47651 Orthoptera if both taxon_id and taxon_name are
  NULL). Ignored if `taxon_name` is provided.

- place_name:

  Character. Name of the place to search within. Default is "Australia".

- place_id:

  Integer. Optional iNaturalist place ID. If NULL, will be resolved from
  `place_name`. Default is NULL.

- quality_grade:

  Character. Quality grade filter for observations. Default is
  "research".

- per_page:

  Integer. Number of results per API page. Default is 200.

- max_pages:

  Numeric. Maximum number of pages to retrieve. Default is 10. Use Inf
  for all available pages.

- allowed_licenses:

  Character vector. Lowercase license codes to accept. Default includes
  Creative Commons licenses: cc0, cc-by, cc-by-sa, cc-by-nc,
  cc-by-nc-sa.

## Value

A data.frame with columns: taxon_id, scientific_name, common_name,
n_recordings. Sorted by n_recordings in descending order. Returns empty
data.frame if no species meet criteria.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get Orthoptera species with at least 5 recordings in Australia
orth_aus <- get_inat_species_summary(
  min_recordings = 5,
  place_name = "Australia"
)

# Search by taxon name instead of ID
crickets <- get_inat_species_summary(
  taxon_name = "Gryllidae",
  min_recordings = 5,
  place_name = "United States"
)
} # }
```
