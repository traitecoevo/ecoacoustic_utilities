# Download ALA Occurrences for a Taxon Within a Circular Area

Retrieves occurrence records from the Atlas of Living Australia (ALA)
for a specified taxon within a circular geographic area defined by a
center point and radius.

## Usage

``` r
get_ala_circle_occurrences(taxon, lat, lon, radius_km = 10, email = NULL, ...)
```

## Arguments

- taxon:

  Character. Scientific or common name that ALA can resolve.

- lat:

  Numeric. Latitude of circle centre in decimal degrees (WGS84).

- lon:

  Numeric. Longitude of circle centre in decimal degrees (WGS84).

- radius_km:

  Numeric. Radius in kilometres. Default is 10 km.

- email:

  Character. ALA/GBIF login email. If NULL, assumes you've already
  called
  [`galah_config()`](https://galah.ala.org.au/R/reference/galah_config.html)
  elsewhere. Default is NULL.

- ...:

  Additional arguments passed to
  [`atlas_occurrences()`](https://galah.ala.org.au/R/reference/atlas_.html),
  such as `filter = galah_filter(year >= 2000)`.

## Value

A tibble of occurrence records from the ALA.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all bird records within 10 km of a point
birds <- get_ala_circle_occurrences(
  taxon = "Aves",
  lat = -33.7,
  lon = 151.3,
  radius_km = 10,
  email = "your.email@example.com"
)

# With additional filters
recent_birds <- get_ala_circle_occurrences(
  taxon = "Manorina melanocephala",
  lat = -33.7,
  lon = 151.3,
  radius_km = 5,
  filter = galah_filter(year >= 2010)
)
} # }
```
