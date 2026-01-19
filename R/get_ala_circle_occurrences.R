#' Download ALA Occurrences for a Taxon Within a Circular Area
#'
#' Retrieves occurrence records from the Atlas of Living Australia (ALA) for a
#' specified taxon within a circular geographic area defined by a center point
#' and radius.
#'
#' @param taxon Character. Scientific or common name that ALA can resolve.
#' @param lat Numeric. Latitude of circle centre in decimal degrees (WGS84).
#' @param lon Numeric. Longitude of circle centre in decimal degrees (WGS84).
#' @param radius_km Numeric. Radius in kilometres. Default is 10 km.
#' @param email Character. ALA/GBIF login email. If NULL, assumes you've already
#'   called \code{galah_config()} elsewhere. Default is NULL.
#' @param ... Additional arguments passed to \code{atlas_occurrences()}, such as
#'   \code{filter = galah_filter(year >= 2000)}.
#'
#' @return A tibble of occurrence records from the ALA.
#'
#' @examples
#' \dontrun{
#' # Get all bird records within 10 km of a point
#' birds <- get_ala_circle_occurrences(
#'   taxon = "Aves",
#'   lat = -33.7,
#'   lon = 151.3,
#'   radius_km = 10,
#'   email = "your.email@example.com"
#' )
#'
#' # With additional filters
#' recent_birds <- get_ala_circle_occurrences(
#'   taxon = "Manorina melanocephala",
#'   lat = -33.7,
#'   lon = 151.3,
#'   radius_km = 5,
#'   filter = galah_filter(year >= 2010)
#' )
#' }
#'
#' @export
#' @importFrom galah galah_config galah_call galah_identify galah_geolocate atlas_occurrences
get_ala_circle_occurrences <- function(taxon,
                                       lat,
                                       lon,
                                       radius_km = 10,
                                       email = NULL,
                                       ...) {
  # Optionally set config here; skip if user has already done it
  if (!is.null(email)) {
    galah_config(email = email, atlas = "Australia")
  }
  
  galah_call() |>
    galah_identify(taxon) |>
    galah_geolocate(
      lat    = lat,
      lon    = lon,
      radius = radius_km,
      type   = "radius"
    ) |>
    atlas_occurrences(...)
}
