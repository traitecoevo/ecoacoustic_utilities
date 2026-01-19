#' Download ALA occurrences for a taxon within a circular area
#'
#' @param taxon      Character. Scientific or common name that ALA can resolve.
#' @param lat        Numeric. Latitude of circle centre (WGS84).
#' @param lon        Numeric. Longitude of circle centre (WGS84).
#' @param radius_km  Numeric. Radius in kilometres (default 10 km).
#' @param email      Character. ALA/GBIF login email; if NULL assumes you've
#'                   already called galah_config() elsewhere.
#' @param ...        Extra arguments passed to atlas_occurrences()
#'                   (e.g. filter = galah_filter(year >= 2000)).
#'
#' @return A tibble of occurrence records.
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
library(galah)

# one-off: set your email once per session, or pass email= in the function
galah_config(email = "wcornwell@gmail.com", atlas = "Australia")

# all records of Manorina melanocephala within 5 km of a point
bod_birds <- get_ala_circle_occurrences(
  taxon      = "Aves",
  lat        = -32.543,
  lon        = 149.0145,
  radius_km  = 10
)

# with extra filters (e.g. year)
recent_m_mel <- get_ala_circle_occurrences(
  taxon = "Manorina melanocephala",
  lat   = -33.7,
  lon   = 151.3,
  radius_km = 5,
  filter = galah_filter(year >= 2010)
)

library(dplyr)
library(ggplot2)

threshold <- 0.015  # example: collapse species with total < 50 records

bod_birds %>%
  filter(dataResourceName %in% c("eBird Australia",
                                 "NSW BioNet Atlas",
                                 "BirdLife Australia, Birdata")) %>%
  group_by(dataResourceName) %>%
  mutate(total_resourse_obs = n()) %>%
  group_by(dataResourceName,scientificName) %>%
  summarise(relative_obs = n()/mean(total_resourse_obs)) ->sp_summary

sp_summary %>%
  group_by(scientificName) %>%
  mutate(mm=max(relative_obs)) %>%
  mutate(
    species_collapsed = if_else(
      mm < threshold,
      "Other",
      scientificName
    ) )%>%
  ungroup() %>%
  ggplot(aes(x = dataResourceName,
             y = relative_obs,
             fill = species_collapsed)) +
  geom_col(position = "stack") +
  labs(
    x = "Data source",
    y = "Number of records",
    fill = "Species"
  ) +
  theme_minimal()


