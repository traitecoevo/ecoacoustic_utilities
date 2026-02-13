# Resolve R CMD check notes for global variables used in tidy evaluation or NSE
if (getRversion() >= "2.15.1") {
    utils::globalVariables(
        c(
            "media_id", # used in get_ala_sounds
            "n_recordings", # used in plot_inat_species_summary
            "display_name", # used in plot_inat_species_summary
            "multimedia", # used in get_ala_sounds
            "recordID", # used in get_ala_sounds
            "scientificName" # used in get_ala_sounds
        )
    )
}
