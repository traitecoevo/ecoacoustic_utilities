#' Check if a remote API is reachable
#'
#' @param api_type Character. Either "inat" or "ala".
#' @param timeout_secs Numeric. Seconds to wait before timing out. Default is 2.
#'
#' @return Logical. TRUE if reachable, FALSE otherwise.
#' @importFrom httr GET status_code timeout
#' @keywords internal
is_api_reachable <- function(api_type = c("inat", "ala"), timeout_secs = 2) {
    api_type <- match.arg(api_type)

    url <- if (api_type == "inat") {
        "https://api.inaturalist.org/v1/observations"
    } else {
        "https://ala.org.au"
    }

    reachable <- tryCatch(
        {
            # We use a very light request (HEAD or per_page=0) if possible
            query <- if (api_type == "inat") list(per_page = 0) else list()
            resp <- httr::GET(url, query = query, httr::timeout(timeout_secs))
            httr::status_code(resp) < 500 # Consider it reachable if not a server error
        },
        error = function(e) {
            FALSE
        }
    )

    return(reachable)
}

#' Skip test if API is unreachable
#'
#' Helper for testthat to elegantly skip integration tests when APIs are down.
#'
#' @param api_type Character. "inat" or "ala".
#' @keywords internal
skip_if_api_unavailable <- function(api_type) {
    if (!is_api_reachable(api_type)) {
        testthat::skip(paste(api_type, "API is currently unreachable"))
    }
}
