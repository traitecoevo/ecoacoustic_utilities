#' Check if a remote API is reachable
#'
#' Sends a lightweight probe request and retries a few times before giving up,
#' so that a single slow round-trip or transient network blip does not cause a
#' false "unreachable" result (which would make the downloaders bail early and
#' silently return 0).
#'
#' @param api_type Character. One of "inat", "ala", or "xc".
#' @param timeout_secs Numeric. Seconds to wait per attempt before timing out.
#'   Default is 10.
#' @param n_tries Integer. Number of attempts before declaring the API
#'   unreachable. Default is 3, with a short pause between attempts.
#'
#' @return Logical. TRUE if reachable, FALSE otherwise.
#' @importFrom httr GET status_code timeout
#' @keywords internal
is_api_reachable <- function(api_type = c("inat", "ala", "xc"),
                             timeout_secs = 10, n_tries = 3) {
    api_type <- match.arg(api_type)

    url <- if (api_type == "inat") {
        "https://api.inaturalist.org/v1/observations"
    } else if (api_type == "xc") {
        "https://xeno-canto.org/api/3/recordings"
    } else {
        "https://ala.org.au"
    }

    # Light probe request. XC v3 only accepts tag-based queries, so use a valid
    # tag rather than a bare term (a bare term returns a 400 client error).
    query <- if (api_type == "inat") {
        list(per_page = 0)
    } else if (api_type == "xc") {
        list(query = 'grp:"birds"', page = 1)
    } else {
        list()
    }

    for (attempt in seq_len(n_tries)) {
        reachable <- tryCatch(
            {
                resp <- httr::GET(url, query = query, httr::timeout(timeout_secs))
                # Reachable if the server answered with anything but a 5xx error.
                httr::status_code(resp) < 500
            },
            error = function(e) FALSE
        )
        if (reachable) {
            return(TRUE)
        }
        if (attempt < n_tries) Sys.sleep(1)
    }

    return(FALSE)
}

#' Skip test if API is unreachable
#'
#' Helper for testthat to elegantly skip integration tests when APIs are down.
#'
#' @param api_type Character. One of "inat", "ala", or "xc".
#' @keywords internal
skip_if_api_unavailable <- function(api_type) {
    if (!is_api_reachable(api_type)) {
        testthat::skip(paste(api_type, "API is currently unreachable"))
    }
}
