# Check if a remote API is reachable

Sends a lightweight probe request and retries a few times before giving
up, so that a single slow round-trip or transient network blip does not
cause a false "unreachable" result (which would make the downloaders
bail early and silently return 0).

## Usage

``` r
is_api_reachable(
  api_type = c("inat", "ala", "xc"),
  timeout_secs = 10,
  n_tries = 3
)
```

## Arguments

- api_type:

  Character. One of "inat", "ala", or "xc".

- timeout_secs:

  Numeric. Seconds to wait per attempt before timing out. Default is 10.

- n_tries:

  Integer. Number of attempts before declaring the API unreachable.
  Default is 3, with a short pause between attempts.

## Value

Logical. TRUE if reachable, FALSE otherwise.
