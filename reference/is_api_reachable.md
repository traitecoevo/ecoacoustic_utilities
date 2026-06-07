# Check if a remote API is reachable

Check if a remote API is reachable

## Usage

``` r
is_api_reachable(api_type = c("inat", "ala", "xc"), timeout_secs = 2)
```

## Arguments

- api_type:

  Character. One of "inat", "ala", or "xc".

- timeout_secs:

  Numeric. Seconds to wait before timing out. Default is 2.

## Value

Logical. TRUE if reachable, FALSE otherwise.
