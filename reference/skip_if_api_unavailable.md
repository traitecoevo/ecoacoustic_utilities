# Skip test if API is unreachable

Helper for testthat to elegantly skip integration tests when APIs are
down.

## Usage

``` r
skip_if_api_unavailable(api_type)
```

## Arguments

- api_type:

  Character. "inat" or "ala".
