# Get basemap tiles providers

Get basemap tiles providers and the credits (attribution text).
`tmap_providers()` returns a list (or vector) with provider nams (or
credits). `tmap_provider_credits()`

## Usage

``` r
.tmap_providers

tmap_provider_credits(provider)

tmap_providers(mode, credits = FALSE, as.list = credits)
```

## Format

An object of class `environment` of length 44.

## Arguments

- provider:

  provider name

- mode:

  mode. If not specified the default mode is used

- credits:

  If `TRUE` the credit (attribution) text is returned. If `FALSE`
  (default) the provider name.

- as.list:

  Should the output be returned as list where names are provider names?
  By default `TRUE` when `credits` is also `TRUE`.

## Value

list or vector (see `as.list`) with providers (or credits).
`tmap_provider_credits()` returns the credits text for the provided
provider.
