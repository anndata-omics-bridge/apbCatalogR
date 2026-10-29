# Download a newer catalog

Downloads the bundle of an apb-catalog release from GitHub into the user
cache, so lookups use it without a package update. Later sessions keep
using the newest bundle, shipped or downloaded.

## Usage

``` r
update_catalog(version = NULL)
```

## Arguments

- version:

  The apb-catalog release, such as `"0.2.0"`; `NULL` downloads the
  latest release.

## Value

The
[`catalog_version()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/catalog_version.md)
now in use, invisibly.

## Examples

``` r
if (interactive()) {
  update_catalog()
}
```
