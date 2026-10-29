# The catalog in use

Reports the bundle lookups answer from: its version, whether it was
shipped with the package or downloaded by
[`update_catalog()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/update_catalog.md),
and the apb2 version whose rules it carries.

## Usage

``` r
catalog_version()
```

## Value

A list with `version`, `origin` (`"shipped"` or `"cache"`), `path` and
`apb2_version`.

## Examples

``` r
catalog_version()
#> $version
#> [1] "0.1.1"
#> 
#> $origin
#> [1] "shipped"
#> 
#> $path
#> [1] "/home/runner/work/_temp/Library/apbCatalogR/extdata/bundle"
#> 
#> $apb2_version
#> [1] "0.1.1"
#> 
```
