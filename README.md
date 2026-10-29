# apbCatalogR

apb-catalog's API in R: look up the fields of an [APB](https://github.com/anndata-omics-bridge/apb2) result by meaning, such as a PEP or an unnormalized intensity, without naming vendor columns.

The package works on the bundle [apb-catalog](https://anndata-omics-bridge.github.io/apb-catalog/) releases: apb2's parse rules, the reviewed catalogue sets and the answers apb-catalog gives on synthetic results. It reads APB Parquet results into anndataR objects; it does not convert vendor files.

```r
library(apbCatalogR)
catalog <- Catalog$new("DIA-NN", "2.2.0", "abundance")
catalog$layer("peptidoform_ion", kind = "intensity", normalization = "none")
```

`update_catalog()` downloads the bundle of the latest apb-catalog release; `catalog_version()` reports the one in use.
