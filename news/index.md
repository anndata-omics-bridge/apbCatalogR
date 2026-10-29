# Changelog

## apbCatalogR 0.99.0

- First version: apb-catalog’s API in R on the apb-catalog bundle: the
  R6 classes `Catalog`, `DataCatalog` and `Reference`,
  [`catalogues()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/catalogues.md),
  [`rule_coverage()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/rule_coverage.md),
  [`catalogue_table()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/catalogue_table.md)
  and the conditions `UnresolvedField`, `StaleField` and
  `MissingRequiredField`.
- [`read_parsed_levels()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/read_parsed_levels.md)
  reads an APB Parquet result into `ParsedLevels`, one anndataR
  `AnnData` per level.
- [`update_catalog()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/update_catalog.md)
  downloads the bundle of an apb-catalog release;
  [`catalog_version()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/catalog_version.md)
  reports the bundle in use.
