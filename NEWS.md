# apbCatalogR 0.99.0

* First version: apb-catalog's API in R on the apb-catalog bundle: the R6 classes `Catalog`, `DataCatalog` and `Reference`, `catalogues()`, `rule_coverage()`, `catalogue_table()` and the conditions `UnresolvedField`, `StaleField` and `MissingRequiredField`.
* `read_parsed_levels()` reads an APB Parquet result into `ParsedLevels`, one anndataR `AnnData` per level.
* `update_catalog()` downloads the bundle of an apb-catalog release; `catalog_version()` reports the bundle in use.
* Snapshots are not ported: `DataCatalog$snapshot()`, `$resolve()`, `attach_snapshot()`, `stored_snapshot()`, `stale_levels()` and `ResolutionSnapshot` have no R counterpart.
