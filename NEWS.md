# apbCatalogR 0.99.0

* First version: apb-catalog's API in R on the apb-catalog bundle: the R6 classes `Catalog`, `DataCatalog` and `Reference`, `catalogues()`, `rule_coverage()`, `catalogue_table()` and the conditions `UnresolvedField`, `StaleField` and `MissingRequiredField`.
* `read_parsed_levels()` reads an APB Parquet result into `ParsedLevels`, one anndataR `AnnData` per level. It also reads apb2's `.h5ad` and `.duckdb` results into the same `ParsedLevels`; Parquet needs the suggested `arrow` package and DuckDB the suggested `duckdb` and `DBI` packages; `.h5ad` needs only anndataR 1.2.2 or later, the first to read the text columns apb2 writes.
* The bundle is apb-catalog 0.1.4: the `abundance` set names normalization in the kind, `intensity`, `normalized_intensity` or `intensity_unknown_normalization` (likewise for MS1), as PSI-MS names raw and normalized values apart; its `normalization` qualifier is gone. A level has at most one layer per kind.
* `update_catalog()` downloads the bundle of an apb-catalog release; `catalog_version()` reports the bundle in use.
* The bundle holds only the rules and catalogue sets; the synthetic results and expected answers ship as a separate test bundle in `inst/extdata/test-bundle/`.
* Bundle format 2: the catalogue sets are five CSV tables (`entries`, `entry_qualifiers`, `vocabulary`, `requirements`, `producers`) described by `datapackage.json`; a vendor catalogue reviews every rule level the bundle's rules declare, and every qualifier lists `unknown` as a value. Bundles of format 1 are passed over.
* Snapshots are not ported: `DataCatalog$snapshot()`, `$resolve()`, `attach_snapshot()`, `stored_snapshot()`, `stale_levels()` and `ResolutionSnapshot` have no R counterpart.
