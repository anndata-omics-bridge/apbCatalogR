# inst/extdata

Both folders come from [apb-catalog](https://github.com/anndata-omics-bridge/apb-catalog) and are copied unchanged. `make bundle` in apb-catalog (`scripts/build_bundle.py`) writes them to its `build/apb-catalog-bundle/` and `build/apb-catalog-test-bundle/`. They are never edited by hand; a new apb-catalog version replaces both.

Current copy: apb-catalog 0.1.4, commit `718bee9` (10 Nov 2026), built with apb2 commit `cb7a522`, with the rule levels of apb2 0.1.1. Each folder's `manifest.json` records these versions and lists its files.

## bundle/

What the package looks terms up in (188 KB). It holds the catalogue tables of apb-catalog as CSV files in `catalogues/`, with columns described in `catalogues/datapackage.json` (a Frictionless Data Package):

- `sets.csv`: one row per catalogue set, its purpose and its users
- `terms.csv`: the terms each set defines, before any software maps them
- `kinds.csv`, `qualifiers.csv`, `qualifier_values.csv`: what a term stands for, and the qualifiers, such as normalization or unit, that refine it
- `catalog.csv`: where each term's field lives in an APB result, per quantification tool and version
- `producers.csv`: software that adds fields after conversion, such as apb-fasta
- `versions.csv`: the software versions apb2 has a parsing rule for

## test-bundle/

The answers the Python implementation gives, which `tests/testthat/test-cases.R` requires the R package to repeat (4.2 MB). It contains no measured data.

- `results.tar.gz`: 48 synthetic APB results, each with two samples and three features, in Parquet and DuckDB, and the 47 with one level also as h5ad; apb2's `.apb.json` record sits beside each. Forty-two are one per apb2 rule level, with every field the rule level keeps, and six are edge cases (two levels in one result, two with FASTA provenance, an unreviewed rule, SDRF columns, a prolfquapp design). Packed as one archive because their file paths exceed the 100 bytes R packages allow
- `cases/catalog.json`, `cases/data_catalog.json`, `cases/tables.json`: the expected answers of `Catalog`, `DataCatalog` and the catalogue tables for every request and result
