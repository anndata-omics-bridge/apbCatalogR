# apbCatalogR

apb-catalog's API in R: look up the fields of an [APB](https://github.com/anndata-omics-bridge/apb2) result by meaning, such as a PEP or an unnormalized intensity, without naming vendor columns.

Documentation: [anndata-omics-bridge.github.io/apbCatalogR](https://anndata-omics-bridge.github.io/apbCatalogR/), with the [vignette](https://anndata-omics-bridge.github.io/apbCatalogR/vignettes/apbCatalogR.html) as the walkthrough.

## APB

The AnnData Proteomics Bridge, [apb2](https://anndata-omics-bridge.github.io/apb2/), converts the reports of proteomics quantification software into one result layout. Declarative parse rules describe each vendor table, so one parser reads them all.

- Software: AlphaDIA, DIA-NN, FragPipe, MaxQuant, PEAKS, Sage, Spectronaut and [more](https://anndata-omics-bridge.github.io/apb2/supported_software/)
- Levels: peptidoform ion, peptidoform, peptide, protein, peptidoform fragment
- Per level: one AnnData, runs as `obs`, features as `var`, measurements as layers
- Formats: h5ad, h5mu, Parquet, DuckDB

Columns keep the vendor's names: the posterior error probability is `PEP` in MaxQuant and DIA-NN, `EG_PEP` in Spectronaut.

## apb-catalog

[apb-catalog](https://anndata-omics-bridge.github.io/apb-catalog/) records what those columns mean, in reviewed catalogue sets. A consumer asks a set for a kind, such as `pep` or `intensity`, and gets the column.

| Set | Fields |
| --- | --- |
| `abundance` | Intensities, and whether the software normalizes them across runs |
| `confidence` | PEP and q-values per run and per feature |
| `proteobench_entrapment` | Precursor q-values for ProteoBench's entrapment scoring |
| `apb` | Fields APB writes for every vendor: ProForma identifiers, decoy and contaminant flags, sample annotations |
| `chromatography` | Retention times, peak bounds and widths, with their unit |

A lookup answers one of three ways:

- One field: the column, or its data
- Software does not report it: `NULL`
- Several fields: an `UnresolvedField` error, never a silent choice

## This package

apbCatalogR is apb-catalog's Python API in R, as R6 classes with the same names and methods. It needs no Python: the rules and catalogues come from a bundle shipped with the package.

| Call | Answers |
| --- | --- |
| `Catalog$new(software, version, set)` | From the rules alone, without data |
| `DataCatalog$new(result, set)` | On one result, and returns the data |
| `catalogues()`, `rule_coverage()`, `catalogue_table()` | The sets, how each rule covers a set, every entry as a table |
| `read_parsed_levels(path)` | An APB Parquet result as `ParsedLevels`, one anndataR `AnnData` per level |

It does not convert vendor files. Convert with apb2, as Parquet:

```bash
apb2 convert DATA --params PARAMETER_FILE --format parquet --output result   # writes result.parquet
```

## Install

```r
install.packages(c("BiocManager", "remotes"))
BiocManager::install("anndata-omics-bridge/apbCatalogR")
```

## Use

Ask the rules which column a software version reports:

```r
library(apbCatalogR)
diann <- Catalog$new("DIA-NN", "2.2.0", "abundance")
diann$kinds("peptidoform_ion", "layers")
#> [1] "intensity"     "ms1_intensity"
diann$layer("peptidoform_ion", kind = "intensity", normalization = "none")
#> <Reference peptidoform_ion layers: Precursor_Quantity>
```

Get the data of a result:

```r
result <- read_parsed_levels("result.parquet")
confidence <- DataCatalog$new(result, "confidence")
pep <- confidence$layer("peptidoform_ion", kind = "pep")   # runs x features matrix
```

## The bundle

apb-catalog builds a bundle at each release:

- apb2's parse rules
- the reviewed catalogue sets
- synthetic results, with apb-catalog's answers on them; the tests check that R gives the same answers

How R gets the latest one:

- Shipped: `inst/extdata/bundle/`, replaced by a pull request from each apb-catalog release
- `update_catalog()`: downloads the latest release's bundle into the user cache
- Each session uses the newest bundle, shipped or downloaded; `catalog_version()` reports which
