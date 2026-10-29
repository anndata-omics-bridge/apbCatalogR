# One APB result: its levels on one observation axis

One APB result: its levels on one observation axis

One APB result: its levels on one observation axis

## Value

The class generator; `ParsedLevels$new()` returns a `ParsedLevels`
object.

## Details

The R counterpart of apb2's `ParsedLevels`, as
[`read_parsed_levels()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/read_parsed_levels.md)
returns it.

Each level is an anndataR `AnnData` laid out as apb2's `.h5ad` writer
lays it out: the primary layer in `X`, the other layers in `layers`, obs
and var frames named by their key columns, and the level's APB part in
`uns$apb`: `parse` (with `provenance$rule_json`), `roles`, `storage` and
post-parse records.

## Public fields

- `levels`:

  A named list of anndataR `AnnData`, one per level.

- `uns`:

  The root parse record.

- `metadata`:

  The root post-parse records, such as `sdrf` or `fasta`.

- `hierarchy`:

  How the levels nest, or `NULL`.

- `annotation_tables`:

  A named list of data frames; each carries its metadata as the
  attribute `apb`.

- `feature_relations`:

  A named list of data frames of `source`, `row`, `target`, `column` and
  `value`; each carries its metadata as the attribute `apb`.

## Methods

### Public methods

- [`ParsedLevels$new()`](#method-ParsedLevels-new)

- [`ParsedLevels$print()`](#method-ParsedLevels-print)

- [`ParsedLevels$clone()`](#method-ParsedLevels-clone)

------------------------------------------------------------------------

### Method `new()`

Collect the levels and records of one result.

#### Usage

    ParsedLevels$new(
      levels,
      uns = list(),
      metadata = list(),
      hierarchy = NULL,
      annotation_tables = list(),
      feature_relations = list()
    )

#### Arguments

- `levels`:

  A named list of anndataR `AnnData`.

- `uns`:

  The root parse record.

- `metadata`:

  The root post-parse records.

- `hierarchy`:

  How the levels nest, or `NULL`.

- `annotation_tables`:

  A named list of data frames.

- `feature_relations`:

  A named list of data frames.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the levels and their shapes.

#### Usage

    ParsedLevels$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ParsedLevels$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
results <- tempfile()
archive <- system.file("extdata", "bundle", "results.tar.gz", package = "apbCatalogR")
utils::untar(archive, exdir = results)
result <- read_parsed_levels(file.path(results, "edge-two-levels.parquet"))
names(result$levels)
#> [1] "peptidoform_ion" "protein"        
result$levels$protein
#> InMemoryAnnData object with n_obs × n_vars = 2 × 3
#>     obs: 'Run', 'Run_Index'
#>     var: 'Protein_Group', 'Decoy', 'Protein_Names', 'Genes', 'Lib_PG_Q_Value', 'Global_PG_Q_Value', 'apb_Decoy', 'apb_Contaminant'
#>     uns: 'apb'
#>     layers: 'Genes_MaxLFQ', 'PG_Q_Value', 'PG_PEP', 'Genes_MaxLFQ_Empirical_Quality', 'Genes_MaxLFQ_Empirical_Unique_Quality', 'Genes_MaxLFQ_Quality', 'Genes_MaxLFQ_Unique', 'Genes_MaxLFQ_Unique_Quality', 'Genes_TopN', 'PG_MaxLFQ_Empirical_Quality', 'PG_MaxLFQ_Quality', 'PG_TopN'
```
