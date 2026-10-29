# What one result's fields mean to one consumer

What one result's fields mean to one consumer

What one result's fields mean to one consumer

## Value

The class generator; `DataCatalog$new()` returns a `DataCatalog`.

## Details

Answers on a result read with
[`read_parsed_levels()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/read_parsed_levels.md)
and hands the data back directly. A lookup of a field the result's
stored rule declares differently from the catalog's rule signals
`StaleField`; ambiguous or unreviewed answers signal `UnresolvedField`.

## Active bindings

- `description`:

  What this catalogue set holds and who uses it.

## Methods

### Public methods

- [`DataCatalog$new()`](#method-DataCatalog-new)

- [`DataCatalog$kinds()`](#method-DataCatalog-kinds)

- [`DataCatalog$layer()`](#method-DataCatalog-layer)

- [`DataCatalog$var()`](#method-DataCatalog-var)

- [`DataCatalog$obs()`](#method-DataCatalog-obs)

- [`DataCatalog$fields()`](#method-DataCatalog-fields)

- [`DataCatalog$print()`](#method-DataCatalog-print)

- [`DataCatalog$clone()`](#method-DataCatalog-clone)

------------------------------------------------------------------------

### Method `new()`

Bind a catalogue set to the levels of one result.

#### Usage

    DataCatalog$new(result, catalog, enforce = NULL)

#### Arguments

- `result`:

  A
  [ParsedLevels](https://anndata-omics-bridge.github.io/apbCatalogR/reference/ParsedLevels.md)
  object.

- `catalog`:

  The catalogue set, such as `"confidence"`; see
  [`catalogues()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/catalogues.md).

- `enforce`:

  `NULL`, `"MUST"` or `"SHOULD"`: signal `MissingRequiredField` when a
  field the set requires at that strength is missing.

------------------------------------------------------------------------

### Method `kinds()`

The kinds the level offers at one location, given the other qualifiers.

#### Usage

    DataCatalog$kinds(level, location, ...)

#### Arguments

- `level`:

  The quantification level.

- `location`:

  `"layers"`, `"var"` or `"obs"`.

- `...`:

  Other qualifiers by name.

#### Returns

A character vector.

------------------------------------------------------------------------

### Method `layer()`

The layer with this meaning.

#### Usage

    DataCatalog$layer(level, kind, ...)

#### Arguments

- `level`:

  The quantification level.

- `kind`:

  The kind, such as `"pep"`.

- `...`:

  Other qualifiers by name.

#### Returns

An observations-by-features matrix, or `NULL` when the result has no
such layer.

------------------------------------------------------------------------

### Method [`var()`](https://rdrr.io/r/stats/cor.html)

The per-feature column with this meaning, from `var` or a `varm` table.

#### Usage

    DataCatalog$var(level, kind, ...)

#### Arguments

- `level`:

  The quantification level.

- `kind`:

  The kind.

- `...`:

  Other qualifiers by name.

#### Returns

A vector, or `NULL`.

------------------------------------------------------------------------

### Method `obs()`

The per-sample column with this meaning.

#### Usage

    DataCatalog$obs(level, kind, ...)

#### Arguments

- `level`:

  The quantification level.

- `kind`:

  The kind.

- `...`:

  Other qualifiers by name.

#### Returns

A vector, or `NULL`.

------------------------------------------------------------------------

### Method `fields()`

Every catalogued field of the result, one row per field, as
[`catalogue_table()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/catalogue_table.md)
shows them.

#### Usage

    DataCatalog$fields()

#### Returns

A data frame with `present` and, for a field apb2 left out because it
held one value, that `constant`.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the set and the levels.

#### Usage

    DataCatalog$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DataCatalog$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
results <- tempfile()
archive <- system.file("extdata", "bundle", "results.tar.gz", package = "apbCatalogR")
utils::untar(archive, exdir = results)
result <- read_parsed_levels(file.path(results, "maxquant_wide-peptidoform_ion.parquet"))
catalog <- DataCatalog$new(result, "confidence")
dim(catalog$layer("peptidoform_ion", kind = "pep"))
#> [1] 2 3
```
