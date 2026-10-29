# Which column one software version reports for each meaning

Which column one software version reports for each meaning

Which column one software version reports for each meaning

## Value

The class generator; `Catalog$new()` returns a `Catalog`.

## Details

Answers from the rules alone, without data. The rule of the software
whose version pattern admits the version decides which fields each level
has. Lookups that several fields answer, or that an unreviewed level
cannot, signal `UnresolvedField`.

## Active bindings

- `description`:

  What this catalogue set holds and who uses it.

## Methods

### Public methods

- [`Catalog$new()`](#method-Catalog-new)

- [`Catalog$kinds()`](#method-Catalog-kinds)

- [`Catalog$layer()`](#method-Catalog-layer)

- [`Catalog$var()`](#method-Catalog-var)

- [`Catalog$obs()`](#method-Catalog-obs)

- [`Catalog$fields()`](#method-Catalog-fields)

- [`Catalog$print()`](#method-Catalog-print)

- [`Catalog$clone()`](#method-Catalog-clone)

------------------------------------------------------------------------

### Method `new()`

Bind a catalogue set to the rule levels of one software version.

#### Usage

    Catalog$new(software, version, catalog, enforce = NULL)

#### Arguments

- `software`:

  The software name, such as `"DIA-NN"`.

- `version`:

  The software version, such as `"2.2.0"`.

- `catalog`:

  The catalogue set, such as `"abundance"` or `"confidence"`; see
  [`catalogues()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/catalogues.md).

- `enforce`:

  `NULL`, `"MUST"` or `"SHOULD"`: signal `MissingRequiredField` when a
  field the set requires at that strength is missing.

------------------------------------------------------------------------

### Method `kinds()`

The kinds the level offers at one location, given the other qualifiers.

#### Usage

    Catalog$kinds(level, location, ...)

#### Arguments

- `level`:

  The quantification level.

- `location`:

  `"layers"`, `"var"` or `"obs"`.

- `...`:

  Other qualifiers by name, such as `normalization = "none"`.

#### Returns

A character vector.

------------------------------------------------------------------------

### Method `layer()`

Where the sample-by-feature layer with this meaning lives.

#### Usage

    Catalog$layer(level, kind, ...)

#### Arguments

- `level`:

  The quantification level.

- `kind`:

  The kind, such as `"intensity"`.

- `...`:

  Other qualifiers by name.

#### Returns

A
[Reference](https://anndata-omics-bridge.github.io/apbCatalogR/reference/Reference.md),
or `NULL` when the level has no such layer.

------------------------------------------------------------------------

### Method [`var()`](https://rdrr.io/r/stats/cor.html)

Where the per-feature column with this meaning lives, in `var` or a
`varm` table.

#### Usage

    Catalog$var(level, kind, ...)

#### Arguments

- `level`:

  The quantification level.

- `kind`:

  The kind.

- `...`:

  Other qualifiers by name.

#### Returns

A
[Reference](https://anndata-omics-bridge.github.io/apbCatalogR/reference/Reference.md),
or `NULL`.

------------------------------------------------------------------------

### Method `obs()`

Where the per-sample column with this meaning lives.

#### Usage

    Catalog$obs(level, kind, ...)

#### Arguments

- `level`:

  The quantification level.

- `kind`:

  The kind.

- `...`:

  Other qualifiers by name.

#### Returns

A
[Reference](https://anndata-omics-bridge.github.io/apbCatalogR/reference/Reference.md),
or `NULL`.

------------------------------------------------------------------------

### Method `fields()`

Every catalogued field the rule declares, one row per field, as
[`catalogue_table()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/catalogue_table.md)
shows them.

#### Usage

    Catalog$fields()

#### Returns

A data frame, with `present` true for each row.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the set and the levels.

#### Usage

    Catalog$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Catalog$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
catalog <- Catalog$new("DIA-NN", "2.2.0", "abundance")
catalog$layer("peptidoform_ion", kind = "intensity", normalization = "none")
#> <Reference peptidoform_ion layers: Precursor_Quantity>
catalog$kinds("peptidoform_ion", "layers")
#> [1] "intensity"     "ms1_intensity"
```
