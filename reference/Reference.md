# Where a field lives

Where a field lives

Where a field lives

## Value

The class generator; `Reference$new()` returns a `Reference`.

## Details

The answer of a
[Catalog](https://anndata-omics-bridge.github.io/apbCatalogR/reference/Catalog.md)
lookup: the level, the location (`"layers"`, `"var"`, `"varm"` or
`"obs"`), the column name and, for a `varm` field, its table.

## Public fields

- `level`:

  The quantification level, such as `"peptidoform_ion"`.

- `location`:

  `"layers"`, `"var"`, `"varm"` or `"obs"`.

- `name`:

  The layer or column name.

- `table`:

  The `varm` table, `NA` elsewhere.

## Methods

### Public methods

- [`Reference$new()`](#method-Reference-new)

- [`Reference$print()`](#method-Reference-print)

- [`Reference$clone()`](#method-Reference-clone)

------------------------------------------------------------------------

### Method `new()`

Where one field lives.

#### Usage

    Reference$new(level, location, name, table = NA_character_)

#### Arguments

- `level`:

  The quantification level.

- `location`:

  `"layers"`, `"var"`, `"varm"` or `"obs"`.

- `name`:

  The layer or column name.

- `table`:

  The `varm` table, `NA` elsewhere.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the reference.

#### Usage

    Reference$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Reference$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
Reference$new("peptidoform_ion", "layers", "PEP")
#> <Reference peptidoform_ion layers: PEP>
```
