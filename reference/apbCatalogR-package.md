# apbCatalogR: look up APB result fields by meaning

The R implementation of apb-catalog. It works on the bundle apb-catalog
releases: apb2's effective parse rules, the reviewed catalogue sets and
the answers apb-catalog gives on synthetic results.
[Catalog](https://anndata-omics-bridge.github.io/apbCatalogR/reference/Catalog.md)
answers which column one software version reports for a meaning, without
data;
[DataCatalog](https://anndata-omics-bridge.github.io/apbCatalogR/reference/DataCatalog.md)
answers on a result read with
[`read_parsed_levels()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/read_parsed_levels.md).

## Details

The package ships a bundle;
[`update_catalog()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/update_catalog.md)
downloads a newer one and
[`catalog_version()`](https://anndata-omics-bridge.github.io/apbCatalogR/reference/catalog_version.md)
reports the one in use.

## Conditions

Lookups signal conditions of the classes apb-catalog raises in Python:

- `UnresolvedField`: the catalogue cannot answer with one field; the
  request is ambiguous or unknown. Its `resolution` holds the candidates
  and reasons.

- `StaleField`: the result's stored rule declares the field differently
  from the bundle's rule.

- `MissingRequiredField`: a field the set requires is missing, under
  `enforce`. Signalled as an error, or as a warning for a `SHOULD` field
  when `enforce = "MUST"`.

Catch them with [`tryCatch()`](https://rdrr.io/r/base/conditions.html),
for example `tryCatch(..., UnresolvedField = function(condition) NULL)`.

## See also

Useful links:

- <https://github.com/anndata-omics-bridge/apbCatalogR>

- Report bugs at
  <https://github.com/anndata-omics-bridge/apbCatalogR/issues>

## Author

**Maintainer**: Witold Wolski <wew@fgcz.ethz.ch>
([ORCID](https://orcid.org/0000-0002-6468-120X))
