# apbCatalogR — agent rules

The closest `AGENTS.md` wins. Explicit user instructions override this file. The workspace [AGENTS.md](../AGENTS.md) applies where this file is silent.

## What this package is

apb-catalog's API in R, on the bundle apb-catalog releases. It reimplements the catalog, never apb2's conversion. The plan is the workspace's language-bundle TODO, TODO_language_bundle.md under the apb-catalog planning folder.

- `inst/extdata/bundle/` is apb-catalog's bundle and `inst/extdata/test-bundle/` its test bundle, both unchanged; a release of apb-catalog replaces them by pull request. Never edit them by hand: change apb-catalog and rebuild with `make bundle` there
- `tests/testthat/test-cases.R` runs every answer the test bundle records; a change that makes R answer differently from Python is a bug in R, or a change that belongs in apb-catalog first
- Each R file mirrors one apb-catalog module: `source.R`, `rules.R`, `resolver.R`, `catalog.R`, `coverage.R`, `table.R`

## Conventions

Set up with `usethis`; everything else follows prolfquapp and the R package rules of prolfqua_fml's AGENTS.md, except that the vignette uses `BiocStyle`, not `fgczQuartoTemplate`.

- R6 classes, same API as apb-catalog's class-based Python API: `Catalog$new()`, `DataCatalog$new()`, `Reference$new()`, no wrapper functions; snapshots are not ported, so `DataCatalog$snapshot()`, `$resolve()`, `attach_snapshot()`, `stored_snapshot()`, `stale_levels()` and `ResolutionSnapshot` have no R counterpart
- Bioconductor-compatible: dependencies from CRAN or Bioconductor only, no `Remotes:`; no network in tests or the vignette
- roxygen2 writes `NAMESPACE`; `NEWS.md` gets a bullet for every user-visible change
- 120 characters, 2 spaces (`air.toml`, `.lintr`)

## Commands

| Task | Command |
| --- | --- |
| Document | `make document` |
| Tests | `make test` |
| Full check | `make check` |
| BiocCheck | `make check-bioc` |
| Lint | `make lint` |
| Format | `make format` |
| Site (altdoc, as prolfquapp) | `make site` |
