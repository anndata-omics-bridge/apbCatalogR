# apbCatalogR — agent rules

The closest `AGENTS.md` wins. Explicit user instructions override this file. The workspace [AGENTS.md](../AGENTS.md) applies where this file is silent.

## What this package is

apb-catalog's API in R, on the bundle apb-catalog releases. It reimplements the catalog, never apb2's conversion. Open work is in the workspace's TODO_bioconductor_submission.md under the apbCatalogR planning folder.

- `inst/extdata/bundle/` is apb-catalog's bundle and `inst/extdata/test-bundle/` its test bundle, both unchanged; a release of apb-catalog replaces them by pull request. Never edit them by hand: change apb-catalog and rebuild with `make bundle` there
- `tests/testthat/test-cases.R` runs every answer the test bundle records; a change that makes R answer differently from Python is a bug in R, or a change that belongs in apb-catalog first
- Each R file mirrors one apb-catalog module: `source.R`, `rules.R`, `resolver.R`, `catalog.R`, `coverage.R`, `table.R`

## Conventions

Set up with `usethis`; everything else follows prolfquapp and the R package rules of prolfqua_fml's AGENTS.md, except that the vignette uses `BiocStyle`, not `fgczQuartoTemplate`.

- R6 classes, same API as apb-catalog's class-based Python API: `Catalog$new()`, `DataCatalog$new()`, lookups through `levels`, no wrapper functions; snapshots are not ported, so `DataCatalog$snapshot()`, `$resolve()`, `attach_snapshot()` and `ResolutionSnapshot` have no R counterpart
- Bioconductor-compatible: dependencies from CRAN or Bioconductor only, no `Remotes:`; no network in tests or the vignette
- Base R only: no dplyr, tidyr, rlang or other tidyverse import, even though the catalog ships as tables; their fixed cost per call made lookups on tables of a few rows 20 times slower. Code that builds a value uses `lapply()`, `vapply()`, `Map()`, `Filter()`, `Find()` or `Reduce()`, not a `for` loop
- roxygen2 writes `NAMESPACE`; `NEWS.md` gets a bullet for every user-visible change
- 80 characters, 4 spaces, as BiocCheck asks (`air.toml`, `.lintr`); the vignette's prose is wrapped at 80 too, since BiocCheck counts every line of it
- `make check-bioc` runs BiocCheck on the built tarball, as Bioconductor's builder does, so local site folders such as `_quarto/` are not checked; use BiocCheck devel (`BiocManager::install("Bioconductor/BiocCheck@devel")`), as CI does: release 1.48 misses roxygen2 8's `Config/roxygen2/version` and style-checks `man/`

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
