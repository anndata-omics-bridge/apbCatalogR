# The newest readable bundle is used: shipped with the package or downloaded into the user cache.

# A copy of the shipped bundle claiming another version and format, packed as a release asset.
bundle_archive <- function(version, format = 1L) {
  root <- withr::local_tempdir(.local_envir = parent.frame())
  folder <- file.path(root, "apb-catalog-bundle")
  dir.create(folder)
  file.copy(list.files(bundle_file(), full.names = TRUE), folder, recursive = TRUE)
  manifest <- jsonlite::fromJSON(file.path(folder, "manifest.json"), simplifyVector = FALSE)
  manifest$version <- version
  manifest$bundle_format <- format
  jsonlite::write_json(manifest, file.path(folder, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  archive <- file.path(root, "apb-catalog-bundle.tar.gz")
  withr::with_dir(root, utils::tar(archive, "apb-catalog-bundle", compression = "gzip"))
  archive
}

local_cache <- function(envir = parent.frame()) {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir(.local_envir = envir), .local_envir = envir)
  apbCatalogR:::reset_bundle()
  withr::defer(apbCatalogR:::reset_bundle(), envir = envir)
}

test_that("without a download the shipped bundle answers", {
  local_cache()
  expect_identical(catalog_version()$origin, "shipped")
})

test_that("a newer downloaded bundle wins over the shipped one", {
  local_cache()
  apbCatalogR:::install_bundle(bundle_archive("999.0.0"))
  expect_identical(catalog_version()[c("version", "origin")], list(version = "999.0.0", origin = "cache"))
})

test_that("an older downloaded bundle does not win over a newer shipped one", {
  local_cache()
  apbCatalogR:::install_bundle(bundle_archive("0.0.1"))
  expect_identical(catalog_version()$origin, "shipped")
})

test_that("a bundle of a newer format is refused and the next one is used", {
  local_cache()
  newer <- bundle_archive("999.0.0", format = 99L)
  expect_error(apbCatalogR:::install_bundle(newer), "update apbCatalogR")
  unpacked <- file.path(apbCatalogR:::cache_root(), "999.0.0")
  dir.create(unpacked, recursive = TRUE)
  utils::untar(newer, exdir = unpacked)
  file.copy(list.files(file.path(unpacked, "apb-catalog-bundle"), full.names = TRUE), unpacked, recursive = TRUE)
  apbCatalogR:::reset_bundle()
  expect_warning(version <- catalog_version(), "update apbCatalogR")
  expect_identical(version$origin, "shipped")
})
