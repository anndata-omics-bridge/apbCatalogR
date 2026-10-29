# The bundle apb-catalog releases: rules, catalogue sets and expected answers. The package ships one; update_catalog()
# downloads newer ones into the user cache. The newest readable bundle is used.

BUNDLE_FORMAT <- 1L
BUNDLE_NAME <- "apb-catalog-bundle"
RELEASES <- "https://github.com/anndata-omics-bridge/apb-catalog/releases"

state <- new.env(parent = emptyenv())

Bundle <- R6::R6Class(
  "Bundle",
  public = list(
    dir = NULL,
    manifest = NULL,
    origin = NULL,

    initialize = function(dir, origin) {
      self$dir <- dir
      self$manifest <- read_json_file(file.path(dir, "manifest.json"))
      self$origin <- origin
      private$cache <- new.env(parent = emptyenv())
    },

    # One JSON file of the bundle, read once.
    json = function(...) {
      path <- file.path(...)
      if (is.null(private$cache[[path]])) {
        private$cache[[path]] <- read_json_file(file.path(self$dir, path))
      }
      private$cache[[path]]
    },

    # A value computed once per bundle.
    memo = function(key, compute) {
      if (is.null(private$cache[[key]])) {
        private$cache[[key]] <- compute()
      }
      private$cache[[key]]
    }
  ),
  private = list(cache = NULL)
)

cache_root <- function() {
  tools::R_user_dir("apbCatalogR", which = "cache")
}

shipped_dir <- function() {
  system.file("extdata", "bundle", package = "apbCatalogR", mustWork = TRUE)
}

# The bundle to use: the newest of the shipped and cached bundles this package can read.
choose_bundle <- function(dirs, origins) {
  manifests <- lapply(dirs, function(dir) read_json_file(file.path(dir, "manifest.json")))
  readable <- vapply(manifests, function(manifest) manifest$bundle_format <= BUNDLE_FORMAT, logical(1))
  for (index in which(!readable)) {
    warning(
      sprintf(
        "bundle %s in %s has format %s; update apbCatalogR to read it",
        manifests[[index]]$version,
        dirs[[index]],
        manifests[[index]]$bundle_format
      ),
      call. = FALSE
    )
  }
  if (!any(readable)) {
    stop("no bundle this version of apbCatalogR can read; update apbCatalogR", call. = FALSE)
  }
  versions <- lapply(manifests, function(manifest) package_version(manifest$version))
  best <- NULL
  for (index in which(readable)) {
    if (is.null(best) || versions[[index]] > versions[[best]]) {
      best <- index
    }
  }
  Bundle$new(dirs[[best]], origins[[best]])
}

current_bundle <- function() {
  if (is.null(state$bundle)) {
    cached <- list.dirs(cache_root(), recursive = FALSE)
    cached <- cached[file.exists(file.path(cached, "manifest.json"))]
    state$bundle <- choose_bundle(c(shipped_dir(), cached), c("shipped", rep("cache", length(cached))))
  }
  state$bundle
}

reset_bundle <- function() {
  state$bundle <- NULL
}

#' The catalog in use
#'
#' Reports the bundle lookups answer from: its version, whether it was shipped with the package or downloaded by
#' [update_catalog()], and the apb2 version whose rules it carries.
#'
#' @return A list with `version`, `origin` (`"shipped"` or `"cache"`), `path` and `apb2_version`.
#' @export
#' @examples
#' catalog_version()
catalog_version <- function() {
  bundle <- current_bundle()
  list(
    version = bundle$manifest$version,
    origin = bundle$origin,
    path = bundle$dir,
    apb2_version = bundle$manifest$apb2_version
  )
}

#' Download a newer catalog
#'
#' Downloads the bundle of an apb-catalog release from GitHub into the user cache, so lookups use it without a
#' package update. Later sessions keep using the newest bundle, shipped or downloaded.
#'
#' @param version The apb-catalog release, such as `"0.2.0"`; `NULL` downloads the latest release.
#' @return The [catalog_version()] now in use, invisibly.
#' @export
#' @examples
#' if (interactive()) {
#'   update_catalog()
#' }
update_catalog <- function(version = NULL) {
  url <- if (is.null(version)) {
    sprintf("%s/latest/download/%s.tar.gz", RELEASES, BUNDLE_NAME)
  } else {
    sprintf("%s/download/v%s/%s.tar.gz", RELEASES, version, BUNDLE_NAME)
  }
  archive <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(archive), add = TRUE)
  utils::download.file(url, archive, mode = "wb", quiet = TRUE)
  install_bundle(archive)
}

# Unpack one bundle archive into the user cache, under its version.
install_bundle <- function(archive) {
  unpacked <- tempfile()
  on.exit(unlink(unpacked, recursive = TRUE), add = TRUE)
  utils::untar(archive, exdir = unpacked)
  source <- file.path(unpacked, BUNDLE_NAME)
  manifest <- read_json_file(file.path(source, "manifest.json"))
  if (manifest$bundle_format > BUNDLE_FORMAT) {
    stop(
      sprintf("bundle %s has format %s; update apbCatalogR to read it", manifest$version, manifest$bundle_format),
      call. = FALSE
    )
  }
  dir.create(cache_root(), recursive = TRUE, showWarnings = FALSE)
  target <- file.path(cache_root(), manifest$version)
  unlink(target, recursive = TRUE)
  file.copy(source, cache_root(), recursive = TRUE)
  file.rename(file.path(cache_root(), BUNDLE_NAME), target)
  reset_bundle()
  invisible(catalog_version())
}
