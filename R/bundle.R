# The bundle apb-catalog builds: catalog tables and expected answers. The
# package ships one; update_catalog() installs others into the user cache. The
# newest readable bundle is used.

BUNDLE_FORMAT <- 5L
BUNDLE_NAME <- "apb-catalog-bundle"

state <- new.env(parent = emptyenv())

Bundle <- R6Class(
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
                private$cache[[path]] <- read_json_file(file.path(
                    self$dir,
                    path
                ))
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
    R_user_dir("apbCatalogR", which = "cache")
}

shipped_dir <- function() {
    system.file("extdata", "bundle", package = "apbCatalogR", mustWork = TRUE)
}

# The bundle to use: the newest of the shipped and cached bundles this package
# can read, those of its format; an older format is passed over, a newer one
# with a warning.
choose_bundle <- function(dirs, origins) {
    manifests <- lapply(dirs, function(dir) {
        read_json_file(file.path(dir, "manifest.json"))
    })
    formats <- vapply(
        manifests,
        function(manifest) as.integer(manifest$bundle_format),
        integer(1)
    )
    readable <- formats == BUNDLE_FORMAT
    lapply(which(formats > BUNDLE_FORMAT), function(index) {
        warning(
            sprintf(
                "bundle %s in %s has format %s; update apbCatalogR to read it",
                manifests[[index]]$version,
                dirs[[index]],
                manifests[[index]]$bundle_format
            ),
            call. = FALSE
        )
    })
    if (!any(readable)) {
        stop(
            "no bundle this apbCatalogR version can read; update apbCatalogR",
            call. = FALSE
        )
    }
    versions <- package_version(vapply(
        manifests,
        function(manifest) as.character(manifest$version),
        character(1)
    ))
    # The first of the newest readable bundles.
    candidates <- which(readable)
    newest <- versions[candidates] == max(versions[candidates])
    best <- candidates[newest][[1L]]
    Bundle$new(dirs[[best]], origins[[best]])
}

current_bundle <- function() {
    if (is.null(state$bundle)) {
        cached <- list.dirs(cache_root(), recursive = FALSE)
        cached <- cached[file.exists(file.path(cached, "manifest.json"))]
        state$bundle <- choose_bundle(
            c(shipped_dir(), cached),
            c("shipped", rep("cache", length(cached)))
        )
    }
    state$bundle
}

reset_bundle <- function() {
    state$bundle <- NULL
}

#' The catalog in use
#'
#' Reports the bundle lookups answer from: its version, whether it was shipped
#' with the package or installed by [update_catalog()], and the apb2 version
#' its rule levels come from.
#'
#' @return A list with `version`, `origin` (`"shipped"` or `"cache"`), `path`
#'   and `apb2_version`.
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

#' Install a catalog bundle
#'
#' Installs a bundle archive into the user cache, so lookups use it without a
#' package update. apb-catalog's `make bundle` writes the archive, as
#' `build/apb-catalog-bundle.tar.gz`. Later sessions keep using the newest
#' bundle, shipped or installed. Nothing is downloaded.
#'
#' @param path Path to the bundle archive, a `.tar.gz` file.
#' @return The [catalog_version()] now in use, invisibly.
#' @export
#' @examples
#' if (interactive()) {
#'     update_catalog("apb-catalog/build/apb-catalog-bundle.tar.gz")
#' }
update_catalog <- function(path) {
    check_path(path)
    install_bundle(path)
}

# Unpack one bundle archive into the user cache, under its version.
install_bundle <- function(archive) {
    unpacked <- tempfile()
    on.exit(unlink(unpacked, recursive = TRUE), add = TRUE)
    untar(archive, exdir = unpacked)
    source <- file.path(unpacked, BUNDLE_NAME)
    manifest <- read_json_file(file.path(source, "manifest.json"))
    if (manifest$bundle_format > BUNDLE_FORMAT) {
        stop(
            sprintf(
                "bundle %s has format %s; update apbCatalogR to read it",
                manifest$version,
                manifest$bundle_format
            ),
            call. = FALSE
        )
    }
    if (manifest$bundle_format < BUNDLE_FORMAT) {
        stop(
            sprintf(
                "bundle %s has format %s; this apbCatalogR reads format %s",
                manifest$version,
                manifest$bundle_format,
                BUNDLE_FORMAT
            ),
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
