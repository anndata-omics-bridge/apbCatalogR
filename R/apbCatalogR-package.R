#' apbCatalogR: look up APB result fields by meaning
#'
#' The R implementation of apb-catalog. It works on the bundle apb-catalog
#' releases: the reviewed sets and the levels apb2 converts for each
#' software version. [Catalog] answers which column one software version reports
#' for a meaning, without data; [DataCatalog] answers on a result read with
#' [read_parsed_levels()].
#'
#' The package ships a bundle; [update_catalog()] installs another one from a
#' local archive and [catalog_version()] reports the one in use.
#'
#' @section Conditions:
#' Lookups signal conditions of the classes apb-catalog raises in Python:
#' * `UnresolvedField`: the catalog cannot answer with one field; the request
#'   is ambiguous or unknown. Its `resolution` holds the candidates and reasons.
#' * `MissingRequiredField`: fields the named sets require are missing, under
#'   `enforce`, checked once when a catalog opens. Its `missing` lists every
#'   missing field, each as its `resolution` and requirement `row`, and its
#'   message has one line per field. Signalled as an error, or as a warning
#'   for the `recommended` fields when `enforce = "required"`.
#'
#' Catch them with [tryCatch()], for example
#' `tryCatch(..., UnresolvedField = function(condition) NULL)`.
#'
#' @keywords internal
#' @importFrom anndataR AnnData read_h5ad
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom Matrix sparseMatrix
#' @importFrom R6 R6Class
#' @importFrom stats setNames
#' @importFrom tools R_user_dir
#' @importFrom utils read.csv untar
"_PACKAGE"
