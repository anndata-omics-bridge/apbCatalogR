#' apbCatalogR: look up APB result fields by meaning
#'
#' The R implementation of apb-catalog. It works on the bundle apb-catalog releases: apb2's effective parse rules
#' and the reviewed catalogue sets. [Catalog] answers which
#' column one software version reports for a meaning, without data; [DataCatalog] answers on a result read with
#' [read_parsed_levels()].
#'
#' The package ships a bundle; [update_catalog()] downloads a newer one and [catalog_version()] reports the one in
#' use.
#'
#' @section Conditions:
#' Lookups signal conditions of the classes apb-catalog raises in Python:
#' * `UnresolvedField`: the catalogue cannot answer with one field; the request is ambiguous or unknown. Its
#'   `resolution` holds the candidates and reasons.
#' * `StaleField`: the result's stored rule declares the field differently from the bundle's rule.
#' * `MissingRequiredField`: a field the set requires is missing, under `enforce`. Signalled as an error, or as a
#'   warning for a `SHOULD` field when `enforce = "MUST"`.
#'
#' Catch them with [tryCatch()], for example `tryCatch(..., UnresolvedField = function(condition) NULL)`.
#'
#' @keywords internal
#' @importFrom R6 R6Class
"_PACKAGE"
