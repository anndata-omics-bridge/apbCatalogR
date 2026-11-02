# A result's levels read through a catalog: anndataR's slots, looked up by key as cf_xarray does; apb_catalog.levels.

#' One level of a result, read through a catalog
#'
#' anndataR's slots of one level: `X`, `layers`, `var`, `obs`, `obs_names` and `var_names`. `X` is the level's
#' primary layer and `layers` its other layers, observations by variables; a key may name the primary layer, and then
#' answers `X`. `cat$levels$protein$layers[["maxlfq"]]` looks the name up as one of the catalog's keys first, then as
#' the level's own name in that slot, and signals an error when it is neither.
#'
#' @field name The level's name.
#' @field layers The level's layers, read with `[[`; `$keys()` lists the keys it offers.
#' @field var The level's per-feature columns, from `var` or a `varm` table, read with `[[`.
#' @field obs The level's per-sample columns, read with `[[`.
#' @return The class generator; a [DataCatalog]'s `levels` hold its objects.
#' @keywords internal
Level <- R6::R6Class(
  "Level",
  public = list(
    name = NULL,
    layers = NULL,
    var = NULL,
    obs = NULL,

    #' @description Bind one level to the catalog's lookups.
    #' @param name The level's name.
    #' @param level The level, an anndataR `AnnData`.
    #' @param find The field a key names on a level and location, or `NULL`.
    #' @param offered The keys a level offers at a location.
    #' @param known Every key of the catalog's sets.
    initialize = function(name, level, find, offered, known) {
      self$name <- name
      private$level <- level
      self$layers <- Slot$new(
        name,
        "layers",
        find,
        offered,
        known,
        function(reference) level_layer(level, reference$name),
        names(level$layers)
      )
      self$var <- Slot$new(
        name,
        "var",
        find,
        offered,
        known,
        function(reference) {
          if (is.na(reference$table)) level$var[[reference$name]] else level$varm[[reference$table]][[reference$name]]
        },
        colnames(level$var)
      )
      self$obs <- Slot$new(
        name,
        "obs",
        find,
        offered,
        known,
        function(reference) level$obs[[reference$name]],
        colnames(level$obs)
      )
    }
  ),
  active = list(
    #' @field X The primary layer, observations by variables.
    X = function() private$level$X,
    #' @field obs_names One name per observation: its key columns, joined by `|`.
    obs_names = function() private$level$obs_names,
    #' @field var_names One name per variable: its key columns, joined by `|`.
    var_names = function() private$level$var_names
  ),
  private = list(level = NULL)
)

#' One slot of a level, read by key first
#'
#' As cf_xarray's accessor: `[[` reads a key, else the level's own name; `$keys()` lists the catalog's keys only.
#'
#' @return The class generator; a [Level]'s `layers`, `var` and `obs` are its objects.
#' @keywords internal
Slot <- R6::R6Class(
  "Slot",
  public = list(
    #' @description Bind one slot of a level.
    #' @param level The level's name.
    #' @param location `"layers"`, `"var"` or `"obs"`.
    #' @param find The field a key names on a level and location, or `NULL`.
    #' @param offered The keys a level offers at a location.
    #' @param known Every key of the catalog's sets.
    #' @param read The value of a field the slot holds.
    #' @param named The level's own names in this slot.
    initialize = function(level, location, find, offered, known, read, named) {
      private$level <- level
      private$location <- location
      private$find <- find
      private$offered <- offered
      private$known <- known
      private$read <- read
      private$named <- named
    },

    #' @description The field a key names, else the level's own field of that name; an error when it is neither.
    #' @param name A key, or the level's own name.
    #' @return A matrix for layers, a vector for var and obs.
    get = function(name) {
      if (name %in% private$known) {
        found <- private$find(private$level, private$location, name)
        if (!is.null(found)) {
          return(private$read(found))
        }
      }
      if (name %in% private$named) {
        return(private$read(list(name = name, table = NA_character_)))
      }
      stop(sprintf("%s %s has no key or field '%s'", private$level, private$location, name), call. = FALSE)
    },

    #' @description The keys the level offers in this slot.
    #' @return A character vector.
    keys = function() {
      private$offered(private$level, private$location)
    }
  ),
  private = list(
    level = NULL,
    location = NULL,
    find = NULL,
    offered = NULL,
    known = NULL,
    read = NULL,
    named = NULL
  )
)

#' Read a slot by key
#'
#' @param x A [Slot].
#' @param i A key, or the level's own name.
#' @param ... Ignored.
#' @return A matrix for layers, a vector for var and obs.
#' @export
`[[.Slot` <- function(x, i, ...) {
  x$get(i)
}
