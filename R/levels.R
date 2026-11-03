# A result's levels read through a catalog: anndataR's slots, looked up by key as cf_xarray does; apb_catalog.levels.

#' One level of a result, read through a catalog
#'
#' anndataR's slots of one level: `X`, `layers`, `var`, `obs`, `obs_names` and `var_names`. `X` is the level's
#' primary layer and `layers` its other layers, observations by variables; a key may name the primary layer, and then
#' answers `X`. `cat$levels$protein$layers[["maxlfq"]]` looks the name up as one of the catalog's keys first, then as
#' the level's own name in that slot, and signals an error when it is neither. `obs[c("group", "Run")]` reads every
#' column the names name as a data frame.
#'
#' @field name The level's name.
#' @field layers The level's layers, read with `[[`; `$keys()` lists the keys it offers.
#' @field var The level's per-feature columns, from `var` or a `varm` table, read with `[[` or `[`.
#' @field obs The level's per-sample columns, read with `[[` or `[`.
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
    #' @param lookup How the catalog answers keys: `find`, `find_all`, `offered` and `known`.
    initialize = function(name, level, lookup) {
      self$name <- name
      private$level <- level
      self$layers <- Slot$new(
        name,
        "layers",
        lookup,
        function(reference) level_layer(level, reference$name),
        names(level$layers)
      )
      self$var <- Columns$new(
        name,
        "var",
        lookup,
        function(reference) {
          if (is.na(reference$table)) level$var[[reference$name]] else level$varm[[reference$table]][[reference$name]]
        },
        colnames(level$var),
        columns_frame
      )
      self$obs <- Columns$new(
        name,
        "obs",
        lookup,
        function(reference) level$obs[[reference$name]],
        colnames(level$obs),
        columns_frame
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

#' One level of a software version's rule, holding names
#'
#' The slots of a [Level] without data: `layers`, `var` and `obs` answer a key with the field's name, a `varm` column
#' as `table/column`. There is no `X`, `obs_names`, `var_names` or own name: a name that is no key the level answers
#' signals an error.
#'
#' @field name The level's name.
#' @field layers The level's layer names, read with `[[`; `$keys()` lists the keys it offers.
#' @field var The level's per-feature column names, read with `[[` or `[`.
#' @field obs The level's per-sample column names, read with `[[` or `[`.
#' @return The class generator; a [Catalog]'s `levels` hold its objects.
#' @keywords internal
LevelNames <- R6::R6Class(
  "LevelNames",
  public = list(
    name = NULL,
    layers = NULL,
    var = NULL,
    obs = NULL,

    #' @description Bind one rule level to the catalog's lookups.
    #' @param name The level's name.
    #' @param lookup How the catalog answers keys: `find`, `find_all`, `offered` and `known`.
    initialize = function(name, lookup) {
      self$name <- name
      self$layers <- Slot$new(name, "layers", lookup, slot_name, character(0))
      self$var <- Columns$new(name, "var", lookup, slot_name, character(0), columns_names)
      self$obs <- Columns$new(name, "obs", lookup, slot_name, character(0), columns_names)
    }
  )
)

#' One slot of a level, read by key first
#'
#' As cf_xarray's accessor: `[[` reads a key, else the level's own name; `$keys()` lists the catalog's keys only.
#'
#' @return The class generator; a [Level]'s `layers` is its object.
#' @keywords internal
Slot <- R6::R6Class(
  "Slot",
  public = list(
    #' @description Bind one slot of a level.
    #' @param level The level's name.
    #' @param location `"layers"`, `"var"` or `"obs"`.
    #' @param lookup How the catalog answers keys: `find`, `find_all`, `offered` and `known`.
    #' @param read The value of a field the slot holds.
    #' @param named The level's own names in this slot.
    initialize = function(level, location, lookup, read, named) {
      private$level <- level
      private$location <- location
      private$lookup <- lookup
      private$read <- read
      private$named <- named
    },

    #' @description The field a key names, else the level's own field of that name; an error when it is neither.
    #' @param name A key, or the level's own name.
    #' @return A matrix for layers, a vector for var and obs; a name without data.
    get = function(name) {
      if (name %in% private$lookup$known) {
        found <- private$lookup$find(private$level, private$location, name)
        if (!is.null(found)) {
          return(private$read(found))
        }
      }
      private$read(private$own(name))
    },

    #' @description The keys the level offers in this slot.
    #' @return A character vector.
    keys = function() {
      private$lookup$offered(private$level, private$location)
    }
  ),
  private = list(
    level = NULL,
    location = NULL,
    lookup = NULL,
    read = NULL,
    named = NULL,

    own = function(name) {
      if (!(name %in% private$named)) {
        stop(sprintf("%s %s has no key or field '%s'", private$level, private$location, name), call. = FALSE)
      }
      list(name = name, table = NA_character_)
    }
  )
)

#' A var or obs slot, also read by several names
#'
#' `obs[c("group")]` holds every column the key names, each under its own name, where `obs[["group"]]` signals
#' `UnresolvedField` because there are several.
#'
#' @return The class generator; a [Level]'s `var` and `obs` are its objects.
#' @keywords internal
Columns <- R6::R6Class(
  "Columns",
  inherit = Slot,
  public = list(
    #' @description Bind one var or obs slot of a level.
    #' @param level The level's name.
    #' @param location `"var"` or `"obs"`.
    #' @param lookup How the catalog answers keys: `find`, `find_all`, `offered` and `known`.
    #' @param read The value of a field the slot holds.
    #' @param named The level's own names in this slot.
    #' @param combine The fields read, a named list, as one value.
    initialize = function(level, location, lookup, read, named, combine) {
      super$initialize(level, location, lookup, read, named)
      private$combine <- combine
    },

    #' @description Every field the names name, in order; an error for a name that is neither a key nor the level's own.
    #' @param names Keys, or the level's own names.
    #' @return A data frame; names without data.
    get_all = function(names) {
      found <- list()
      for (name in names) {
        for (reference in private$fields(name)) {
          found[[slot_name(reference)]] <- reference
        }
      }
      private$combine(lapply(found, private$read))
    }
  ),
  private = list(
    combine = NULL,

    fields = function(name) {
      if (name %in% private$lookup$known) {
        found <- private$lookup$find_all(private$level, private$location, name)
        if (length(found) > 0L) {
          return(found)
        }
      }
      list(private$own(name))
    }
  )
)

# A field's name in its slot; table/column for a column of a varm table.
slot_name <- function(reference) {
  if (is.na(reference$table)) reference$name else paste0(reference$table, "/", reference$name)
}

columns_frame <- function(columns) {
  as.data.frame(columns, optional = TRUE)
}

columns_names <- function(columns) {
  unname(unlist(columns, use.names = FALSE)) %||% character(0)
}

#' Read a slot by key
#'
#' @param x A [Slot].
#' @param i A key, or the level's own name.
#' @param ... Ignored.
#' @return A matrix for layers, a vector for var and obs; a name without data.
#' @export
`[[.Slot` <- function(x, i, ...) {
  x$get(i)
}

#' Read a var or obs slot by several keys
#'
#' @param x A var or obs slot of a level.
#' @param i Keys, or the level's own names.
#' @param ... Ignored.
#' @return A data frame of every column the names name; the names without data.
#' @export
`[.Columns` <- function(x, i, ...) {
  x$get_all(i)
}
