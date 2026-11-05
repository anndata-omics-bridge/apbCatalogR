# A result's levels read through a catalog: anndataR's slots, looked up by key
# as cf_xarray does; apb_catalog.levels.

#' One level of a result, read through a catalog
#'
#' anndataR's slots of one level: `X`, `layers`, `var`, `obs`, `obs_names` and
#' `var_names`. `X` is the level's primary layer and `layers` its other layers,
#' observations by variables; a key may name the primary layer, and then answers
#' `X`. `cat$levels$protein$layers[["maxlfq"]]` looks the name up as one of the
#' catalog's keys first, then as the level's own name in that slot, and signals
#' an error when it is neither; a prefix makes the lookup exact, `"c:q_value"`
#' the confidence set alone, `"v:q_value"` the level's own name alone.
#' `obs[c("group", "Run")]` reads every column the names name as a data frame.
#'
#' @field name The level's name.
#' @field layers The level's layers, read with `[[` or `$get()`; `$name()` names
#'   the field, `$keys()` lists the keys it offers.
#' @field var The level's per-feature columns, from `var` or a `varm` table,
#'   read with `[[`, `[` or `$get()`.
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
        #' @param lookup How the catalog answers keys: `find`, `find_all`,
        #'   `offered`, `known` and `sets`, one lookup per set prefix.
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
                    if (is.na(reference$table)) {
                        level$var[[reference$name]]
                    } else {
                        level$varm[[reference$table]][[reference$name]]
                    }
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
        #' @field obs_names One name per observation: its key columns, joined by
        #'   `|`.
        obs_names = function() private$level$obs_names,
        #' @field var_names One name per variable: its key columns, joined by
        #'   `|`.
        var_names = function() private$level$var_names
    ),
    private = list(level = NULL)
)

#' One level of a software version's rule, holding names
#'
#' The slots of a [Level] without data: `layers`, `var` and `obs` answer a key
#' with the field's name, a `varm` column as `table/column`. There is no `X`,
#' `obs_names`, `var_names` or own name: a name that is no key the level answers
#' signals an error.
#'
#' @field name The level's name.
#' @field layers The level's layer names, read with `[[`; `$keys()` lists the
#'   keys it offers.
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
        #' @param lookup How the catalog answers keys: `find`, `find_all`,
        #'   `offered`, `known` and `sets`, one lookup per set prefix.
        initialize = function(name, lookup) {
            self$name <- name
            self$layers <- Slot$new(
                name,
                "layers",
                lookup,
                slot_name,
                character(0)
            )
            self$var <- Columns$new(
                name,
                "var",
                lookup,
                slot_name,
                character(0),
                columns_names
            )
            self$obs <- Columns$new(
                name,
                "obs",
                lookup,
                slot_name,
                character(0),
                columns_names
            )
        }
    )
)

#' One slot of a level, read by key first
#'
#' As cf_xarray's accessor: `[[` reads a key, else the level's own name;
#' `$keys()` lists the catalog's keys only. `$get()` returns `NULL` where `[[`
#' signals an error, `$name()` the field's name; both take the qualifiers a key
#' leaves open. A set's prefix, `"c:q_value"`, asks that set alone and never
#' the level's own name; `"v:q_value"` asks the level's own name alone.
#'
#' @return The class generator; a [Level]'s `layers` is its object.
#' @keywords internal
Slot <- R6::R6Class(
    "Slot",
    public = list(
        #' @description Bind one slot of a level.
        #' @param level The level's name.
        #' @param location `"layers"`, `"var"` or `"obs"`.
        #' @param lookup How the catalog answers keys: `find`, `find_all`,
        #'   `offered`, `known` and `sets`, one lookup per set prefix.
        #' @param read The value of a field the slot holds.
        #' @param named The level's own names in this slot.
        initialize = function(level, location, lookup, read, named) {
            private$level <- level
            private$location <- location
            private$lookup <- lookup
            private$read <- read
            private$named <- named
        },

        #' @description The field a key names, narrowed by further qualifiers,
        #'   else the level's own field of that name.
        #' @param key A key, or the level's own name.
        #' @param ... Further qualifiers by name, such as `unit = "minute"`.
        #' @return A matrix for layers, a vector for var and obs, a name without
        #'   data; `NULL` when it is neither.
        get = function(key, ...) {
            reference <- private$reference(key, qualifier_arguments(...))
            if (is.null(reference)) NULL else private$read(reference)
        },

        #' @description The name of the field `get` reads; an error when the
        #'   name is neither a key the level answers nor one of its own names.
        #' @param key A key, or the level's own name.
        #' @param ... Further qualifiers by name, such as `unit = "minute"`.
        #' @return The field's name, such as `"PEP"`; `table/column` for a
        #'   column of a `varm` table.
        name = function(key, ...) {
            slot_name(private$found(key, qualifier_arguments(...)))
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

        # What `[[` reads: an error where `get` returns NULL.
        item = function(key) {
            private$read(private$found(key, list()))
        },

        reference = function(name, qualifiers) {
            route <- slot_route(private$lookup, name, qualifiers)
            if (route_asks(route, qualifiers)) {
                found <- route$lookup$find(
                    private$level,
                    private$location,
                    route$key,
                    qualifiers
                )
                if (!is.null(found) || !route$own) {
                    return(found)
                }
            }
            private$own(route$key)
        },

        found = function(key, qualifiers) {
            reference <- private$reference(key, qualifiers)
            if (is.null(reference)) {
                private$missing(key)
            }
            reference
        },

        own = function(name) {
            if (name %in% private$named) {
                list(name = name, table = NA_character_)
            }
        },

        missing = function(name) {
            stop(
                sprintf(
                    "%s %s has no key or field '%s'",
                    private$level,
                    private$location,
                    name
                ),
                call. = FALSE
            )
        }
    )
)

#' A var or obs slot, also read by several names
#'
#' `obs[c("group")]` holds every column the key names, each under its own name,
#' where `obs[["group"]]` signals `UnresolvedField` because there are several.
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
        #' @param lookup How the catalog answers keys: `find`, `find_all`,
        #'   `offered`, `known` and `sets`, one lookup per set prefix.
        #' @param read The value of a field the slot holds.
        #' @param named The level's own names in this slot.
        #' @param combine The fields read, a named list, as one value.
        initialize = function(level, location, lookup, read, named, combine) {
            super$initialize(level, location, lookup, read, named)
            private$combine <- combine
        },

        #' @description Every field the names name, in order; an error for a
        #'   name that is neither a key nor the level's own.
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
            route <- slot_route(private$lookup, name, list())
            if (route_asks(route, list())) {
                found <- route$lookup$find_all(
                    private$level,
                    private$location,
                    route$key
                )
                if (length(found) > 0L) {
                    return(found)
                }
            }
            own <- if (route$own) private$own(route$key)
            if (is.null(own)) {
                private$missing(name)
            }
            list(own)
        }
    )
)

# The prefix that asks a level's own name, never a set: v:q_value.
VENDOR <- "v"

# The lookup a name asks, the key it asks for, and whether the level's own name
# answers when the lookup does not: a plain name asks every set, then the own
# name; a set's prefix that set alone; v: the own name alone, with no lookup and
# no qualifiers.
slot_route <- function(lookup, name, qualifiers) {
    colon <- regexpr(":", name, fixed = TRUE)
    if (colon < 0L) {
        return(list(lookup = lookup, key = name, own = TRUE))
    }
    prefix <- substr(name, 1L, colon - 1L)
    key <- substr(name, colon + 1L, nchar(name))
    if (prefix == VENDOR) {
        if (length(qualifiers) > 0L) {
            stop(
                sprintf(
                    "'%s' names the level's own field, %s",
                    name,
                    "which takes no qualifiers"
                ),
                call. = FALSE
            )
        }
        return(list(lookup = NULL, key = key, own = TRUE))
    }
    found <- lookup$sets[[prefix]]
    if (is.null(found)) {
        stop(
            sprintf(
                "unknown prefix '%s' in '%s'; prefixes: %s",
                prefix,
                name,
                paste(sort_c(c(VENDOR, names(lookup$sets))), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    list(lookup = found, key = key, own = FALSE)
}

# Whether a routed name asks its lookup: a known key, qualifiers, or a set's
# prefix, which the own name never answers.
route_asks <- function(route, qualifiers) {
    !is.null(route$lookup) &&
        (route$key %in%
            route$lookup$known ||
            length(qualifiers) > 0L ||
            !route$own)
}

# A field's name in its slot; table/column for a column of a varm table.
slot_name <- function(reference) {
    if (is.na(reference$table)) {
        reference$name
    } else {
        paste0(reference$table, "/", reference$name)
    }
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
    x$.__enclos_env__$private$item(i)
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
