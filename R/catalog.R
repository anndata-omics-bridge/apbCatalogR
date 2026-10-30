# Look up APB fields by meaning, so callers never name vendor columns; apb_catalog.catalog.

# Qualifiers a request means unless it says otherwise, where the set declares them.
DEFAULTS <- c(stage = "identification")

#' Where a field lives
#'
#' The answer of a [Catalog] lookup: the level, the location (`"layers"`, `"var"`, `"varm"` or `"obs"`), the column
#' name and, for a `varm` field, its table.
#'
#' @field level The quantification level, such as `"peptidoform_ion"`.
#' @field location `"layers"`, `"var"`, `"varm"` or `"obs"`.
#' @field name The layer or column name.
#' @field table The `varm` table, `NA` elsewhere.
#' @return The class generator; `Reference$new()` returns a `Reference`.
#' @export
#' @examples
#' Reference$new("peptidoform_ion", "layers", "PEP")
Reference <- R6::R6Class(
  "Reference",
  public = list(
    level = NULL,
    location = NULL,
    name = NULL,
    table = NULL,

    #' @description Where one field lives.
    #' @param level The quantification level.
    #' @param location `"layers"`, `"var"`, `"varm"` or `"obs"`.
    #' @param name The layer or column name.
    #' @param table The `varm` table, `NA` elsewhere.
    initialize = function(level, location, name, table = NA_character_) {
      self$level <- level
      self$location <- location
      self$name <- name
      self$table <- table
    },

    #' @description Print the reference.
    #' @param ... Ignored.
    print = function(...) {
      table <- if (is.na(self$table)) "" else paste0(" [", self$table, "]")
      writeLines(sprintf("<Reference %s %s%s: %s>", self$level, self$location, table, self$name))
      invisible(self)
    }
  )
)

reference_of <- function(reference) {
  Reference$new(reference$level, reference$location, reference$name, reference$table)
}

# The entry bound to the level's column: by name, by the role's column or by prefix. A prefix matching several columns
# binds to each, so the lookup reports them as ambiguous.
bind_entry <- function(entry, roles, obs_columns) {
  reference <- entry$reference
  if (!is.na(reference$role)) {
    return(if (reference$role %in% names(roles)) list(bound_entry(entry, roles[[reference$role]])) else list())
  }
  if (!is.na(reference$prefix)) {
    matching <- obs_columns[startsWith(tolower(obs_columns), tolower(reference$prefix))]
    return(lapply(matching, function(column) bound_entry(entry, column)))
  }
  list(entry)
}

# Bind one level to its set: the reviewed vendor entries, then APB's own fields. Only entries whose field the level
# holds answer; a producer that has not run leaves its entries pending. A set with vendor catalogues answers only for
# a reviewed rule.
level_view <- function(name, software, pattern, rule_level, set, retained, roles, obs_columns, ran, annotations) {
  if (is.null(software) || is.null(pattern) || is.null(rule_level)) {
    return(UnreviewedLevel$new(name, NULL, NULL, "the level records no rule_json software version"))
  }
  entries <- list()
  reviewed <- set$reviewed(software, pattern, rule_level)
  if (set$needs_review) {
    if (is.null(reviewed)) {
      reason <- sprintf("no source catalogue reviewed %s %s %s", software, pattern, rule_level)
      return(UnreviewedLevel$new(name, software, pattern, reason))
    }
    entries <- held_entries(entries_for(reviewed$catalogue, reviewed$variant), retained)
  }
  produced <- producer_entries(set$producers, rule_level, retained, roles, obs_columns, ran)
  entries <- c(entries, produced$entries, annotation_entries(annotations, rule_level, roles, obs_columns))
  if (!is.null(reviewed)) {
    return(ReviewedLevel$new(name, reviewed, entries, produced$pending))
  }
  ProducedLevel$new(name, software, pattern, entries, produced$pending)
}

# The entries whose field the level holds.
held_entries <- function(entries, retained) {
  Filter(function(entry) reference_key(entry$reference) %in% retained, entries)
}

bound_entries <- function(source, rule_level, roles, obs_columns) {
  unlist(lapply(entries_on(source, rule_level), bind_entry, roles, obs_columns), recursive = FALSE)
}

# The producers' entries the level holds, and the entries of producers that have not run.
producer_entries <- function(producers, rule_level, retained, roles, obs_columns, ran) {
  entries <- list()
  pending <- list()
  for (producer in producers) {
    offered <- bound_entries(producer, rule_level, roles, obs_columns)
    if (ran(producer)) {
      entries <- c(entries, held_entries(offered, retained))
    } else {
      reason <- pending_reason(producer)
      pending <- c(pending, lapply(offered, function(entry) list(entry = entry, reason = reason)))
    }
  }
  list(entries = entries, pending = pending)
}

annotation_entries <- function(annotations, rule_level, roles, obs_columns) {
  unlist(lapply(annotations, bound_entries, rule_level, roles, obs_columns), recursive = FALSE)
}

check_enforce <- function(name, set, enforce) {
  if (is.null(enforce)) {
    return(invisible(NULL))
  }
  if (!(enforce %in% c("MUST", "SHOULD"))) {
    stop("enforce is NULL, \"MUST\" or \"SHOULD\"", call. = FALSE)
  }
  if (length(set$requirements) == 0L) {
    stop(sprintf("catalogue '%s' sets no requirements, so nothing to enforce", name), call. = FALSE)
  }
}

# A missing field the set requires: signal MissingRequiredField, or warn for a SHOULD field under enforce = "MUST".
enforce_requirement <- function(resolution, row, enforce) {
  resolution$requirement <- row$requirement
  if (row$requirement == "MUST" || enforce == "SHOULD") {
    stop(missing_required_field(resolution, row))
  }
  warning(missing_required_warning(resolution, row))
  NULL
}

# Who each entry of the set comes from, by entry id.
source_labels <- function(set) {
  labels <- character(0)
  for (source in set$sources()) {
    for (entry in source$entries) {
      labels[[entry$entry_id]] <- source$label
    }
  }
  labels
}

# The requests, answers and rows both catalogues share for one set.
Lookups <- R6::R6Class(
  "Lookups",
  public = list(
    name = NULL,
    set = NULL,
    views = NULL,
    enforce = NULL,

    initialize = function(name, set, views, enforce) {
      check_enforce(name, set, enforce)
      self$name <- name
      self$set <- set
      self$views <- views
      self$enforce <- enforce
    },

    request = function(level, location, qualifiers) {
      declared <- names(self$set$vocabulary$qualifiers)
      described <- as.list(DEFAULTS[names(DEFAULTS) %in% declared])
      for (name in names(qualifiers)) {
        described[[name]] <- unname(qualifiers[[name]])
      }
      field_request(level, location, described)
    },

    answer = function(request) {
      check_predicate(self$set$vocabulary, request$qualifiers)
      view <- self$views[[request$level]] %||% AbsentLevel$new(request$level)
      view$resolve(request)
    },

    # The field asked for; a missing required field signals or warns as enforce says.
    find = function(level, location, qualifiers) {
      request <- self$request(level, location, qualifiers)
      resolution <- self$answer(request)
      row <- if (resolution$status == "missing" && !is.null(self$enforce)) {
        self$set$requirement_for(level, location, request$qualifiers)
      }
      if (is.null(row) || row$requirement == "MAY") {
        return(resolution$found())
      }
      enforce_requirement(resolution, row, self$enforce)
    },

    kinds = function(level, location, qualifiers) {
      self$answer(self$request(level, location, qualifiers))$offered("kind")
    },

    rows = function(view, entries) {
      labels <- source_labels(self$set)
      rule <- view$rule_name %||% ANY_RULE
      lapply(entries, function(entry) {
        entry_row(
          self$name,
          self$set,
          labels[[entry$entry_id]],
          view$software_version_pattern %||% NA_character_,
          if (length(entry$rules) > 0L) rule else ANY_RULE,
          entry
        )
      })
    }
  )
)

qualifier_arguments <- function(...) {
  qualifiers <- list(...)
  if (length(qualifiers) > 0L && (is.null(names(qualifiers)) || any(names(qualifiers) == ""))) {
    stop("qualifiers are named, such as normalization = \"none\"", call. = FALSE)
  }
  qualifiers
}

fields_frame <- function(rows, set) {
  records_frame(rows, c(set_columns(set), "present", "constant"))
}

#' Which column one software version reports for each meaning
#'
#' Answers from the rules alone, without data. The rule of the software whose version pattern admits the version
#' decides which fields each level has. Lookups that several fields answer, or that an unreviewed level cannot,
#' signal `UnresolvedField`.
#'
#' @field description What this catalogue set holds and who uses it.
#' @return The class generator; `Catalog$new()` returns a `Catalog`.
#' @export
#' @examples
#' catalog <- Catalog$new("DIA-NN", "2.2.0", "abundance")
#' catalog$layer("peptidoform_ion", kind = "intensity", normalization = "none")
#' catalog$kinds("peptidoform_ion", "layers")
Catalog <- R6::R6Class(
  "Catalog",
  public = list(
    #' @description Bind a catalogue set to the rule levels of one software version.
    #' @param software The software name, such as `"DIA-NN"`.
    #' @param version The software version, such as `"2.2.0"`.
    #' @param catalog The catalogue set, such as `"abundance"` or `"confidence"`; see [catalogues()].
    #' @param enforce `NULL`, `"MUST"` or `"SHOULD"`: signal `MissingRequiredField` when a field the set requires
    #'   at that strength is missing.
    initialize = function(software, version, catalog, enforce = NULL) {
      set <- catalogue_set(catalog)
      matched <- rules_matching(software, version)
      if (length(matched) == 0L) {
        known <- unique(vapply(
          Filter(function(found) tolower(found$software_name) == tolower(software), rule_levels()),
          function(found) found$software_version_pattern,
          character(1)
        ))
        stop(
          sprintf(
            "no packaged %s rule admits version '%s'; patterns: %s",
            software,
            version,
            paste(sort_c(known), collapse = ", ")
          ),
          call. = FALSE
        )
      }
      views <- list()
      for (found in matched) {
        views[[found$level]] <- level_view(
          found$level,
          found$software_name,
          found$software_version_pattern,
          found$level,
          set,
          retained = rule_retained(found$rule, found$level),
          roles = rule_roles(found$rule, found$level),
          obs_columns = character(0),
          ran = function(producer) length(producer$requires) == 0L,
          annotations = list()
        )
      }
      private$lookups <- Lookups$new(catalog, set, views, enforce)
    },

    #' @description The kinds the level offers at one location, given the other qualifiers.
    #' @param level The quantification level.
    #' @param location `"layers"`, `"var"` or `"obs"`.
    #' @param ... Other qualifiers by name, such as `normalization = "none"`.
    #' @return A character vector.
    kinds = function(level, location, ...) {
      private$lookups$kinds(level, location, qualifier_arguments(...))
    },

    #' @description Where the sample-by-feature layer with this meaning lives.
    #' @param level The quantification level.
    #' @param kind The kind, such as `"intensity"`.
    #' @param ... Other qualifiers by name.
    #' @return A [Reference], or `NULL` when the level has no such layer.
    layer = function(level, kind, ...) {
      private$found(level, "layers", kind, qualifier_arguments(...))
    },

    #' @description Where the per-feature column with this meaning lives, in `var` or a `varm` table.
    #' @param level The quantification level.
    #' @param kind The kind.
    #' @param ... Other qualifiers by name.
    #' @return A [Reference], or `NULL`.
    var = function(level, kind, ...) {
      private$found(level, "var", kind, qualifier_arguments(...))
    },

    #' @description Where the per-sample column with this meaning lives.
    #' @param level The quantification level.
    #' @param kind The kind.
    #' @param ... Other qualifiers by name.
    #' @return A [Reference], or `NULL`.
    obs = function(level, kind, ...) {
      private$found(level, "obs", kind, qualifier_arguments(...))
    },

    #' @description Every catalogued field the rule declares, one row per field, as [catalogue_table()] shows them.
    #' @return A data frame, with `present` true for each row.
    fields = function() {
      lookups <- private$lookups
      rows <- unlist(
        lapply(lookups$views, function(view) {
          lapply(lookups$rows(view, view$entries), function(row) c(row, list(present = TRUE, constant = NA_character_)))
        }),
        recursive = FALSE
      )
      fields_frame(rows, lookups$set)
    },

    #' @description Print the set and the levels.
    #' @param ... Ignored.
    print = function(...) {
      writeLines(sprintf(
        "<Catalog %s: %s>",
        private$lookups$name,
        paste(names(private$lookups$views), collapse = ", ")
      ))
      invisible(self)
    }
  ),
  active = list(
    description = function() private$lookups$set$description
  ),
  private = list(
    lookups = NULL,
    found = function(level, location, kind, qualifiers) {
      found <- private$lookups$find(level, location, c(list(kind = kind), qualifiers))
      if (is.null(found)) NULL else reference_of(found)
    }
  )
)

#' What one result's fields mean to one consumer
#'
#' Answers on a result read with [read_parsed_levels()] and hands the data back directly. A lookup of a field the
#' result's stored rule declares differently from the catalog's rule signals `StaleField`; ambiguous or unreviewed
#' answers signal `UnresolvedField`.
#'
#' @field description What this catalogue set holds and who uses it.
#' @return The class generator; `DataCatalog$new()` returns a `DataCatalog`.
#' @export
#' @examples
#' results <- tempfile()
#' archive <- system.file("extdata", "test-bundle", "results.tar.gz", package = "apbCatalogR")
#' utils::untar(archive, exdir = results)
#' result <- read_parsed_levels(file.path(results, "maxquant_wide-peptidoform_ion.parquet"))
#' catalog <- DataCatalog$new(result, "confidence")
#' dim(catalog$layer("peptidoform_ion", kind = "pep"))
DataCatalog <- R6::R6Class(
  "DataCatalog",
  public = list(
    #' @description Bind a catalogue set to the levels of one result.
    #' @param result A [ParsedLevels] object.
    #' @param catalog The catalogue set, such as `"confidence"`; see [catalogues()].
    #' @param enforce `NULL`, `"MUST"` or `"SHOULD"`: signal `MissingRequiredField` when a field the set requires
    #'   at that strength is missing.
    initialize = function(result, catalog, enforce = NULL) {
      if (!inherits(result, "ParsedLevels")) {
        stop("result is a ParsedLevels, as read_parsed_levels() returns", call. = FALSE)
      }
      private$parsed <- result
      set <- catalogue_set(catalog)
      annotations <- Filter(function(annotation) annotation_applies(annotation, result$metadata), set$annotations)
      views <- list()
      for (name in names(result$levels)) {
        level <- result$levels[[name]]
        version <- level_rule_version(level)
        views[[name]] <- level_view(
          name,
          version$software,
          version$pattern,
          version$level,
          set,
          retained = level_retained(level),
          roles = level_roles(level),
          obs_columns = colnames(level$obs),
          ran = function(producer) producer_ran(producer, result$metadata),
          annotations = annotations
        )
      }
      private$lookups <- Lookups$new(catalog, set, views, enforce)
    },

    #' @description The kinds the level offers at one location, given the other qualifiers.
    #' @param level The quantification level.
    #' @param location `"layers"`, `"var"` or `"obs"`.
    #' @param ... Other qualifiers by name.
    #' @return A character vector.
    kinds = function(level, location, ...) {
      private$lookups$kinds(level, location, qualifier_arguments(...))
    },

    #' @description The layer with this meaning.
    #' @param level The quantification level.
    #' @param kind The kind, such as `"pep"`.
    #' @param ... Other qualifiers by name.
    #' @return An observations-by-features matrix, or `NULL` when the result has no such layer.
    layer = function(level, kind, ...) {
      reference <- private$find(level, "layers", kind, qualifier_arguments(...))
      if (is.null(reference)) NULL else level_layer(private$parsed$levels[[level]], reference$name)
    },

    #' @description The per-feature column with this meaning, from `var` or a `varm` table.
    #' @param level The quantification level.
    #' @param kind The kind.
    #' @param ... Other qualifiers by name.
    #' @return A vector, or `NULL`.
    var = function(level, kind, ...) {
      reference <- private$find(level, "var", kind, qualifier_arguments(...))
      if (is.null(reference)) {
        return(NULL)
      }
      found <- private$parsed$levels[[level]]
      if (is.na(reference$table)) found$var[[reference$name]] else found$varm[[reference$table]][[reference$name]]
    },

    #' @description The per-sample column with this meaning.
    #' @param level The quantification level.
    #' @param kind The kind.
    #' @param ... Other qualifiers by name.
    #' @return A vector, or `NULL`.
    obs = function(level, kind, ...) {
      reference <- private$find(level, "obs", kind, qualifier_arguments(...))
      if (is.null(reference)) NULL else private$parsed$levels[[level]]$obs[[reference$name]]
    },

    #' @description Every catalogued field of the result, one row per field, as [catalogue_table()] shows them.
    #' @return A data frame with `present` and, for a field apb2 left out because it held one value, that
    #'   `constant`.
    fields = function() {
      lookups <- private$lookups
      rows <- unlist(
        lapply(names(lookups$views), function(name) {
          level_fields(lookups, lookups$views[[name]], private$parsed$levels[[name]])
        }),
        recursive = FALSE
      )
      fields_frame(rows, lookups$set)
    },

    #' @description Print the set and the levels.
    #' @param ... Ignored.
    print = function(...) {
      writeLines(sprintf(
        "<DataCatalog %s: %s>",
        private$lookups$name,
        paste(names(private$lookups$views), collapse = ", ")
      ))
      invisible(self)
    }
  ),
  active = list(
    description = function() private$lookups$set$description
  ),
  private = list(
    lookups = NULL,
    parsed = NULL,

    find = function(level, location, kind, qualifiers) {
      reference <- private$lookups$find(level, location, c(list(kind = kind), qualifiers))
      if (!is.null(reference)) {
        check_current(private$lookups$views[[level]], private$parsed$levels[[level]], reference)
      }
      reference
    }
  )
)

# Signal StaleField when the level's stored rule declares the field differently from the bundle's rule.
check_current <- function(view, level, reference) {
  rule <- view$rule_name
  stored_rule <- level_stored_rule(level)
  if (is.null(rule) || is.null(stored_rule)) {
    return(invisible(NULL))
  }
  stored <- rule_field(stored_rule, reference$location, reference$name)
  current <- lapply(
    rule_declarations(rule, stored_rule$quantification_level),
    rule_field,
    reference$location,
    reference$name
  )
  if (
    any(vapply(current, function(declared) json_equal(stored, declared), logical(1))) ||
      (is.null(stored) && all(vapply(current, is.null, logical(1))))
  ) {
    return(invisible(NULL))
  }
  declared <- Find(Negate(is.null), current)
  stop(stale_field(reference, source_text(stored), source_text(declared)))
}

# The rows of one level's fields: those it holds, and for a reviewed level those it lacks, with the single value of a
# field apb2 left out.
level_fields <- function(lookups, view, level) {
  rows <- lapply(lookups$rows(view, view$entries), function(row) c(row, list(present = TRUE, constant = NA_character_)))
  if (!inherits(view, "ReviewedLevel")) {
    return(rows)
  }
  held <- vapply(view$entries, function(entry) entry$entry_id, character(1))
  left_out <- level_left_out(level)
  absent <- Filter(function(entry) !(entry$entry_id %in% held), entries_for(view$rule$catalogue, view$rule$variant))
  c(
    rows,
    Map(
      function(entry, row) {
        constant <- left_out[[reference_key(entry$reference)]] %||% NA_character_
        c(row, list(present = FALSE, constant = constant))
      },
      absent,
      lookups$rows(view, absent)
    )
  )
}

source_text <- function(declared) {
  if (is.null(declared)) {
    return("no such field")
  }
  source <- declared$source
  if (is.null(source) || identical(source, "")) {
    source <- declared$inputs
  }
  if (!is.null(source) && length(source) > 0L) json_text(source) else json_text(declared[sort_c(names(declared))])
}
