# Look up APB fields by meaning, so callers never name vendor columns; apb_catalog.catalog.

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

# The entry bound to the level's column: by name or by prefix. A prefix matching several columns binds to each, so the
# lookup reports them as ambiguous.
bind_entry <- function(entry, obs_columns) {
  reference <- entry$reference
  if (!is.na(reference$prefix)) {
    matching <- obs_columns[startsWith(tolower(obs_columns), tolower(reference$prefix))]
    return(lapply(matching, function(column) bound_entry(entry, column)))
  }
  list(entry)
}

# Bind one level to its set: the reviewed vendor entries, then the apps' and annotations'. Only entries whose field the
# level holds answer; an app that has not run leaves its entries pending. A set with vendor catalogues answers only for
# a reviewed software version.
level_view <- function(name, software, pattern, rule_level, set, retained, obs_columns, ran, annotations) {
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
  produced <- producer_entries(set$producers, rule_level, retained, obs_columns, ran)
  entries <- c(entries, produced$entries, annotation_entries(annotations, rule_level, obs_columns))
  if (!is.null(reviewed)) {
    return(ReviewedLevel$new(name, reviewed, entries, produced$pending))
  }
  ProducedLevel$new(name, software, pattern, entries, produced$pending)
}

# The entries whose field the level holds.
held_entries <- function(entries, retained) {
  Filter(function(entry) reference_key(entry$reference) %in% retained, entries)
}

bound_entries <- function(source, rule_level, obs_columns) {
  unlist(lapply(entries_on(source, rule_level), bind_entry, obs_columns), recursive = FALSE)
}

# Every field the set's entries name on one level: without data, the catalogue says what a level holds.
catalogued_keys <- function(set, level) {
  entries <- unlist(lapply(set$sources(), function(source) source$entries), recursive = FALSE)
  named <- Filter(
    function(entry) entry$reference$level %in% c(level, ANY_VERSION) && !is.na(entry$reference$name),
    entries
  )
  unique(vapply(named, function(entry) reference_key(entry$reference), character(1)))
}

# The apps' entries the level holds, and the entries of apps that have not run.
producer_entries <- function(producers, rule_level, retained, obs_columns, ran) {
  entries <- list()
  pending <- list()
  for (producer in producers) {
    offered <- bound_entries(producer, rule_level, obs_columns)
    if (ran(producer)) {
      entries <- c(entries, held_entries(offered, retained))
    } else {
      reason <- pending_reason(producer)
      pending <- c(pending, lapply(offered, function(entry) list(entry = entry, reason = reason)))
    }
  }
  list(entries = entries, pending = pending)
}

annotation_entries <- function(annotations, rule_level, obs_columns) {
  unlist(lapply(annotations, bound_entries, rule_level, obs_columns), recursive = FALSE)
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

    # The request for a key, or for every key when key is NULL, narrowed by further qualifiers; a qualifier naming the
    # kind or a value the key already fixes is an error.
    request = function(level, location, key, qualifiers) {
      fixed <- if (is.null(key)) list() else key_predicate(self$set$key(key))
      named <- c("kind", names(fixed))
      clashing <- named[named %in% names(qualifiers)]
      if (length(clashing) > 0L) {
        stop(
          sprintf("ask by key; %s fixes %s", key %||% "the lookup", paste(clashing, collapse = ", ")),
          call. = FALSE
        )
      }
      field_request(level, location, key, c(fixed, lapply(qualifiers, unname)))
    },

    answer = function(request) {
      check_predicate(self$set$vocabulary, request$qualifiers)
      view <- self$views[[request$level]] %||% AbsentLevel$new(request$level)
      view$resolve(request)
    },

    # The answer; a missing required field signals or warns as enforce says.
    lookup = function(level, location, key, qualifiers) {
      resolution <- self$answer(self$request(level, location, key, qualifiers))
      row <- if (resolution$status == "missing" && !is.null(self$enforce)) {
        self$set$requirement_for(level, location, key)
      }
      if (!is.null(row) && row$requirement != "MAY") {
        enforce_requirement(resolution, row, self$enforce)
      }
      resolution
    },

    # The one field asked for, NULL when the level has none.
    find = function(level, location, key, qualifiers) {
      self$lookup(level, location, key, qualifiers)$found()
    },

    # Every field one key names on a level.
    find_all = function(level, location, key) {
      answer_fields(self$lookup(level, location, key, list()), self$views[[level]])
    },

    keys = function(level, location, qualifiers) {
      held_keys(self$answer(self$request(level, location, NULL, qualifiers)), self$views[[level]])
    },

    rows = function(view, entries) {
      lapply(entries, function(entry) {
        pattern <- view$software_version_pattern %||% NA_character_
        entry_row(self$name, self$set, entry_source(entry), pattern, entry)
      })
    }
  )
)

# The keys a field the level holds answers; a later producer's keys wait until it runs, so every key listed reads.
held_keys <- function(resolution, view) {
  held <- vapply(view$entries %||% list(), function(entry) reference_key(entry$reference), character(1))
  answering <- vapply(
    Filter(function(candidate) reference_key(candidate$reference) %in% held, resolution$candidates),
    function(candidate) candidate$key,
    character(1)
  )
  offered <- resolution$offered()
  offered[offered %in% answering]
}

# Every field an answer names: none when missing, one when resolved, each match when ambiguous. A field the key may
# name that is undecided, or whose producer has not run, signals UnresolvedField.
answer_fields <- function(resolution, view) {
  if (resolution$status != "ambiguous") {
    found <- resolution$found()
    return(if (is.null(found)) list() else list(found))
  }
  answering <- vapply(view$entries, function(entry) reference_key(entry$reference), character(1))
  for (candidate in resolution$candidates) {
    if (length(candidate$undecided) > 0L || !(reference_key(candidate$reference) %in% answering)) {
      stop(unresolved_field(resolution))
    }
  }
  lapply(resolution$candidates, function(candidate) candidate$reference)
}

qualifier_arguments <- function(...) {
  qualifiers <- list(...)
  if (length(qualifiers) > 0L && (is.null(names(qualifiers)) || any(names(qualifiers) == ""))) {
    stop("qualifiers are named, such as unit = \"minute\"", call. = FALSE)
  }
  qualifiers
}

fields_frame <- function(rows, set) {
  records_frame(rows, c(set_columns(set), "present", "constant"))
}

#' Which column one software version reports for each meaning
#'
#' Answers from the catalogue alone, without data. The rule levels whose version pattern admits the version give the
#' levels; the set's entries for that pattern give each level's fields. `levels` have the slots of a [DataCatalog]'s
#' levels, holding names. The methods ask by one of the set's keys, such as `"normalized_intensity"`, and return a
#' [Reference]; further qualifiers, such as `unit`, narrow them. Lookups that several fields answer, or that an
#' unreviewed level cannot, signal `UnresolvedField`.
#'
#' @field description What this catalogue set holds and who uses it.
#' @return The class generator; `Catalog$new()` returns a `Catalog`.
#' @export
#' @examples
#' catalog <- Catalog$new("DIA-NN", "2.2.0", "abundance")
#' catalog$levels$peptidoform_ion$layers[["intensity"]]
#' catalog$layer("peptidoform_ion", "intensity")
#' catalog$keys("peptidoform_ion", "layers")
Catalog <- R6::R6Class(
  "Catalog",
  public = list(
    #' @field levels One level per rule level, with a [DataCatalog]'s slots holding names.
    levels = NULL,

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
          retained = catalogued_keys(set, found$level),
          obs_columns = character(0),
          ran = function(producer) length(producer$requires) == 0L,
          annotations = list()
        )
      }
      private$lookups <- Lookups$new(catalog, set, views, enforce)
      lookup <- list(
        find = function(level, location, key) private$lookups$find(level, location, key, list()),
        find_all = function(level, location, key) private$lookups$find_all(level, location, key),
        offered = function(level, location) self$keys(level, location),
        known = names(set$keys)
      )
      self$levels <- stats::setNames(lapply(names(views), LevelNames$new, lookup = lookup), names(views))
    },

    #' @description The keys the level offers at one location, given further qualifiers.
    #' @param level The quantification level.
    #' @param location `"layers"`, `"var"` or `"obs"`.
    #' @param ... Further qualifiers by name, such as `unit = "minute"`.
    #' @return A character vector.
    keys = function(level, location, ...) {
      private$lookups$keys(level, location, qualifier_arguments(...))
    },

    #' @description Where the sample-by-feature layer with this meaning lives.
    #' @param level The quantification level.
    #' @param key The key, such as `"intensity"`; see [catalogue_table()].
    #' @param ... Further qualifiers by name, besides those the key fixes.
    #' @return A [Reference], or `NULL` when the level has no such layer.
    layer = function(level, key, ...) {
      private$found(level, "layers", key, qualifier_arguments(...))
    },

    #' @description Where the per-feature column with this meaning lives, in `var` or a `varm` table.
    #' @param level The quantification level.
    #' @param key The key.
    #' @param ... Further qualifiers by name, besides those the key fixes.
    #' @return A [Reference], or `NULL`.
    var = function(level, key, ...) {
      private$found(level, "var", key, qualifier_arguments(...))
    },

    #' @description Where the per-sample column with this meaning lives.
    #' @param level The quantification level.
    #' @param key The key.
    #' @param ... Further qualifiers by name, besides those the key fixes.
    #' @return A [Reference], or `NULL`.
    obs = function(level, key, ...) {
      private$found(level, "obs", key, qualifier_arguments(...))
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
    found = function(level, location, key, qualifiers) {
      found <- private$lookups$find(level, location, key, qualifiers)
      if (is.null(found)) NULL else reference_of(found)
    }
  )
)

#' What one result's fields mean to one consumer
#'
#' Answers on a result read with [read_parsed_levels()] and hands the data back directly. A lookup asks by one of the
#' set's keys, such as `"q_value"` or Sage's `"quantification_q_value"`. Ambiguous or unreviewed answers signal
#' `UnresolvedField`.
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
#' dim(catalog$layer("peptidoform_ion", "pep"))
DataCatalog <- R6::R6Class(
  "DataCatalog",
  public = list(
    #' @field levels One [Level] per level of the result, with anndataR's slots looked up by key.
    levels = NULL,

    #' @description Bind catalogue sets to the levels of one result; `apb`, APB's own fields, is always in.
    #' @param result A [ParsedLevels] object.
    #' @param ... The catalogue sets, such as `"abundance"` and `"confidence"`; see [catalogues()].
    #' @param enforce `NULL`, `"MUST"` or `"SHOULD"`: signal `MissingRequiredField` when a field a set requires
    #'   at that strength is missing.
    initialize = function(result, ..., enforce = NULL) {
      if (!inherits(result, "ParsedLevels")) {
        stop("result is a ParsedLevels, as read_parsed_levels() returns", call. = FALSE)
      }
      private$parsed <- result
      names <- unique(c(as.character(c(...)), BASE))
      private$sets <- stats::setNames(lapply(names, function(name) data_lookups(result, name, enforce)), names)
      check_composed_enforce(private$sets, enforce)
      check_shared_keys(private$sets)
      private$known <- unique(unlist(lapply(private$sets, function(lookups) names(lookups$set$keys))))
      lookup <- list(
        find = function(level, location, key) private$find(level, location, key, list()),
        find_all = function(level, location, key) composed_fields(private$sets, private$known, level, location, key),
        offered = function(level, location) self$keys(level, location),
        known = private$known
      )
      self$levels <- stats::setNames(
        lapply(names(result$levels), function(name) Level$new(name, result$levels[[name]], lookup)),
        names(result$levels)
      )
    },

    #' @description The keys the level offers at one location, given further qualifiers, from every set that
    #'   declares those qualifiers.
    #' @param level The quantification level.
    #' @param location `"layers"`, `"var"` or `"obs"`.
    #' @param ... Further qualifiers by name, besides those the key fixes.
    #' @return A character vector.
    keys = function(level, location, ...) {
      composed_keys(private$sets, level, location, qualifier_arguments(...))
    },

    #' @description The layer with this meaning.
    #' @param level The quantification level.
    #' @param key The key, such as `"pep"`; see [catalogue_table()].
    #' @param ... Further qualifiers by name, besides those the key fixes.
    #' @return An observations-by-features matrix, or `NULL` when the result has no such layer.
    layer = function(level, key, ...) {
      reference <- private$find(level, "layers", key, qualifier_arguments(...))
      if (is.null(reference)) NULL else level_layer(private$parsed$levels[[level]], reference$name)
    },

    #' @description The per-feature column with this meaning, from `var` or a `varm` table.
    #' @param level The quantification level.
    #' @param key The key.
    #' @param ... Further qualifiers by name, besides those the key fixes.
    #' @return A vector, or `NULL`.
    var = function(level, key, ...) {
      reference <- private$find(level, "var", key, qualifier_arguments(...))
      if (is.null(reference)) {
        return(NULL)
      }
      found <- private$parsed$levels[[level]]
      if (is.na(reference$table)) found$var[[reference$name]] else found$varm[[reference$table]][[reference$name]]
    },

    #' @description The per-sample column with this meaning.
    #' @param level The quantification level.
    #' @param key The key.
    #' @param ... Further qualifiers by name, besides those the key fixes.
    #' @return A vector, or `NULL`.
    obs = function(level, key, ...) {
      reference <- private$find(level, "obs", key, qualifier_arguments(...))
      if (is.null(reference)) NULL else private$parsed$levels[[level]]$obs[[reference$name]]
    },

    #' @description Every catalogued field of the result in every composed set, one row per field, as
    #'   [catalogue_table()] shows them.
    #' @return A data frame with `present` and, for a field apb2 left out because it held one value, that
    #'   `constant`.
    fields = function() {
      rows <- list()
      for (lookups in private$sets) {
        rows <- c(
          rows,
          unlist(
            lapply(names(lookups$views), function(name) {
              level_fields(lookups, lookups$views[[name]], private$parsed$levels[[name]])
            }),
            recursive = FALSE
          )
        )
      }
      # As polars builds a frame from records: the columns in the order the rows first name them.
      columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
      if (is.null(columns)) {
        columns <- c(set_columns(private$sets[[1L]]$set), "present", "constant")
      }
      records_frame(rows, columns)
    },

    #' @description Print the set and the levels.
    #' @param ... Ignored.
    print = function(...) {
      writeLines(sprintf(
        "<DataCatalog %s: %s>",
        paste(names(private$sets), collapse = ", "),
        paste(names(private$parsed$levels), collapse = ", ")
      ))
      invisible(self)
    }
  ),
  active = list(
    description = function() private$sets[[1L]]$set$description
  ),
  private = list(
    sets = NULL,
    known = NULL,
    parsed = NULL,

    # The field one key names, asked of every set that defines the key; the first found.
    find = function(level, location, key, qualifiers) {
      found <- NULL
      for (lookups in defining_sets(private$sets, private$known, key, qualifiers)) {
        reference <- lookups$find(level, location, key, qualifiers)
        if (is.null(found)) {
          found <- reference
        }
      }
      found
    }
  )
)

# The set every data catalog composes: APB's own fields, so obs and var always answer.
BASE <- "apb"

# One set bound to every level of a result; enforce applies where the set has requirements.
data_lookups <- function(result, name, enforce) {
  set <- catalogue_set(name)
  annotations <- Filter(function(annotation) annotation_applies(annotation, result$metadata), set$annotations)
  views <- list()
  for (level_name in names(result$levels)) {
    level <- result$levels[[level_name]]
    version <- level_rule_version(level)
    views[[level_name]] <- level_view(
      level_name,
      version$software,
      version$pattern,
      version$level,
      set,
      retained = level_retained(level),
      obs_columns = colnames(level$obs),
      ran = function(producer) producer_ran(producer, result$metadata),
      annotations = annotations
    )
  }
  Lookups$new(name, set, views, if (length(set$requirements) > 0L) enforce else NULL)
}

# Whether a set declares every qualifier a request names.
declares <- function(lookups, qualifiers) {
  all(names(qualifiers) %in% names(lookups$set$vocabulary$qualifiers))
}

# enforce needs a composed set with requirements.
check_composed_enforce <- function(sets, enforce) {
  has_requirements <- vapply(sets, function(lookups) length(lookups$set$requirements) > 0L, logical(1))
  if (!is.null(enforce) && !any(has_requirements)) {
    stop(
      sprintf("catalogues %s set no requirements, so nothing to enforce", paste(names(sets), collapse = ", ")),
      call. = FALSE
    )
  }
}

# The keys a level offers at one location in every set that declares the qualifiers asked.
composed_keys <- function(sets, level, location, qualifiers) {
  eligible <- Filter(function(lookups) declares(lookups, qualifiers), sets)
  if (length(eligible) == 0L) {
    stop(
      sprintf("no catalogue set declares the qualifiers %s", paste(sort_c(names(qualifiers)), collapse = ", ")),
      call. = FALSE
    )
  }
  offered <- unlist(lapply(eligible, function(lookups) lookups$keys(level, location, qualifiers)), use.names = FALSE)
  if (is.null(offered)) character(0) else sort_c(unique(offered))
}

# Every field one key names, from every set that defines the key.
composed_fields <- function(sets, known, level, location, key) {
  found <- list()
  for (lookups in defining_sets(sets, known, key, list())) {
    for (reference in lookups$find_all(level, location, key)) {
      found[[reference_key(reference)]] <- found[[reference_key(reference)]] %||% reference
    }
  }
  unname(found)
}

# The sets that define a key and declare every further qualifier asked; the first that defines it when none declares
# them, so its lookup names what it lacks.
defining_sets <- function(sets, known, key, qualifiers) {
  found <- Filter(function(lookups) key %in% names(lookups$set$keys), sets)
  if (length(found) == 0L) {
    stop(sprintf("unknown key '%s'; keys: %s", key, paste(sort_c(known), collapse = ", ")), call. = FALSE)
  }
  declaring <- Filter(function(lookups) declares(lookups, qualifiers), found)
  if (length(declaring) > 0L) declaring else found[1L]
}

# Whether every set's fields for one key contain, or are contained in, every other set's.
nested_fields <- function(fields) {
  all(vapply(
    fields,
    function(one) all(vapply(fields, function(other) all(one %in% other) || all(other %in% one), logical(1))),
    logical(1)
  ))
}

# Sets that share a key name the same fields with it, level by level and slot by slot; a set may name more, told
# apart by a qualifier the other set lacks.
check_shared_keys <- function(sets) {
  named <- list()
  for (set_name in names(sets)) {
    for (level in names(sets[[set_name]]$views)) {
      for (entry in sets[[set_name]]$views[[level]]$entries) {
        location <- if (entry$reference$location == "varm") "var" else entry$reference$location
        slot <- paste(level, entry$key, location, sep = "\x1f")
        named[[slot]][[set_name]] <- unique(c(named[[slot]][[set_name]], reference_key(entry$reference)))
      }
    }
  }
  for (slot in names(named)) {
    fields <- named[[slot]]
    if (!nested_fields(fields)) {
      parts <- strsplit(slot, "\x1f", fixed = TRUE)[[1L]]
      stop(
        sprintf(
          "key '%s' names different %s fields on %s in the sets %s",
          parts[[2L]],
          parts[[3L]],
          parts[[1L]],
          paste(sort_c(names(fields)), collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
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
