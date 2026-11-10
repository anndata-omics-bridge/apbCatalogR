# Look up APB fields by meaning, so callers never name vendor columns;
# apb_catalog.catalog.

# Bind one level to its set: the reviewed vendor entries, then the apps' and
# annotations'. Only entries whose field the level holds answer; an app that has
# not run leaves its entries pending. A set with vendor catalogues answers only
# for a reviewed software version. `ran` tells, for the set's producers, which
# have run; `conventions` are the annotation conventions the result records.
level_view <- function(
    name,
    software,
    pattern,
    rule_level,
    set,
    retained,
    obs_columns,
    ran,
    conventions
) {
    if (is.null(software) || is.null(pattern) || is.null(rule_level)) {
        return(UnreviewedLevel$new(
            name,
            NULL,
            NULL,
            "the level records no rule_json software version"
        ))
    }
    reviewed <- set$reviewed(software, pattern, rule_level)
    vendor <- set$entries[0L, ]
    if (set$needs_review) {
        if (!reviewed) {
            unreviewed <- paste(software, pattern, rule_level)
            reason <- paste("no source catalogue reviewed", unreviewed)
            return(UnreviewedLevel$new(name, software, pattern, reason))
        }
        vendor <- held_entries(
            entries_for(set, software, pattern, rule_level),
            retained
        )
    }
    produced <- producer_entries(set, rule_level, retained, obs_columns, ran)
    annotated <- filter(
        level_entries(set, "annotation", rule_level, obs_columns),
        source_convention(.data$source) %in% conventions
    )
    entries <- bind_rows(vendor, produced$entries, annotated)
    if (reviewed) {
        rule <- list(software = software, pattern = pattern, level = rule_level)
        return(ReviewedLevel$new(name, rule, entries, produced$pending))
    }
    ProducedLevel$new(name, software, pattern, entries, produced$pending)
}

# The entries of a set's sources of one type on a level, bound to its obs
# columns.
level_entries <- function(set, type, level, obs_columns) {
    set$entries |>
        filter(.data$type == .env$type) |>
        entries_on(level) |>
        bind_entries(obs_columns)
}

# The apps' entries on a level: those of apps that have run whose field the
# level holds, and the `pending` entries of apps that have not, with the
# `reason` they wait.
producer_entries <- function(set, level, retained, obs_columns, ran) {
    producers <- set$producers
    run <- producers$producer[ran(producers)]
    offered <- level_entries(set, "producer", level, obs_columns)
    pending <- offered |>
        filter(!(.data$source %in% run)) |>
        mutate(
            reason = pending_reason(
                .data$source,
                producers$instruction[match(.data$source, producers$producer)]
            )
        )
    list(
        entries = held_entries(
            filter(offered, .data$source %in% run),
            retained
        ),
        pending = pending
    )
}

# Every field the set's entries name on one level: without data, the catalogue
# says what a level holds.
catalogued_keys <- function(set, level) {
    named <- filter(
        set$entries,
        .data$level %in% c(.env$level, ANY_VERSION),
        !is.na(.data$name)
    )
    unique(entry_addresses(named))
}

check_enforce <- function(enforce) {
    if (!is.null(enforce) && !(enforce %in% c("required", "recommended"))) {
        stop(
            "enforce is NULL, \"required\" or \"recommended\"",
            call. = FALSE
        )
    }
}

# Check the named sets' requirements once, when the catalog opens. Under
# enforce = "required" the missing recommended fields give one
# MissingRequiredField warning; under "recommended" they signal with the
# required ones. An error when no named set has requirement rows.
enforce_sets <- function(sets, enforce) {
    if (is.null(enforce)) {
        return(invisible(NULL))
    }
    binding <- Filter(
        function(lookups) nrow(lookups$set$requirements) > 0L,
        sets
    )
    if (length(binding) == 0L) {
        names <- vapply(sets, function(lookups) lookups$name, character(1))
        stop(
            sprintf(
                "catalogues %s set no requirements, so nothing to enforce",
                paste(names, collapse = ", ")
            ),
            call. = FALSE
        )
    }
    missing <- do.call(
        c,
        unname(lapply(binding, function(lookups) lookups$missing()))
    )
    required <- vapply(
        missing,
        function(found) found$row$requirement == "required",
        logical(1)
    )
    warned <- missing[enforce == "required" & !required]
    raising <- missing[enforce == "recommended" | required]
    if (length(warned) > 0L) {
        warning(missing_required_warning(warned))
    }
    if (length(raising) > 0L) {
        stop(missing_required_field(raising))
    }
    invisible(NULL)
}

# The requests, answers and rows both catalogues share for one set.
Lookups <- R6Class(
    "Lookups",
    public = list(
        name = NULL,
        set = NULL,
        views = NULL,

        initialize = function(name, set, views) {
            self$name <- name
            self$set <- set
            self$views <- views
        },

        # The request for a key, or for every key when key is NULL, narrowed by
        # further qualifiers; a qualifier naming the kind or a value the key
        # already fixes is an error.
        request = function(level, location, key, qualifiers) {
            found <- if (is.null(key)) NULL else self$set$key(key)
            fixed <- if (is.null(found)) list() else key_accepted(found)
            named <- c("kind", names(fixed))
            clashing <- named[named %in% names(qualifiers)]
            if (length(clashing) > 0L) {
                stop(
                    sprintf(
                        "ask by key; %s fixes %s",
                        key %||% "the lookup",
                        paste(clashing, collapse = ", ")
                    ),
                    call. = FALSE
                )
            }
            field_request(
                level,
                location,
                key,
                c(fixed, lapply(qualifiers, unname)),
                kind = found$kind
            )
        },

        answer = function(request) {
            check_predicate(
                self$set$vocabulary,
                request$kind,
                request$qualifiers
            )
            view <- self$views[[request$level]] %||%
                AbsentLevel$new(request$level)
            view$resolve(request)
        },

        # The answer to one key; requirements were checked when the catalog
        # opened.
        lookup = function(level, location, key, qualifiers) {
            self$answer(self$request(level, location, key, qualifiers))
        },

        # Every required or recommended field a bound level lacks, each as its
        # resolution, with its requirement set, and its requirement row; a field
        # the catalogue cannot decide is not missing.
        missing = function() {
            rows <- filter(
                self$set$requirements,
                .data$requirement != "optional",
                .data$level %in% names(self$views)
            )
            found <- lapply(seq_len(nrow(rows)), function(index) {
                row <- as.list(rows[index, ])
                resolution <- self$lookup(
                    row$level,
                    row$location,
                    row$key,
                    list()
                )
                if (resolution$status != "missing") {
                    return(NULL)
                }
                resolution$requirement <- row$requirement
                list(resolution = resolution, row = row)
            })
            Filter(Negate(is.null), found)
        },

        # The one field asked for, NULL when the level has none.
        find = function(level, location, key, qualifiers) {
            self$lookup(level, location, key, qualifiers)$found()
        },

        # Every field one key names on a level.
        find_all = function(level, location, key) {
            answer_fields(
                self$lookup(level, location, key, list()),
                self$views[[level]]
            )
        },

        keys = function(level, location, qualifiers) {
            held_keys(
                self$answer(self$request(level, location, NULL, qualifiers)),
                self$views[[level]]
            )
        },

        # How this set alone answers keys, as a name with its prefix asks it.
        alone = function() {
            list(
                find = function(level, location, key, qualifiers) {
                    self$find(level, location, key, qualifiers)
                },
                find_all = function(level, location, key) {
                    self$find_all(level, location, key)
                },
                offered = function(level, location) {
                    self$keys(level, location, list())
                },
                known = self$set$keys$key,
                sets = list()
            )
        },

        # The table rows of entries on a view, with the view's software version
        # pattern.
        rows = function(view, entries) {
            pattern <- view$software_version_pattern %||% NA_character_
            entry_rows(self$set, entries, pattern)
        }
    )
)

# The keys a field the level holds answers; a later producer's keys wait until
# it runs, so every key listed reads.
held_keys <- function(resolution, view) {
    candidates <- resolution$candidates
    held <- entry_addresses(view$entries)
    answering <- candidates$key[entry_addresses(candidates) %in% held]
    offered <- resolution$offered()
    offered[offered %in% answering]
}

# Every field an answer names: none when missing, one when resolved, each match
# when ambiguous. A field the key may name that is undecided, or whose producer
# has not run, signals UnresolvedField.
answer_fields <- function(resolution, view) {
    if (resolution$status != "ambiguous") {
        found <- resolution$found()
        return(if (is.null(found)) list() else list(found))
    }
    candidates <- resolution$candidates
    answering <- entry_addresses(view$entries)
    if (
        any(lengths(candidates$undecided) > 0L) ||
            !all(entry_addresses(candidates) %in% answering)
    ) {
        stop(unresolved_field(resolution))
    }
    entry_references(candidates)
}

qualifier_arguments <- function(...) {
    qualifiers <- list(...)
    if (
        length(qualifiers) > 0L &&
            (is.null(names(qualifiers)) || any(names(qualifiers) == ""))
    ) {
        stop("qualifiers are named, such as unit = \"minute\"", call. = FALSE)
    }
    qualifiers
}

# The rows of views, with every column of the set and `present` and `constant`.
fields_frame <- function(rows, set) {
    column_frame(rows, c(set_columns(set), "present", "constant"))
}

# The table rows of the fields a view holds.
present_rows <- function(lookups, view) {
    entries <- view$entries %||% lookups$set$entries[0L, ]
    mutate(
        lookups$rows(view, entries),
        present = TRUE,
        constant = NA_character_
    )
}

#' Which column one software version reports for each meaning
#'
#' Answers from the catalogue alone, without data. The rule levels whose version
#' pattern admits the version give the levels; the set's entries for that
#' pattern give each level's fields. `levels` have the slots of a
#' [DataCatalog]'s levels, holding names: a slot answers one of the set's keys,
#' such as `"intensity_norm_settings"`, with the field's name, and its `$get()`
#' takes further qualifiers, such as `unit`. Lookups that several fields answer,
#' or that an unreviewed level cannot, signal `UnresolvedField`. `enforce`
#' checks the set's requirements against the rule's levels once, when the
#' catalog opens.
#'
#' @field description What this catalogue set holds and who uses it.
#' @return The class generator; `Catalog$new()` returns a `Catalog`.
#' @export
#' @examples
#' catalog <- Catalog$new("DIA-NN", "2.2.0", "abundance_alpha")
#' catalog$levels$peptidoform_ion$layers[["intensity"]]
#' catalog$levels$peptidoform_ion$layers$keys()
Catalog <- R6Class(
    "Catalog",
    public = list(
        #' @field levels One level per rule level, with a [DataCatalog]'s slots
        #'   holding names.
        levels = NULL,

        #' @description Bind a catalogue set to the rule levels of one software
        #'   version.
        #' @param software The software name, such as `"DIA-NN"`.
        #' @param version The software version, such as `"2.2.0"`.
        #' @param catalog The catalogue set, such as `"abundance_alpha"` or
        #'   `"confidence_alpha"`; see [catalogues()].
        #' @param enforce `NULL`, `"required"` or `"recommended"`: signal one
        #'   `MissingRequiredField` listing every field the set requires at
        #'   that strength and the rule lacks; under `"required"`, missing
        #'   recommended fields give one warning. An error when the set has no
        #'   requirements.
        initialize = function(software, version, catalog, enforce = NULL) {
            check_enforce(enforce)
            set <- catalogue_set(catalog)
            matched <- rules_matching(software, version)
            check_rules_matched(matched, software, version)
            views <- catalog_views(set, matched)
            private$lookups <- Lookups$new(catalog, set, views)
            enforce_sets(list(private$lookups), enforce)
            lookup <- list(
                find = function(level, location, key, qualifiers) {
                    private$lookups$find(level, location, key, qualifiers)
                },
                find_all = function(level, location, key) {
                    private$lookups$find_all(level, location, key)
                },
                offered = function(level, location) {
                    private$lookups$keys(level, location, list())
                },
                known = set$keys$key,
                sets = setNames(
                    list(private$lookups$alone()),
                    set$vocabulary$prefix
                )
            )
            self$levels <- setNames(
                lapply(names(views), LevelNames$new, lookup = lookup),
                names(views)
            )
        },

        #' @description Every catalogued field the rule declares, one row per
        #'   field, as [catalogue_table()] shows them.
        #' @return A data frame, with `present` true for each row.
        fields = function() {
            lookups <- private$lookups
            rows <- lapply(lookups$views, function(view) {
                present_rows(lookups, view)
            })
            fields_frame(bind_rows(rows), lookups$set)
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
        lookups = NULL
    )
)

# One view per rule level a software version matches, named by its level; of
# two rule levels naming one level, the later answers.
catalog_views <- function(set, matched) {
    levels <- unique(matched$level)
    matched <- matched[!duplicated(matched$level, fromLast = TRUE), ]
    matched <- matched[match(levels, matched$level), ]
    setNames(
        lapply(seq_len(nrow(matched)), function(index) {
            level <- matched$level[[index]]
            level_view(
                level,
                matched$software_name[[index]],
                matched$software_version_pattern[[index]],
                level,
                set,
                retained = catalogued_keys(set, level),
                obs_columns = character(0),
                ran = function(producers) lengths(producers$requires) == 0L,
                conventions = character(0)
            )
        }),
        levels
    )
}

# No packaged rule admitting a software version signals an error naming the
# version patterns the software's rules have.
check_rules_matched <- function(matched, software, version) {
    if (nrow(matched) > 0L) {
        return(invisible())
    }
    levels <- rule_levels()
    known <- unique(levels$software_version_pattern[
        tolower(levels$software_name) == tolower(software)
    ])
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

#' What one result's fields mean to one consumer
#'
#' Answers on a result read with [read_parsed_levels()] and hands the data back
#' directly: `levels` give each level's slots, looked up by one of the set's
#' keys, such as `"q_value"` or Sage's `"quantification_q_value"`. Ambiguous or
#' unreviewed answers signal `UnresolvedField`.
#'
#' Composes the named sets alone: `apb_alpha`, the fields apb2 convert writes,
#' answers only when named. A key routes to the sets that define it; two sets
#' naming different fields for one key on one level is an error. With no set
#' named, every name is the level's own. A named set that can answer nothing on
#' the result, because its app has not run or the result records no sample
#' annotation, is an error saying what to run. `enforce` checks the
#' requirements once, here.
#'
#' `DataCatalog$answering_sets(result)` returns the sets
#' `DataCatalog$new(result, name)` opens, as a character vector: every set but
#' one whose app has not run on the result or whose annotation it does not
#' record. It signals no error for such a set, so a consumer can leave it out.
#'
#' @field description What the first catalogue set named holds and who uses
#'   it; an error when no set was named.
#' @return The class generator; `DataCatalog$new()` returns a `DataCatalog`.
#' @export
#' @examples
#' results <- tempfile()
#' archive <- system.file(
#'     "extdata", "test-bundle", "results.tar.gz",
#'     package = "apbCatalogR"
#' )
#' utils::untar(archive, exdir = results)
#' result <- read_parsed_levels(
#'     file.path(results, "maxquant_wide-peptidoform_ion.parquet")
#' )
#' catalog <- DataCatalog$new(result, "confidence_alpha")
#' dim(catalog$levels$peptidoform_ion$layers[["pep"]])
#' catalog$levels$peptidoform_ion$layers$name("pep")
DataCatalog <- R6Class(
    "DataCatalog",
    public = list(
        #' @field levels One [Level] per level of the result, with anndataR's
        #'   slots looked up by key.
        levels = NULL,

        #' @description Bind the named catalogue sets, and no others, to the
        #'   levels of one result.
        #' @param result A [ParsedLevels] object.
        #' @param ... The catalogue sets, such as `"abundance_alpha"` and
        #'   `"confidence_alpha"`; see [catalogues()].
        #' @param enforce `NULL`, `"required"` or `"recommended"`: signal one
        #'   `MissingRequiredField` listing every field the named sets require
        #'   at that strength and the result lacks; under `"required"`, missing
        #'   recommended fields give one warning. An error when no named set
        #'   has requirements.
        initialize = function(result, ..., enforce = NULL) {
            if (!inherits(result, "ParsedLevels")) {
                stop(
                    "result is a ParsedLevels, as read_parsed_levels() returns",
                    call. = FALSE
                )
            }
            check_enforce(enforce)
            private$parsed <- result
            names <- unique(as.character(c(...)))
            private$sets <- setNames(
                lapply(names, function(name) data_lookups(result, name)),
                names
            )
            check_shared_keys(private$sets)
            private$known <- unique(unlist(
                lapply(private$sets, function(lookups) lookups$set$keys$key),
                use.names = FALSE
            )) %||%
                character(0)
            enforce_sets(private$sets, enforce)
            lookup <- composed_lookup(private$sets, private$known)
            self$levels <- setNames(
                lapply(names(result$levels), function(name) {
                    Level$new(name, result$levels[[name]], lookup)
                }),
                names(result$levels)
            )
        },

        #' @description Every catalogued field of the result in every composed
        #'   set, one row per field, as [catalogue_table()] shows them.
        #' @return A data frame with `present` and, for a field apb2 left out
        #'   because it held one value, that `constant`.
        fields = function() {
            rows <- lapply(private$sets, function(lookups) {
                bind_rows(lapply(names(lookups$views), function(name) {
                    level_fields(
                        lookups,
                        lookups$views[[name]],
                        private$parsed$levels[[name]]
                    )
                }))
            })
            # As polars builds a frame from records: the columns in the order
            # the rows first name them.
            rows <- Filter(function(found) nrow(found) > 0L, rows)
            if (length(rows) > 0L) {
                return(as.data.frame(bind_rows(rows)))
            }
            if (length(private$sets) == 0L) {
                return(data.frame())
            }
            fields_frame(NULL, private$sets[[1L]]$set)
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
        description = function() private$first()$set$description
    ),
    private = list(
        sets = NULL,
        known = NULL,
        parsed = NULL,

        # The first set named; an error when no set was named.
        first = function() {
            if (length(private$sets) == 0L) {
                stop("no catalogue set named", call. = FALSE)
            }
            private$sets[[1L]]
        }
    )
)

# How a DataCatalog's levels answer keys from its composed sets: `find`,
# `find_all`, `offered`, `known` and one lookup per set prefix.
composed_lookup <- function(sets, known) {
    list(
        find = function(level, location, key, qualifiers) {
            composed_find(sets, known, level, location, key, qualifiers)
        },
        find_all = function(level, location, key) {
            composed_fields(sets, known, level, location, key)
        },
        offered = function(level, location) {
            composed_keys(sets, level, location)
        },
        known = known,
        sets = setNames(
            lapply(sets, function(lookups) lookups$alone()),
            vapply(
                sets,
                function(lookups) lookups$set$vocabulary$prefix,
                character(1)
            )
        )
    )
}

# The field one key names, asked of every set that defines the key; the first
# found.
composed_find <- function(sets, known, level, location, key, qualifiers) {
    references <- lapply(
        defining_sets(sets, known, key, qualifiers),
        function(lookups) lookups$find(level, location, key, qualifiers)
    )
    Find(Negate(is.null), references)
}

# The sets DataCatalog$new(result, name) opens: see answers().
DataCatalog$answering_sets <- function(result) {
    if (!inherits(result, "ParsedLevels")) {
        stop(
            "result is a ParsedLevels, as read_parsed_levels() returns",
            call. = FALSE
        )
    }
    Filter(
        function(name) answers(catalogue_set(name), result$metadata),
        set_names()
    )
}

# One set bound to every level of a result.
data_lookups <- function(result, name) {
    set <- catalogue_set(name)
    check_answers(name, set, result$metadata)
    conventions <- set$conventions[set$conventions %in% names(result$metadata)]
    views <- setNames(
        lapply(names(result$levels), function(level_name) {
            level <- result$levels[[level_name]]
            version <- level_rule_version(level)
            level_view(
                level_name,
                version$software,
                version$pattern,
                version$level,
                set,
                retained = level_retained(level),
                obs_columns = colnames(level$obs),
                ran = function(producers) {
                    producers_ran(producers, result$metadata)
                },
                conventions = conventions
            )
        }),
        names(result$levels)
    )
    Lookups$new(name, set, views)
}

# Which of a set's producers have run on a result.
producers_ran <- function(producers, metadata) {
    vapply(producers$requires, producer_ran, logical(1), metadata = metadata)
}

# Whether a set can answer on a result: it has vendor catalogues, one of its
# apps has run, or the result records one of its annotation conventions.
answers <- function(set, metadata) {
    set$needs_review ||
        any(set$conventions %in% names(metadata)) ||
        any(producers_ran(set$producers, metadata))
}

# A set needs a source the result answers from, as answers() decides. An
# error, saying what to run, when it can answer nothing.
check_answers <- function(name, set, metadata) {
    if (answers(set, metadata)) {
        return(invisible(NULL))
    }
    reasons <- pending_reason(set$producers$producer, set$producers$instruction)
    if (length(set$conventions) > 0L) {
        reasons <- c(
            reasons,
            sprintf(
                "the result records no sample annotation (%s); %s",
                paste(sort_c(set$conventions), collapse = ", "),
                "run apb2 annotate"
            )
        )
    }
    if (length(reasons) == 0L) {
        reasons <- "no source of the set applies to the result"
    }
    stop(
        sprintf("%s: %s", name, paste(reasons, collapse = "; ")),
        call. = FALSE
    )
}

# Whether a set declares every qualifier a request names.
declares <- function(lookups, qualifiers) {
    all(names(qualifiers) %in% names(lookups$set$vocabulary$qualifiers))
}

# The keys a level offers at one location, from every set.
composed_keys <- function(sets, level, location) {
    offered <- unlist(
        lapply(sets, function(lookups) lookups$keys(level, location, list())),
        use.names = FALSE
    )
    if (is.null(offered)) character(0) else sort_c(unique(offered))
}

# Every field one key names, from every set that defines the key, each once.
composed_fields <- function(sets, known, level, location, key) {
    found <- unlist(
        lapply(defining_sets(sets, known, key, list()), function(lookups) {
            lookups$find_all(level, location, key)
        }),
        recursive = FALSE
    )
    addresses <- vapply(found, reference_key, character(1))
    unname(found[!duplicated(addresses)])
}

# The sets that define a key and declare every further qualifier asked; the
# first that defines it when none declares them, so its lookup names what it
# lacks.
defining_sets <- function(sets, known, key, qualifiers) {
    found <- Filter(function(lookups) key %in% lookups$set$keys$key, sets)
    if (length(found) == 0L) {
        stop(
            sprintf(
                "unknown key '%s'; keys: %s",
                key,
                paste(sort_c(known), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    declaring <- Filter(function(lookups) declares(lookups, qualifiers), found)
    if (length(declaring) > 0L) declaring else found[1L]
}

# Whether every set's fields for one key contain, or are contained in, every
# other set's.
nested_fields <- function(fields) {
    all(vapply(
        fields,
        function(one) {
            all(vapply(
                fields,
                function(other) all(one %in% other) || all(other %in% one),
                logical(1)
            ))
        },
        logical(1)
    ))
}

# The fields each set's views hold, by level, key and slot: varm fields answer
# in var.
held_fields <- function(sets) {
    bind_rows(lapply(names(sets), function(set_name) {
        views <- sets[[set_name]]$views
        bind_rows(lapply(names(views), function(level) {
            entries <- views[[level]]$entries
            tibble(
                set = rep(set_name, NROW(entries)),
                level = rep(level, NROW(entries)),
                key = as.character(entries$key),
                location = if_else(
                    entries$location %in% "varm",
                    "var",
                    as.character(entries$location)
                ),
                address = entry_addresses(entries)
            )
        }))
    }))
}

# Sets that share a key name the same fields with it, level by level and slot by
# slot; a set may name more, told apart by a qualifier the other set lacks.
check_shared_keys <- function(sets) {
    named <- held_fields(sets)
    if (nrow(named) == 0L) {
        return(invisible(NULL))
    }
    clashing <- named |>
        summarise(
            .by = all_of(c("level", "key", "location")),
            nested = nested_fields(split(
                .data$address,
                factor(.data$set, levels = unique(.data$set))
            )),
            sets = list(unique(.data$set))
        ) |>
        filter(!.data$nested)
    if (nrow(clashing) > 0L) {
        stop(
            sprintf(
                "key '%s' names different %s fields on %s in the sets %s",
                clashing$key[[1L]],
                clashing$location[[1L]],
                clashing$level[[1L]],
                paste(sort_c(clashing$sets[[1L]]), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    invisible(NULL)
}

# The rows of one level's fields: those it holds, and for a reviewed level those
# it lacks, with the single value of a field apb2 left out.
level_fields <- function(lookups, view, level) {
    present <- present_rows(lookups, view)
    if (!inherits(view, "ReviewedLevel")) {
        return(present)
    }
    rule <- view$rule
    absent <- entries_for(lookups$set, rule$software, rule$pattern, rule$level)
    absent <- absent[!(absent$entry_id %in% view$entries$entry_id), ]
    left_out <- unlist(level_left_out(level))
    constant <- if (is.null(left_out)) {
        rep(NA_character_, nrow(absent))
    } else {
        unname(left_out[entry_addresses(absent)])
    }
    bind_rows(
        present,
        mutate(
            lookups$rows(view, absent),
            present = FALSE,
            constant = .env$constant
        )
    )
}
