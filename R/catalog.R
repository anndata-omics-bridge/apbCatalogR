# Look up APB fields by meaning, so callers never name vendor columns;
# apb_catalog.catalog.

# The entry bound to the level's column: by name or by prefix. A prefix matching
# several columns binds to each, so the lookup reports them as ambiguous.
bind_entry <- function(entry, obs_columns) {
    reference <- entry$reference
    if (!is.na(reference$prefix)) {
        matching <- obs_columns[startsWith(
            tolower(obs_columns),
            tolower(reference$prefix)
        )]
        return(lapply(matching, function(column) bound_entry(entry, column)))
    }
    list(entry)
}

# Bind one level to its set: the reviewed vendor entries, then the apps' and
# annotations'. Only entries whose field the level holds answer; an app that has
# not run leaves its entries pending. A set with vendor catalogues answers only
# for a reviewed software version.
level_view <- function(
    name,
    software,
    pattern,
    rule_level,
    set,
    retained,
    obs_columns,
    ran,
    annotations
) {
    if (is.null(software) || is.null(pattern) || is.null(rule_level)) {
        return(UnreviewedLevel$new(
            name,
            NULL,
            NULL,
            "the level records no rule_json software version"
        ))
    }
    entries <- list()
    reviewed <- set$reviewed(software, pattern, rule_level)
    if (set$needs_review) {
        if (is.null(reviewed)) {
            unreviewed <- paste(software, pattern, rule_level)
            reason <- paste("no source catalogue reviewed", unreviewed)
            return(UnreviewedLevel$new(name, software, pattern, reason))
        }
        entries <- held_entries(
            variant_entries(reviewed$catalogue, reviewed$variant),
            retained
        )
    }
    produced <- producer_entries(
        set$producers,
        rule_level,
        retained,
        obs_columns,
        ran
    )
    entries <- c(
        entries,
        produced$entries,
        annotation_entries(annotations, rule_level, obs_columns)
    )
    if (!is.null(reviewed)) {
        return(ReviewedLevel$new(name, reviewed, entries, produced$pending))
    }
    ProducedLevel$new(name, software, pattern, entries, produced$pending)
}

# The entries whose field the level holds.
held_entries <- function(entries, retained) {
    Filter(
        function(entry) reference_key(entry$reference) %in% retained,
        entries
    )
}

bound_entries <- function(source, rule_level, obs_columns) {
    unlist(
        lapply(entries_on(source, rule_level), bind_entry, obs_columns),
        recursive = FALSE
    )
}

# Every field the set's entries name on one level: without data, the catalogue
# says what a level holds.
catalogued_keys <- function(set, level) {
    entries <- unlist(
        lapply(set$sources(), function(source) source$entries),
        recursive = FALSE
    )
    named <- Filter(
        function(entry) {
            entry$reference$level %in%
                c(level, ANY_VERSION) &&
                !is.na(entry$reference$name)
        },
        entries
    )
    unique(vapply(
        named,
        function(entry) reference_key(entry$reference),
        character(1)
    ))
}

# The apps' entries the level holds, and the entries of apps that have not run.
producer_entries <- function(
    producers,
    rule_level,
    retained,
    obs_columns,
    ran
) {
    offered <- lapply(producers, function(producer) {
        bound_entries(producer, rule_level, obs_columns)
    })
    done <- vapply(producers, ran, logical(1))
    pending <- Map(
        function(producer, entries) {
            reason <- pending_reason(producer)
            lapply(entries, function(entry) {
                list(entry = entry, reason = reason)
            })
        },
        producers[!done],
        offered[!done]
    )
    list(
        entries = concat(lapply(offered[done], held_entries, retained)),
        pending = concat(pending)
    )
}

annotation_entries <- function(annotations, rule_level, obs_columns) {
    unlist(
        lapply(annotations, bound_entries, rule_level, obs_columns),
        recursive = FALSE
    )
}

check_enforce <- function(enforce) {
    if (!is.null(enforce) && !(enforce %in% c("required", "recommended"))) {
        stop(
            "enforce is NULL, \"required\" or \"recommended\"",
            call. = FALSE
        )
    }
}

# An argument that names one thing, such as a set, a key or a path.
check_string <- function(value, argument) {
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
        stop(sprintf("%s is one character string", argument), call. = FALSE)
    }
}

# A path that must exist.
check_path <- function(path) {
    check_string(path, "path")
    if (!file.exists(path)) {
        stop(sprintf("%s does not exist", path), call. = FALSE)
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
        function(lookups) length(lookups$set$requirements) > 0L,
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
            rows <- Filter(
                function(row) {
                    row$requirement != "optional" &&
                        !is.null(self$views[[row$level]])
                },
                self$set$requirements
            )
            found <- lapply(rows, function(row) {
                resolution <- self$answer(self$request(
                    row$level,
                    row$location,
                    row$key,
                    list()
                ))
                if (resolution$status != "missing") {
                    return(NULL)
                }
                resolution$requirement <- row$requirement
                list(resolution = resolution, row = row)
            })
            unname(Filter(Negate(is.null), found))
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
                known = names(self$set$keys),
                sets = list()
            )
        },

        rows = function(view, entries) {
            lapply(entries, function(entry) {
                pattern <- view$software_version_pattern %||% NA_character_
                entry_row(
                    self$name,
                    self$set,
                    entry_source(entry),
                    pattern,
                    entry
                )
            })
        }
    )
)

# The keys a field the level holds answers; a later producer's keys wait until
# it runs, so every key listed reads.
held_keys <- function(resolution, view) {
    held <- vapply(
        view$entries %||% list(),
        function(entry) reference_key(entry$reference),
        character(1)
    )
    answering <- vapply(
        Filter(
            function(candidate) reference_key(candidate$reference) %in% held,
            resolution$candidates
        ),
        function(candidate) candidate$key,
        character(1)
    )
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
    answering <- vapply(
        view$entries,
        function(entry) reference_key(entry$reference),
        character(1)
    )
    blocked <- vapply(
        resolution$candidates,
        function(candidate) {
            length(candidate$undecided) > 0L ||
                !(reference_key(candidate$reference) %in% answering)
        },
        logical(1)
    )
    if (any(blocked)) {
        stop(unresolved_field(resolution))
    }
    lapply(resolution$candidates, function(candidate) candidate$reference)
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

fields_frame <- function(rows, set) {
    records_frame(rows, c(set_columns(set), "present", "constant"))
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
            check_string(software, "software")
            check_string(version, "version")
            check_string(catalog, "catalog")
            check_enforce(enforce)
            set <- catalogue_set(catalog)
            matched <- rules_matching(software, version)
            check_rules_matched(matched, software, version)
            views <- rule_views(set, matched)
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
                known = names(set$keys),
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
            rows <- unlist(
                lapply(lookups$views, function(view) {
                    lapply(lookups$rows(view, view$entries), function(row) {
                        c(row, list(present = TRUE, constant = NA_character_))
                    })
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
        lookups = NULL
    )
)

# No packaged rule admitting a software version signals an error naming the
# version patterns the software's rules have.
check_rules_matched <- function(matched, software, version) {
    if (length(matched) > 0L) {
        return(invisible())
    }
    known <- unique(vapply(
        Filter(
            function(found) tolower(found$software_name) == tolower(software),
            rule_levels()
        ),
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
                lapply(private$sets, function(lookups) names(lookups$set$keys)),
                use.names = FALSE
            )) %||%
                character(0)
            enforce_sets(private$sets, enforce)
            lookup <- composed_lookup(
                private$sets,
                private$known,
                function(level, location, key, qualifiers) {
                    private$find(level, location, key, qualifiers)
                }
            )
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
            rows <- concat(lapply(private$sets, function(lookups) {
                concat(lapply(names(lookups$views), function(name) {
                    level_fields(
                        lookups,
                        lookups$views[[name]],
                        private$parsed$levels[[name]]
                    )
                }))
            }))
            # As polars builds a frame from records: the columns in the order
            # the rows first name them.
            columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
            if (is.null(columns) && length(private$sets) > 0L) {
                columns <- c(
                    set_columns(private$sets[[1L]]$set),
                    "present",
                    "constant"
                )
            }
            records_frame(rows, columns %||% character(0))
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
        },

        # The field one key names, asked of every set that defines the key; the
        # first found.
        find = function(level, location, key, qualifiers) {
            found <- lapply(
                defining_sets(private$sets, private$known, key, qualifiers),
                function(lookups) {
                    lookups$find(level, location, key, qualifiers)
                }
            )
            Find(Negate(is.null), found)
        }
    )
)

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

# One set's view of each level the matched rules convert, by level: every
# catalogued key retained, and only apps that need no run have run.
rule_views <- function(set, matched) {
    views <- lapply(matched, function(found) {
        level_view(
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
    })
    named_last(
        views,
        vapply(matched, function(found) found$level, character(1))
    )
}

# One set bound to every level of a result.
data_lookups <- function(result, name) {
    set <- catalogue_set(name)
    check_answers(name, set, result$metadata)
    annotations <- Filter(
        function(annotation) annotation_applies(annotation, result$metadata),
        set$annotations
    )
    views <- Map(
        function(level_name, level) {
            version <- level_rule_version(level)
            level_view(
                level_name,
                version$software,
                version$pattern,
                version$level,
                set,
                retained = level_retained(level),
                obs_columns = colnames(level$obs),
                ran = function(producer) {
                    producer_ran(producer, result$metadata)
                },
                annotations = annotations
            )
        },
        names(result$levels),
        result$levels
    )
    Lookups$new(name, set, views)
}

# Whether a set can answer on a result: it has vendor catalogues, one of its
# apps has run, or the result records one of its annotation conventions.
answers <- function(set, metadata) {
    applies <- vapply(
        set$annotations,
        annotation_applies,
        logical(1),
        metadata = metadata
    )
    ran <- vapply(set$producers, producer_ran, logical(1), metadata = metadata)
    set$needs_review || any(applies) || any(ran)
}

# A set needs a source the result answers from, as answers() decides. An
# error, saying what to run, when it can answer nothing.
check_answers <- function(name, set, metadata) {
    if (answers(set, metadata)) {
        return(invisible(NULL))
    }
    reasons <- vapply(set$producers, pending_reason, character(1))
    if (length(set$annotations) > 0L) {
        conventions <- vapply(
            set$annotations,
            function(found) found$convention,
            character(1)
        )
        reasons <- c(
            reasons,
            sprintf(
                "the result records no sample annotation (%s); %s",
                paste(sort_c(conventions), collapse = ", "),
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

# What a composed catalog's levels ask: one field through `find`, every field
# and key from every set that defines the key, and each set alone by prefix.
composed_lookup <- function(sets, known, find) {
    list(
        find = find,
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

# Every field one key names, from every set that defines the key.
composed_fields <- function(sets, known, level, location, key) {
    found <- concat(lapply(
        defining_sets(sets, known, key, list()),
        function(lookups) lookups$find_all(level, location, key)
    ))
    keys <- vapply(found, reference_key, character(1))
    unname(found[!duplicated(keys)])
}

# The sets that define a key and declare every further qualifier asked; the
# first that defines it when none declares them, so its lookup names what it
# lacks.
defining_sets <- function(sets, known, key, qualifiers) {
    found <- Filter(function(lookups) key %in% names(lookups$set$keys), sets)
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

# Sets that share a key name the same fields with it, level by level and slot by
# slot; a set may name more, told apart by a qualifier the other set lacks.
check_shared_keys <- function(sets) {
    named <- shared_key_fields(sets)
    clashing <- names(named)[!vapply(named, nested_fields, logical(1))]
    if (length(clashing) == 0L) {
        return(invisible(NULL))
    }
    parts <- strsplit(clashing[[1L]], "\x1f", fixed = TRUE)[[1L]]
    stop(
        sprintf(
            "key '%s' names different %s fields on %s in the sets %s",
            parts[[2L]],
            parts[[3L]],
            parts[[1L]],
            paste(sort_c(names(named[[clashing[[1L]]]])), collapse = ", ")
        ),
        call. = FALSE
    )
}

# The fields each set's key names, by slot (level, key and location, varm
# counted as var) and then by set, slots in the order the sets first name them.
shared_key_fields <- function(sets) {
    rows <- concat(lapply(names(sets), function(set_name) {
        views <- sets[[set_name]]$views
        concat(lapply(names(views), function(level) {
            lapply(views[[level]]$entries, function(entry) {
                location <- if (entry$reference$location == "varm") {
                    "var"
                } else {
                    entry$reference$location
                }
                c(
                    slot = paste(level, entry$key, location, sep = "\x1f"),
                    set = set_name,
                    field = reference_key(entry$reference)
                )
            })
        }))
    }))
    slots <- vapply(rows, function(row) row[["slot"]], character(1))
    in_set <- vapply(rows, function(row) row[["set"]], character(1))
    fields <- vapply(rows, function(row) row[["field"]], character(1))
    lapply(split(seq_along(rows), factor(slots, unique(slots))), function(at) {
        lapply(
            split(fields[at], factor(in_set[at], unique(in_set[at]))),
            unique
        )
    })
}

# The rows of one level's fields: those it holds, and for a reviewed level those
# it lacks, with the single value of a field apb2 left out.
level_fields <- function(lookups, view, level) {
    rows <- lapply(lookups$rows(view, view$entries), function(row) {
        c(row, list(present = TRUE, constant = NA_character_))
    })
    if (!inherits(view, "ReviewedLevel")) {
        return(rows)
    }
    held <- vapply(view$entries, function(entry) entry$entry_id, character(1))
    left_out <- level_left_out(level)
    absent <- Filter(
        function(entry) !(entry$entry_id %in% held),
        variant_entries(view$rule$catalogue, view$rule$variant)
    )
    c(
        rows,
        Map(
            function(entry, row) {
                constant <- left_out[[reference_key(entry$reference)]] %||%
                    NA_character_
                c(row, list(present = FALSE, constant = constant))
            },
            absent,
            lookups$rows(view, absent)
        )
    )
}
