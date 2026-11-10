# Reviewed meanings of retained APB fields, per vendor and software version;
# apb_catalog.source and .vocabulary.

UNKNOWN <- "unknown"

# The locations a request reads: var and varm both hold one value per feature.
READS <- list(
    obs = "obs",
    layers = "layers",
    var = c("var", "varm"),
    varm = "varm"
)

# A request accepts one of the set's kinds, or none, and any subset of the
# qualifiers, each with admitted values; every qualifier with values admits
# "unknown".
check_predicate <- function(vocabulary, kind, qualifiers) {
    if (!is.null(kind)) {
        check_kind(vocabulary, kind)
    }
    Map(
        function(name, accepted) {
            if (length(accepted) == 0L) {
                stop(
                    sprintf("qualifier '%s' accepts no value", name),
                    call. = FALSE
                )
            }
            lapply(accepted, function(value) {
                check_value(vocabulary, name, value)
            })
        },
        names(qualifiers),
        qualifiers
    )
    invisible(NULL)
}

check_kind <- function(vocabulary, kind) {
    if (!(kind %in% names(vocabulary$kinds))) {
        stop(
            sprintf(
                "the set does not admit the kind '%s'; kinds: %s",
                kind,
                paste(sort_c(names(vocabulary$kinds)), collapse = ", ")
            ),
            call. = FALSE
        )
    }
}

check_value <- function(vocabulary, name, value) {
    qualifier <- vocabulary$qualifiers[[name]]
    if (is.null(qualifier)) {
        stop(
            sprintf(
                "unknown qualifier '%s'; declared: %s",
                name,
                paste(sort_c(names(vocabulary$qualifiers)), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    if (!(value %in% names(qualifier$values))) {
        stop(
            sprintf(
                "qualifier '%s' does not admit '%s'; admitted: %s",
                name,
                value,
                paste(sort_c(names(qualifier$values)), collapse = ", ")
            ),
            call. = FALSE
        )
    }
}

new_reference <- function(found) {
    list(
        level = found$level,
        location = found$location,
        name = found$name %||% NA_character_,
        table = found$table %||% NA_character_,
        prefix = found$prefix %||% NA_character_
    )
}

# The location, varm table and name a level's retained fields are indexed by, as
# one string.
address_key <- function(location, table, name) {
    if (length(name) == 0L) {
        return(character(0))
    }
    paste(location, ifelse(is.na(table), "", table), name, sep = "\x1f")
}

reference_key <- function(reference) {
    if (is.na(reference$name)) {
        stop("the reference is not bound to a column", call. = FALSE)
    }
    address_key(reference$location, reference$table, reference$name)
}

# A ";"-separated list of one CSV cell; none when the cell is empty.
cell_list <- function(text, separator = ";") {
    if (identical(text, "")) {
        character(0)
    } else {
        strsplit(text, separator, fixed = TRUE)[[1L]]
    }
}

# A catalog.csv row's entry id: its software, location and name joined by
# `/`.
row_entry_ids <- function(rows) {
    paste(rows$software, rows$location, rows$name, sep = "/")
}

# The qualifier and value of an entry's catalog.csv rows, one pair per row, as a
# named character vector; a row with neither adds none. Half a pair, a pair
# qualifying the kind, or a fixed other than true or false, or true without a
# qualifier, is refused.
row_pairs <- function(label, rows) {
    mapply(
        check_pair,
        qualifier = rows$qualifier,
        value = rows$value,
        fixed = rows$fixed,
        MoreArgs = list(label = label)
    )
    paired <- rows$qualifier != ""
    setNames(rows$value[paired], rows$qualifier[paired])
}

check_pair <- function(label, qualifier, value, fixed) {
    if ((qualifier == "") != (value == "")) {
        stop(
            sprintf(
                "catalog.csv: %s has a qualifier and a value only together",
                label
            ),
            call. = FALSE
        )
    }
    if (qualifier == "kind") {
        stop(
            sprintf(
                "catalog.csv: %s qualifies the kind, %s",
                label,
                "which its kind column holds"
            ),
            call. = FALSE
        )
    }
    if (
        !(fixed %in% c("true", "false")) || (fixed == "true" && qualifier == "")
    ) {
        stop(
            sprintf(
                "catalog.csv: %s has fixed '%s'; %s",
                label,
                fixed,
                "true on a row with a qualifier, else false"
            ),
            call. = FALSE
        )
    }
}

# A set's keys from its terms.csv rows, the catalogue's root: each the name
# callers ask by, its kind and PSI-MS terms, the same on every row of the key,
# and the qualifier values it fixes, one row per value; a row with neither fixes
# none. apb-catalog checks the bundle's terms when it writes them.
term_keys <- function(name, rows) {
    names <- unique(rows$key)
    keys <- lapply(names, function(key) {
        found <- rows[rows$key == key, , drop = FALSE]
        kinds <- sort_c(unique(found$kind))
        terms <- sort_c(unique(found$psi_ms))
        if (length(kinds) != 1L || length(terms) != 1L) {
            stop(
                sprintf(
                    "%s: key '%s' has the kinds %s and the PSI-MS terms %s; %s",
                    name,
                    key,
                    paste(kinds, collapse = ", "),
                    paste(terms, collapse = ", "),
                    "every row of a key has one of each"
                ),
                call. = FALSE
            )
        }
        pairs <- found[found$qualifier != "", , drop = FALSE]
        pairs <- pairs[!duplicated(pairs$qualifier), , drop = FALSE]
        list(
            key = key,
            kind = kinds,
            fixed = setNames(pairs$value, pairs$qualifier),
            psi_ms = cell_list(terms)
        )
    })
    setNames(keys, names)
}

# The requirement rows of a set's terms.csv: one per term, a key at one
# location, that states one.
term_requirements <- function(rows) {
    stated <- rows[rows$requirement != "", , drop = FALSE]
    terms <- paste(stated$key, stated$location, sep = "\x1f")
    lapply(which(!duplicated(terms)), function(index) {
        parts <- c(
            strsplit(stated$location[[index]], "/", fixed = TRUE)[[1L]],
            ""
        )
        list(
            level = parts[[1L]],
            location = parts[[2L]],
            key = stated$key[[index]],
            requirement = stated$requirement[[index]]
        )
    })
}

# The qualifier values a request for a key accepts besides its kind: the values
# it fixes.
key_accepted <- function(found) {
    as.list(found$fixed)
}

# One entry from its catalog.csv rows, one per qualifier, its parsed id and its
# key.
row_entry <- function(rows, named, key) {
    if (is.null(key)) {
        stop(sprintf("%s: a field has no key", named$source), call. = FALSE)
    }
    entry_id <- row_entry_ids(rows[1L, , drop = FALSE])
    reference <- list(
        level = named$level,
        location = named$attribute,
        table = named$table
    )
    if (endsWith(named$name, "*")) {
        reference$prefix <- sub("\\*$", "", named$name)
    } else {
        reference$name <- named$name
    }
    list(
        entry_id = entry_id,
        software_version_pattern = version_patterns(
            named$source,
            named$version
        ),
        reference = new_reference(reference),
        key = key$key,
        kind = key$kind,
        qualifiers = row_pairs(entry_id, rows)
    )
}

# Who an entry comes from: the software, an app, or annotation:<convention>, the
# first field of its id.
entry_source <- function(entry) {
    sub("/.*$", "", entry$entry_id)
}

# The entry bound to one level's column: its reference and the column of its id.
bound_entry <- function(entry, name) {
    reference <- entry$reference
    entry$entry_id <- paste0(sub("/[^/]*$", "", entry$entry_id), "/", name)
    entry$reference <- list(
        level = reference$level,
        location = reference$location,
        name = name,
        table = reference$table,
        prefix = NA_character_
    )
    entry
}

# One catalogue per software of versions.csv, each reviewing every level the
# bundle's entries name for its versions, when the set has vendor entries; none
# otherwise.
vendor_catalogues <- function(name, entries) {
    if (length(entries) == 0L) {
        return(list())
    }
    levels <- rule_levels()
    software <- unique(vapply(
        levels,
        function(found) found$software_name,
        character(1)
    ))
    unknown <- setdiff(names(entries), software)
    if (length(unknown) > 0L) {
        stop(
            sprintf(
                "%s: no packaged rule declares the software %s",
                name,
                paste(sort_c(unknown), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    lapply(software, function(vendor) {
        variants <- Filter(
            function(found) found$software_name == vendor,
            levels
        )
        list(
            type = "vendor",
            label = vendor,
            software_name = vendor,
            variants = lapply(variants, function(found) {
                list(
                    level = found$level,
                    software_version_pattern = found$software_version_pattern
                )
            }),
            entries = entries[[vendor]] %||% list()
        )
    })
}

# An app's catalogue, such as apb-fasta's; its producers.csv row names the run
# it waits for and what to run.
producer_catalogue <- function(producer, entries, run) {
    list(
        type = "producer",
        label = producer,
        producer = producer,
        requires = cell_list(run$requires, "."),
        instruction = if (identical(run$instruction, "")) {
            NA_character_
        } else {
            run$instruction
        },
        entries = entries
    )
}

annotation_catalogue <- function(convention, entries) {
    list(
        type = "annotation",
        label = paste0("annotation:", convention),
        convention = convention,
        entries = entries
    )
}

# A set's vocabulary from its sets.csv row, its kinds, its qualifiers and their
# values, shaped as apb_catalog's SetVocabulary.
set_vocabulary <- function(head, kind_rows, qualifier_rows, value_rows) {
    qualifiers <- lapply(seq_len(nrow(qualifier_rows)), function(index) {
        values <- value_rows[
            value_rows$qualifier == qualifier_rows$qualifier[[index]],
            ,
            drop = FALSE
        ]
        list(
            definition = qualifier_rows$definition[[index]],
            values = setNames(as.list(values$definition), values$value)
        )
    })
    list(
        prefix = head$prefix,
        definition = head$definition,
        kind_definition = head$kind_definition,
        kinds = setNames(as.list(kind_rows$definition), kind_rows$kind),
        qualifiers = setNames(qualifiers, qualifier_rows$qualifier)
    )
}

# The entries a vendor catalogue reviewed for one of its rule variants.
variant_entries <- function(vendor, variant) {
    Filter(
        function(entry) {
            variant$software_version_pattern %in%
                entry$software_version_pattern &&
                entry$reference$level == variant$level
        },
        vendor$entries
    )
}

# The entries a producer or annotation catalogue supplies on one level, those of
# every level included.
entries_on <- function(source, level) {
    Filter(Negate(is.null), lapply(source$entries, entry_on, level = level))
}

# The entry on one level: itself, an entry of every level (`*`) taking that
# level in its reference and id, or NULL on another level.
entry_on <- function(entry, level) {
    if (entry$reference$level == level) {
        return(entry)
    }
    if (entry$reference$level != ANY_VERSION) {
        return(NULL)
    }
    fields <- strsplit(entry$entry_id, "/", fixed = TRUE)[[1L]]
    fields[[3L]] <- level
    entry$entry_id <- paste(fields, collapse = "/")
    entry$reference$level <- level
    entry
}

# Whether a result's metadata records a producer's run.
producer_ran <- function(producer, metadata) {
    has_path(metadata, as.character(unlist(producer$requires)))
}

# Whether nested named lists hold the path of keys, each key but the last naming
# a named list.
has_path <- function(section, keys) {
    if (length(keys) == 0L) {
        return(TRUE)
    }
    if (!(keys[[1L]] %in% names(section))) {
        return(FALSE)
    }
    if (length(keys) == 1L) {
        return(TRUE)
    }
    inner <- section[[keys[[1L]]]]
    if (!is.list(inner) || is.null(names(inner))) {
        return(FALSE)
    }
    has_path(inner, keys[-1L])
}

pending_reason <- function(producer) {
    sprintf(
        "%s has not run on this result; %s",
        producer$producer,
        producer$instruction
    )
}

annotation_applies <- function(annotation, metadata) {
    annotation$convention %in% names(metadata)
}

# Whether an entry on a requirement row's level answers the row.
row_describes <- function(row, entry) {
    entry$reference$level %in%
        c(row$level, ANY_VERSION) &&
        entry$reference$location %in% READS[[row$location]] &&
        identical(entry$key, row$key)
}

# One packaged catalogue set: its vocabulary, keys, vendor, producer and
# annotation catalogues and requirements, indexed by software and version.
# Producers hold the fields apps write after conversion and annotations the
# sample-annotation fields; both apply to every software version. A set made
# only of them has no vendor catalogue and needs no review.
CatalogueSet <- R6Class(
    "CatalogueSet",
    public = list(
        name = NULL,
        vocabulary = NULL,
        vocabulary_version = NULL,
        keys = NULL,
        description = NULL,
        catalogues = NULL,
        producers = NULL,
        annotations = NULL,
        requirements = NULL,

        initialize = function(bundle, name) {
            rows <- function(table) {
                found <- catalogue_rows(bundle, table)
                found[found$set == name, , drop = FALSE]
            }
            head <- set_head(rows("sets"), name)
            keys <- term_keys(name, rows("terms"))
            by_source <- source_entries(rows("catalog"), keys)
            sources <- names(by_source)
            runs <- rows("producers")
            annotation <- startsWith(sources, "annotation:")
            app <- !annotation & sources %in% runs$producer
            self$name <- name
            self$vocabulary <- set_vocabulary(
                head,
                rows("kinds"),
                rows("qualifiers"),
                rows("qualifier_values")
            )
            self$vocabulary_version <- bundle$json(
                "catalogues",
                "datapackage.json"
            )$version
            self$keys <- keys
            self$description <- list(
                purpose = head$purpose,
                used_by = cell_list(head$used_by)
            )
            self$catalogues <- vendor_catalogues(
                name,
                by_source[!annotation & !app]
            )
            self$producers <- lapply(which(app), function(index) {
                run <- runs[runs$producer == sources[[index]], , drop = FALSE]
                producer_catalogue(sources[[index]], by_source[[index]], run)
            })
            self$annotations <- lapply(which(annotation), function(index) {
                annotation_catalogue(
                    sub("^annotation:", "", sources[[index]]),
                    by_source[[index]]
                )
            })
            self$requirements <- term_requirements(rows("terms"))
            private$by_version <- reviewed_versions(self$catalogues)
        },

        # Every catalogue of the set: vendor, producer and annotation.
        sources = function() {
            c(self$catalogues, self$producers, self$annotations)
        },

        # The rule reviewed for one software version's level, or NULL.
        reviewed = function(software, pattern, level) {
            private$by_version[[paste(software, pattern, level, sep = "\x1f")]]
        },

        # One of the set's keys; an unknown name signals an error naming the
        # set's keys.
        key = function(name) {
            found <- self$keys[[name]]
            if (is.null(found)) {
                known <- paste(sort_c(names(self$keys)), collapse = ", ")
                stop(
                    sprintf("unknown key '%s'; keys: %s", name, known),
                    call. = FALSE
                )
            }
            found
        }
    ),
    active = list(
        # Whether a level answers only once a vendor catalogue reviewed its
        # rule.
        needs_review = function() length(self$catalogues) > 0L
    ),
    private = list(by_version = list())
)

# The sets.csv row describing the set; an unknown set signals an error naming
# the packaged ones.
set_head <- function(head, name) {
    if (nrow(head) != 1L) {
        stop(
            sprintf(
                "unknown catalogue '%s'; packaged: %s",
                name,
                paste(set_names(), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    head
}

# A set's entries from its catalog.csv rows, one entry per field and key,
# grouped by the source their ids name, in the order the sources first appear.
source_entries <- function(rows, keys) {
    ids <- row_entry_ids(rows)
    fields <- paste(ids, rows$key, sep = "\x1f")
    groups <- split(
        seq_len(nrow(rows)),
        factor(fields, levels = unique(fields))
    )
    entries <- lapply(unname(groups), function(index) {
        field <- rows[index, , drop = FALSE]
        row_entry(
            field,
            entry_id_fields(ids[[index[[1L]]]]),
            keys[[field$key[[1L]]]]
        )
    })
    named_sources <- vapply(entries, entry_source, character(1))
    split(entries, factor(named_sources, levels = unique(named_sources)))
}

# Each vendor catalogue's reviewed rule levels, by software, version pattern and
# level.
reviewed_versions <- function(catalogues) {
    found <- concat(lapply(catalogues, function(vendor) {
        lapply(vendor$variants, function(variant) {
            list(catalogue = vendor, variant = variant)
        })
    }))
    keys <- vapply(
        found,
        function(one) {
            paste(
                one$catalogue$software_name,
                one$variant$software_version_pattern,
                one$variant$level,
                sep = "\x1f"
            )
        },
        character(1)
    )
    named_last(found, keys)
}

catalogue_set <- function(name) {
    bundle <- current_bundle()
    bundle$memo(paste0("set:", name), function() CatalogueSet$new(bundle, name))
}

set_names <- function() {
    catalogue_rows(current_bundle(), "sets")$set
}

# One catalogue table of the bundle, as a data frame of strings with empty cells
# as "", read once; its columns must be those datapackage.json declares.
catalogue_rows <- function(bundle, table) {
    bundle$memo(paste0("table:", table), function() {
        found <- read.csv(
            file.path(bundle$dir, "catalogues", paste0(table, ".csv")),
            colClasses = "character",
            na.strings = character(0),
            check.names = FALSE,
            encoding = "UTF-8"
        )
        resources <- bundle$json("catalogues", "datapackage.json")$resources
        resource <- Filter(
            function(found) identical(found$name, table),
            resources
        )[[1L]]
        declared <- vapply(
            resource$schema$fields,
            function(field) field$name,
            character(1)
        )
        if (!identical(names(found), declared)) {
            stop(
                sprintf(
                    "%s.csv has the columns %s, not %s",
                    table,
                    paste(names(found), collapse = ", "),
                    paste(declared, collapse = ", ")
                ),
                call. = FALSE
            )
        }
        found
    })
}
