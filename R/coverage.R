# Which catalogued fields every rule supplies, and whether each level meets its
# set; apb_catalog.coverage.

COVERAGE_COLUMNS <- c(
    "software",
    "version",
    "software_version_pattern",
    "level",
    "key",
    "kind",
    "qualifiers",
    "origin",
    "producer",
    "requirement",
    "location",
    "table",
    "column",
    "needs",
    "verdict"
)

SEVERITY <- c(required = "fail", recommended = "warn", optional = "pass")

#' The catalogue sets
#'
#' Every catalogue set of the bundle with its prefix, its purpose, its users and
#' its number of entries. The prefix asks the set alone, as `"c:q_value"`.
#'
#' @return A data frame with columns `catalogue`, `prefix`, `purpose`, `used_by`
#'   (a list column), `has_requirements` and `entries`.
#' @export
#' @examples
#' catalogues()
catalogues <- function() {
    rows <- lapply(set_names(), function(name) {
        set <- catalogue_set(name)
        list(
            catalogue = name,
            prefix = set$vocabulary$prefix,
            purpose = set$description$purpose,
            used_by = set$description$used_by,
            has_requirements = length(set$requirements) > 0L,
            entries = sum(vapply(
                set$sources(),
                function(source) length(source$entries),
                integer(1)
            ))
        )
    })
    records_frame(
        rows,
        c(
            "catalogue",
            "prefix",
            "purpose",
            "used_by",
            "has_requirements",
            "entries"
        ),
        lists = "used_by"
    )
}

#' Check every rule against one catalogue set
#'
#' One row per catalogued or required field of each rule level the set reviews;
#' a set without vendor catalogues is checked against every level the bundle's
#' entries name for each version of `versions.csv`. A set with requirements
#' judges each level: `fail` when a required field is unsupported, `warn` when
#' a recommended one is, `pass` otherwise; an unsupported optional field is
#' listed but passes.
#'
#' @param catalogue The set's name.
#' @return A data frame with columns `software`, `version` (the label),
#'   `software_version_pattern`, `level`, `key`, `kind`, `qualifiers` (the other
#'   qualifiers as `name=value`), `origin`, `producer` (the app, `NA` for the
#'   conversion software's fields), `requirement`, `location`, `table`, `column`
#'   (`NA` when unsupported), `needs` (what to run before the field exists) and
#'   `verdict` (`NA` for a set without requirements).
#' @export
#' @examples
#' head(rule_coverage("abundance"))
rule_coverage <- function(catalogue) {
    set <- catalogue_set(catalogue)
    rows <- list()
    for (vendor in set$catalogues) {
        for (variant in vendor$variants) {
            rows <- c(
                rows,
                variant_rows(
                    set,
                    vendor$software_name,
                    variant,
                    entries_for(vendor, variant)
                )
            )
        }
    }
    if (!set$needs_review) {
        for (found in rule_levels()) {
            variant <- list(
                level = found$level,
                software_version_pattern = found$software_version_pattern
            )
            rows <- c(
                rows,
                variant_rows(set, found$software_name, variant, list())
            )
        }
    }
    records_frame(rows, COVERAGE_COLUMNS)
}

variant_rows <- function(set, software, variant, vendor_entries) {
    found <- c(
        lapply(vendor_entries, function(entry) {
            list(entry = entry, producer = NULL)
        }),
        producer_fields(set$producers, variant)
    )
    required <- Filter(
        function(row) row$level == variant$level,
        set$requirements
    )
    missing <- unmet_requirements(required, found)
    verdict <- if (length(required) > 0L) {
        coverage_verdict(vapply(missing, function(row) row$requirement, ""))
    } else {
        NA
    }
    base <- list(
        software = software,
        version = version_label(variant$software_version_pattern),
        software_version_pattern = variant$software_version_pattern,
        level = variant$level,
        verdict = verdict
    )
    rows <- lapply(found, function(item) {
        c(
            base,
            coverage_field(item$entry, item$producer),
            list(requirement = row_requirement(required, item$entry))
        )
    })
    rows <- c(
        rows,
        lapply(missing, function(row) c(base, requirement_field(set, row)))
    )
    if (length(rows) == 0L) {
        rows <- list(base)
    }
    required_order(rows, required)
}

# The requirement rows no found field describes.
unmet_requirements <- function(required, found) {
    Filter(
        function(row) {
            !any(vapply(
                found,
                function(item) row_describes(row, item$entry),
                logical(1)
            ))
        },
        required
    )
}

# A coverage row's field for a required key no field answers.
requirement_field <- function(set, row) {
    key <- set$key(row$key)
    list(
        key = row$key,
        kind = key$kind,
        qualifiers = qualifier_pairs(key$fixed),
        location = row$location,
        requirement = row$requirement
    )
}

# The apps' fields on a rule level: every entry of an app counts, with what to
# run.
producer_fields <- function(producers, variant) {
    unlist(
        lapply(producers, function(producer) {
            lapply(entries_on(producer, variant$level), function(entry) {
                list(entry = entry, producer = producer)
            })
        }),
        recursive = FALSE
    )
}

# Rows in the order of the requirements their key answers, vendor fields before
# producer fields; stable otherwise.
required_order <- function(rows, required) {
    keys <- vapply(required, function(row) row$key, character(1))
    ranks <- vapply(
        rows,
        function(row) {
            index <- match(row$key %||% NA_character_, keys)
            if (is.na(index)) length(required) else index - 1L
        },
        numeric(1)
    )
    produced <- vapply(rows, function(row) !is.null(row$producer), logical(1))
    rows[order(ranks, produced, method = "radix")]
}

row_requirement <- function(required, entry) {
    for (row in required) {
        if (row_describes(row, entry)) {
            return(row$requirement)
        }
    }
    NA_character_
}

qualifier_pairs <- function(qualifiers) {
    if (length(qualifiers) == 0L) {
        return(NA_character_)
    }
    ordered <- sort_c(names(qualifiers))
    paste(paste0(ordered, "=", qualifiers[ordered]), collapse = ";")
}

coverage_verdict <- function(requirements) {
    severities <- SEVERITY[requirements]
    if ("fail" %in% severities) {
        return("fail")
    }
    if ("warn" %in% severities) "warn" else "pass"
}

coverage_field <- function(entry, producer) {
    reference <- entry$reference
    others <- entry$qualifiers
    origin <- if ("origin" %in% names(others)) {
        others[["origin"]]
    } else {
        NA_character_
    }
    others <- others[names(others) != "origin"]
    list(
        key = entry$key,
        kind = entry$kind,
        qualifiers = qualifier_pairs(others),
        origin = origin,
        producer = if (is.null(producer)) NULL else producer$producer,
        location = reference$location,
        table = reference$table,
        column = reference$name,
        needs = if (is.null(producer)) NA_character_ else producer$instruction
    )
}
