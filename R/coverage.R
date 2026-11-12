# Which cataloged fields every rule supplies, and whether each level meets its
# set; apb_catalog.coverage.

# The columns before the set's qualifier.<name> columns; producer, requirement,
# needs and verdict follow.
COVERAGE_COLUMNS <- c(
    "software",
    "version",
    "software_version_pattern",
    "level",
    "slot",
    "table",
    "name",
    "key",
    "kind"
)

SEVERITY <- c(required = "fail", recommended = "warn", optional = "pass")

#' The catalog's sets
#'
#' Every set of the bundle with its prefix, its purpose, its users and its
#' number of entries. The prefix asks the set alone, as `"c:q_value"`.
#'
#' @return A data frame with columns `set`, `prefix`, `purpose`, `used_by` (a
#'   list column), `has_requirements` and `entries`.
#' @export
#' @examples
#' sets()
sets <- function() {
    rows <- lapply(set_names(), function(name) {
        set <- load_set(name)
        list(
            set = name,
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
            "set",
            "prefix",
            "purpose",
            "used_by",
            "has_requirements",
            "entries"
        ),
        lists = "used_by"
    )
}

#' Check every rule against one set
#'
#' One row per cataloged or required field of each rule level the set reviews;
#' a set without vendor entries is checked against every level the bundle's
#' entries name for each version of `versions.csv`. A set with requirements
#' judges each level: `fail` when a required field is unsupported, `warn` when
#' a recommended one is, `pass` otherwise; an unsupported optional field is
#' listed but passes.
#'
#' @param set_name The set's name.
#' @return A data frame with columns `software` (the quantification software of
#'   the rule), `version` (the label), `software_version_pattern`, `level`,
#'   `slot`, `table` (the `varm` table, else `NA`), `name` (the field's name,
#'   `NA` when unsupported), `key`, `kind`, one `qualifier.<name>` column per
#'   other qualifier of the set, as in [catalog_table()], `producer` (the app,
#'   `NA` for the quantification software's fields), `requirement`, `needs`
#'   (what to run before the field exists) and `verdict` (`NA` for a set
#'   without requirements).
#' @export
#' @examples
#' head(rule_coverage("abundance_alpha"))
rule_coverage <- function(set_name) {
    check_string(set_name, "set_name")
    set <- load_set(set_name)
    rows <- concat(lapply(set$vendors, function(vendor) {
        concat(lapply(vendor$variants, function(variant) {
            variant_rows(
                set,
                vendor$software_name,
                variant,
                variant_entries(vendor, variant)
            )
        }))
    }))
    if (!set$needs_review) {
        rows <- c(
            rows,
            concat(lapply(rule_levels(), function(found) {
                variant <- list(
                    level = found$level,
                    software_version_pattern = found$software_version_pattern
                )
                variant_rows(set, found$software_name, variant, list())
            }))
        )
    }
    records_frame(rows, coverage_columns(set))
}

# The coverage table's columns for one set: see rule_coverage().
coverage_columns <- function(set) {
    c(
        COVERAGE_COLUMNS,
        sprintf("qualifier.%s", qualifier_names(set)),
        "producer",
        "requirement",
        "needs",
        "verdict"
    )
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
            coverage_field(set, item$entry, item$producer),
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

# A coverage row's field for a required key no field answers: the key, kind and
# fixed qualifiers.
requirement_field <- function(set, row) {
    key <- set$key(row$key)
    c(
        list(key = row$key, kind = key$kind),
        qualifier_columns(set, key$fixed),
        list(slot = row$location, requirement = row$requirement)
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

# The requirement of the first requirement row that describes the entry.
row_requirement <- function(required, entry) {
    row <- Find(function(row) row_describes(row, entry), required)
    if (is.null(row)) NA_character_ else row$requirement
}

coverage_verdict <- function(requirements) {
    severities <- SEVERITY[requirements]
    if ("fail" %in% severities) {
        return("fail")
    }
    if ("warn" %in% severities) "warn" else "pass"
}

coverage_field <- function(set, entry, producer) {
    reference <- entry$reference
    c(
        list(
            slot = reference$location,
            table = reference$table,
            name = reference$name,
            key = entry$key,
            kind = entry$kind
        ),
        qualifier_columns(set, entry$qualifiers),
        list(
            producer = if (is.null(producer)) NULL else producer$producer,
            needs = if (is.null(producer)) {
                NA_character_
            } else {
                producer$instruction
            }
        )
    )
}
