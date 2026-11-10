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
    bundle <- current_bundle()
    sets <- catalogue_rows(bundle, "sets")
    terms <- catalogue_rows(bundle, "terms")
    entries <- bundle_entries(bundle)
    data.frame(
        catalogue = sets$set,
        prefix = sets$prefix,
        purpose = sets$purpose,
        used_by = I(lapply(sets$used_by, cell_list)),
        has_requirements = sets$set %in% terms$set[terms$requirement != ""],
        entries = tabulate(match(entries$set, sets$set), nrow(sets))
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
#' head(rule_coverage("abundance_alpha"))
rule_coverage <- function(catalogue) {
    set <- catalogue_set(catalogue)
    variants <- coverage_variants(set)
    found <- found_fields(set, variants)
    required <- variants |>
        select(".variant", "level") |>
        inner_join(
            mutate(
                set$requirements,
                .requirement = seq_len(nrow(set$requirements))
            ),
            by = "level",
            relationship = "many-to-many"
        )
    described <- found |>
        inner_join(
            select(
                inner_join(
                    required,
                    tibble(
                        location = rep(names(READS), lengths(READS)),
                        read = unlist(READS, use.names = FALSE)
                    ),
                    by = "location",
                    relationship = "many-to-many"
                ),
                ".variant",
                ".requirement",
                "read",
                "key",
                required = "requirement"
            ),
            by = c(".variant", "key", location = "read"),
            relationship = "many-to-many"
        )
    missing <- anti_join(
        required,
        described,
        by = c(".variant", ".requirement")
    )
    found$requirement <- first_requirement(found, described)
    rows <- bind_rows(
        found_rows(found),
        missing_rows(set, missing)
    )
    rows <- bind_rows(
        rows,
        tibble(.variant = setdiff(variants$.variant, rows$.variant))
    )
    ranks <- required |>
        distinct(.data$.variant, .data$key, .keep_all = TRUE) |>
        select(".variant", "key", .rank = ".requirement")
    rows |>
        mutate(
            .order = seq_len(nrow(rows)),
            produced = !is.na(.data$producer)
        ) |>
        left_join(ranks, by = c(".variant", "key")) |>
        left_join(
            variant_verdicts(variants, required, missing),
            by = ".variant"
        ) |>
        arrange(
            .data$.variant,
            coalesce(.data$.rank, Inf),
            .data$produced,
            .data$.order
        ) |>
        column_frame(COVERAGE_COLUMNS)
}

# The rule levels a set is checked on: those it reviews, vendor by vendor, or
# for a set without vendor catalogues every level the bundle's entries name for
# each version of versions.csv.
coverage_variants <- function(set) {
    levels <- rule_levels()
    if (set$needs_review) {
        software <- unique(levels$software_name)
        levels <- levels[order(match(levels$software_name, software)), ]
    }
    levels |>
        mutate(
            .variant = seq_len(nrow(levels)),
            software = .data$software_name,
            version = version_label(.data$software_version_pattern)
        )
}

# The fields each rule level supplies: the vendor entries reviewed for it, then
# every entry of an app on the level, with what to run.
found_fields <- function(set, variants) {
    vendor <- set$entries |>
        filter(.data$type == "vendor") |>
        inner_join(set$patterns, by = "entry_rank")
    reviewed <- variants |>
        select(
            ".variant",
            "software_name",
            "software_version_pattern",
            "level"
        ) |>
        inner_join(
            vendor,
            by = c(
                software_name = "source",
                "software_version_pattern",
                "level"
            ),
            relationship = "many-to-many"
        )
    produced <- variants |>
        select(".variant", variant_level = "level") |>
        cross_join(filter(set$entries, .data$type == "producer")) |>
        filter(
            .data$level == .data$variant_level | .data$level == ANY_VERSION
        ) |>
        left_join(set$producers, by = c(source = "producer")) |>
        mutate(producer = .data$source)
    bind_rows(reviewed, produced) |>
        arrange(.data$.variant) |>
        mutate(.found = seq_along(.data$.variant))
}

# Each found field's requirement: that of the first requirement row it answers.
first_requirement <- function(found, described) {
    first <- summarise(
        described,
        .by = ".found",
        requirement = .data$required[which.min(.data$.requirement)]
    )
    requirement <- rep(NA_character_, nrow(found))
    requirement[first$.found] <- first$requirement
    requirement
}

# Each level's verdict when its set has requirements on it: fail when a required
# field is unsupported, warn when a recommended one is, pass otherwise.
variant_verdicts <- function(variants, required, missing) {
    verdicts <- required |>
        mutate(
            missed = paste(.data$.variant, .data$.requirement) %in%
                paste(missing$.variant, missing$.requirement)
        ) |>
        summarise(
            .by = ".variant",
            verdict = coverage_verdict(.data$requirement[.data$missed])
        )
    variants |>
        select(
            ".variant",
            "software",
            "version",
            "software_version_pattern",
            "level"
        ) |>
        left_join(verdicts, by = ".variant")
}

# Coverage rows of the fields found: key, kind, the other qualifiers as
# name=value, origin, the app and what to run before the field exists.
found_rows <- function(found) {
    qualifiers <- grep("^qualifier[.]", names(found), value = TRUE)
    values <- tibble(
        .found = integer(0),
        qualifier = character(0),
        value = character(0)
    )
    if (length(qualifiers) > 0L) {
        values <- found |>
            select(".found", all_of(qualifiers)) |>
            pivot_longer(
                -".found",
                names_to = "qualifier",
                names_prefix = "qualifier[.]",
                values_drop_na = TRUE
            )
    }
    tibble(
        .variant = found$.variant,
        key = found$key,
        kind = found$kind,
        qualifiers = qualifier_text(
            filter(values, .data$qualifier != "origin"),
            found$.found
        ),
        origin = found[["qualifier.origin"]] %||% rep(NA, nrow(found)),
        producer = found$producer %||% rep(NA_character_, nrow(found)),
        requirement = found$requirement,
        location = found$location,
        table = found$table,
        column = found$name,
        needs = found$instruction %||% rep(NA_character_, nrow(found))
    )
}

# Coverage rows of the required keys no field answers.
missing_rows <- function(set, missing) {
    rows <- mutate(missing, .missing = seq_len(nrow(missing)))
    fixed <- rows |>
        select(".missing", "key") |>
        inner_join(set$fixed, by = "key", relationship = "many-to-many") |>
        select(".missing", "qualifier", "value")
    tibble(
        .variant = rows$.variant,
        key = rows$key,
        kind = set$keys$kind[match(rows$key, set$keys$key)],
        qualifiers = qualifier_text(fixed, rows$.missing, ".missing"),
        location = rows$location,
        requirement = rows$requirement
    )
}

# Each row's qualifier values as name=value pairs, sorted by name and joined by
# ";"; NA for a row with none.
qualifier_text <- function(values, rows, by = ".found") {
    joined <- values |>
        arrange(.data[[by]], .data$qualifier) |>
        summarise(
            .by = all_of(by),
            text = paste0(.data$qualifier, "=", .data$value, collapse = ";")
        )
    joined$text[match(rows, joined[[by]])]
}

coverage_verdict <- function(requirements) {
    severities <- SEVERITY[requirements]
    if ("fail" %in% severities) {
        return("fail")
    }
    if ("warn" %in% severities) "warn" else "pass"
}
