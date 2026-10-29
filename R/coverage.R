# Which catalogued fields every rule supplies, and whether each level meets its set; apb_catalog.coverage.

COVERAGE_COLUMNS <- c(
  "software",
  "version",
  "rule",
  "level",
  "kind",
  "qualifiers",
  "origin",
  "producer",
  "requirement",
  "location",
  "table",
  "column",
  "inputs",
  "needs",
  "verdict"
)

SEVERITY <- c(MUST = "fail", SHOULD = "warn", MAY = "warn")

#' The catalogue sets
#'
#' Every catalogue set of the bundle with its purpose, its users and its number of entries.
#'
#' @return A data frame with columns `catalogue`, `purpose`, `used_by` (a list column), `has_requirements` and
#'   `entries`.
#' @export
#' @examples
#' catalogues()
catalogues <- function() {
  rows <- lapply(set_names(), function(name) {
    set <- catalogue_set(name)
    list(
      catalogue = name,
      purpose = set$description$purpose,
      used_by = set$description$used_by,
      has_requirements = length(set$requirements) > 0L,
      entries = sum(vapply(set$sources(), function(source) length(source$entries), integer(1)))
    )
  })
  records_frame(rows, c("catalogue", "purpose", "used_by", "has_requirements", "entries"), lists = "used_by")
}

#' Check every rule against one catalogue set
#'
#' One row per catalogued or required field of each rule level the set reviews; a set without vendor catalogues is
#' checked against every rule level. A set with requirements judges each level: `fail` when a MUST field is
#' unsupported, `warn` when a SHOULD or MAY field is, `pass` otherwise.
#'
#' @param catalogue The set's name.
#' @return A data frame with columns `software`, `version`, `rule`, `level`, `kind`, `qualifiers` (the other
#'   qualifiers as `key=value`), `origin`, `producer` (`NA` for the vendor's own fields), `requirement`, `location`,
#'   `table`, `column` (`NA` when unsupported), `inputs` (a list column of the vendor columns apb2 computes the column
#'   from), `needs` (what to run before the field exists) and `verdict` (`NA` for a set without requirements).
#' @export
#' @examples
#' head(rule_coverage("abundance"))
rule_coverage <- function(catalogue) {
  set <- catalogue_set(catalogue)
  rows <- list()
  for (vendor in set$catalogues) {
    for (variant in vendor$variants) {
      rows <- c(rows, variant_rows(set, vendor$software_name, variant, entries_for(vendor, variant)))
    }
  }
  if (!set$needs_review) {
    for (found in rule_levels()) {
      variant <- list(
        rule = found$rule,
        level = found$level,
        software_version_pattern = found$software_version_pattern
      )
      rows <- c(rows, variant_rows(set, found$software_name, variant, list()))
    }
  }
  records_frame(rows, COVERAGE_COLUMNS, lists = "inputs")
}

variant_rows <- function(set, software, variant, vendor_entries) {
  found <- c(
    lapply(vendor_entries, function(entry) list(entry = entry, producer = NULL)),
    producer_fields(set$producers, variant)
  )
  required <- Filter(function(row) row$level == variant$level, set$requirements)
  missing <- Filter(
    function(row) {
      !any(vapply(found, function(item) row_describes(row, item$entry), logical(1)))
    },
    required
  )
  verdict <- if (length(required) > 0L) coverage_verdict(vapply(missing, function(row) row$requirement, "")) else NA
  base <- list(
    software = software,
    version = version_label(variant$software_version_pattern),
    rule = variant$rule,
    level = variant$level,
    verdict = verdict
  )
  rows <- lapply(found, function(item) {
    c(
      base,
      coverage_field(item$entry, item$producer, variant),
      list(requirement = row_requirement(required, item$entry))
    )
  })
  rows <- c(
    rows,
    lapply(missing, function(row) {
      c(
        base,
        list(
          kind = row$kind,
          qualifiers = qualifier_pairs(row$qualifiers),
          location = row$location,
          requirement = row$requirement
        )
      )
    })
  )
  if (length(rows) == 0L) {
    rows <- list(base)
  }
  required_order(rows, required)
}

# The producer fields that count on a rule level: where conversion keeps their column, or once their producer runs.
producer_fields <- function(producers, variant) {
  roles <- rule_roles(variant$rule, variant$level)
  unlist(
    lapply(producers, function(producer) {
      bound <- unlist(lapply(entries_on(producer, variant$level), coverage_bound, roles), recursive = FALSE)
      counted <- Filter(
        function(entry) length(producer$requires) > 0L || variant_retains(variant, entry$reference),
        bound
      )
      lapply(counted, function(entry) list(entry = entry, producer = producer))
    }),
    recursive = FALSE
  )
}

# Rows in the order of the requirements their kind answers, vendor fields before producer fields; stable otherwise.
required_order <- function(rows, required) {
  kinds <- vapply(required, function(row) row$kind, character(1))
  ranks <- vapply(
    rows,
    function(row) {
      index <- match(row$kind %||% NA_character_, kinds)
      if (is.na(index)) length(required) else index - 1L
    },
    numeric(1)
  )
  produced <- vapply(rows, function(row) !is.null(row$producer), logical(1))
  rows[order(ranks, produced, method = "radix")]
}

# A producer entry bound to the rule's column: its role's column when it names a role.
coverage_bound <- function(entry, roles) {
  role <- entry$reference$role
  if (is.na(role)) {
    return(list(entry))
  }
  if (role %in% names(roles)) list(bound_entry(entry, roles[[role]])) else list()
}

variant_retains <- function(variant, reference) {
  reference_key(reference) %in% rule_retained(variant$rule, variant$level)
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
  if (length(severities) > 0L) "warn" else "pass"
}

coverage_field <- function(entry, producer, variant) {
  reference <- entry$reference
  others <- entry$qualifiers[names(entry$qualifiers) != "kind"]
  origin <- if ("origin" %in% names(others)) others[["origin"]] else NA_character_
  others <- others[names(others) != "origin"]
  inputs <- if (reference$location == "var" && variant_retains(variant, reference)) {
    rule_var_inputs(variant$rule, variant$level, reference$name)
  } else {
    character(0)
  }
  list(
    kind = entry$qualifiers[["kind"]],
    qualifiers = qualifier_pairs(others),
    origin = origin,
    producer = if (is.null(producer)) NULL else producer$producer,
    location = reference$location,
    table = reference$table,
    column = reference$name,
    inputs = inputs,
    needs = if (is.null(producer)) NA_character_ else producer$instruction
  )
}
