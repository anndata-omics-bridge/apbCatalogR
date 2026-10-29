# Every catalogue set as one long table: one row per entry and rule it applies to; apb_catalog.table.

KEY_COLUMNS <- c("set", "source", "version_pattern", "rule", "level", "location", "table", "name")

# The rule of an entry that applies to every rule: a producer's or a sample annotation's.
ANY_RULE <- "*"

# The column an entry names: its name, or the role or prefix it is found by.
field_name <- function(entry) {
  reference <- entry$reference
  if (!is.na(reference$name)) {
    return(reference$name)
  }
  if (!is.na(reference$role)) {
    return(paste0("role:", reference$role))
  }
  paste0("prefix:", reference$prefix)
}

# One table row: where the entry's field lives, its qualifiers and its requirement.
entry_row <- function(set_name, set, source, version_pattern, rule, entry) {
  reference <- entry$reference
  requirement <- NA_character_
  for (row in set$requirements) {
    if (row_describes(row, entry)) {
      requirement <- row$requirement
      break
    }
  }
  qualifiers <- names(set$vocabulary$qualifiers)
  c(
    list(
      set = set_name,
      source = source,
      version_pattern = version_pattern,
      rule = rule,
      level = reference$level,
      location = reference$location,
      table = reference$table,
      name = field_name(entry)
    ),
    stats::setNames(as.list(unname(entry$qualifiers[qualifiers])), paste0("qualifier.", qualifiers)),
    list(requirement = requirement, entry_id = entry$entry_id)
  )
}

# Every row of one set: vendor entries once per rule, the others once.
set_rows <- function(set_name, set) {
  rows <- list()
  for (vendor in set$catalogues) {
    patterns <- stats::setNames(
      vapply(vendor$variants, function(variant) variant$software_version_pattern, character(1)),
      vapply(vendor$variants, function(variant) variant$rule, character(1))
    )
    for (entry in vendor$entries) {
      for (rule in entry$rules) {
        rows[[length(rows) + 1L]] <- entry_row(set_name, set, vendor$label, patterns[[rule]], rule, entry)
      }
    }
  }
  for (source in c(set$producers, set$annotations)) {
    for (entry in source$entries) {
      rows[[length(rows) + 1L]] <- entry_row(set_name, set, source$label, NA_character_, ANY_RULE, entry)
    }
  }
  rows
}

set_columns <- function(set) {
  c(KEY_COLUMNS, paste0("qualifier.", names(set$vocabulary$qualifiers)), "requirement", "entry_id")
}

# A data frame from rows given as named lists; a column a row lacks is NA there. `lists` names list columns.
records_frame <- function(rows, columns, lists = character(0)) {
  frame <- data.frame(row.names = seq_along(rows))
  for (column in columns) {
    values <- lapply(rows, function(row) row[[column]])
    if (column %in% lists) {
      frame[[column]] <- I(values)
    } else {
      frame[[column]] <- unlist(lapply(values, function(value) if (is.null(value)) NA else value))
    }
  }
  if (length(rows) == 0L) {
    frame <- as.data.frame(stats::setNames(rep(list(character(0)), length(columns)), columns))
  }
  rownames(frame) <- NULL
  frame
}

#' The catalogue sets as one long table
#'
#' One row per entry and rule it applies to, as `apb-catalog table` prints it.
#'
#' @param catalogue One set's name; `NULL` lists every set of the bundle.
#' @return A data frame with columns `set`, `source` (the vendor, `producer:<name>` or `annotation:<convention>`),
#'   `version_pattern`, `rule` (`*` for an entry of every rule), `level`, `location`, `table`, `name` (`role:<role>`
#'   or `prefix:<prefix>` for a field found by role or column-name prefix), one `qualifier.<name>` column per
#'   qualifier, `requirement` and `entry_id`.
#' @export
#' @examples
#' head(catalogue_table("confidence"))
catalogue_table <- function(catalogue = NULL) {
  names <- if (is.null(catalogue)) set_names() else catalogue
  rows <- list()
  columns <- character(0)
  for (name in names) {
    set <- catalogue_set(name)
    rows <- c(rows, set_rows(name, set))
    columns <- union(columns, set_columns(set))
  }
  qualifiers <- columns[startsWith(columns, "qualifier.")]
  records_frame(rows, c(KEY_COLUMNS, qualifiers, "requirement", "entry_id"))
}
