# Every set as one long table: one row per entry and software version it
# applies to; apb_catalog.table.

KEY_COLUMNS <- c(
    "set",
    "source",
    "version",
    "software_version_pattern",
    "level",
    "slot",
    "table",
    "name"
)

# The column an entry names: its name, or the prefix it is found by.
field_name <- function(entry) {
    reference <- entry$reference
    if (!is.na(reference$name)) {
        return(reference$name)
    }
    paste0("prefix:", reference$prefix)
}

# The set's qualifiers, in vocabulary order; the kind is a column of its own.
qualifier_names <- function(set) {
    names(set$vocabulary$qualifiers)
}

# One qualifier.<name> column per qualifier of the set, NA where no value is
# given.
qualifier_columns <- function(set, values) {
    qualifiers <- qualifier_names(set)
    setNames(
        as.list(unname(values[qualifiers])),
        sprintf("qualifier.%s", qualifiers)
    )
}

# The entry's version label: the one its id names, and for a vendor entry of
# every version (`*`) the label of the pattern the row applies to; apps and
# annotations keep `*`.
entry_version <- function(entry, software_version_pattern) {
    named <- strsplit(entry$entry_id, "/", fixed = TRUE)[[1L]][[2L]]
    if (
        named == ANY_VERSION &&
            length(entry$software_version_pattern) > 0L &&
            !is.na(software_version_pattern)
    ) {
        return(version_label(software_version_pattern))
    }
    named
}

# One table row: who writes the field and for which version, where it lives,
# its key and qualifiers, and its requirement.
entry_row <- function(set_name, set, source, software_version_pattern, entry) {
    reference <- entry$reference
    requirement <- row_requirement(set$requirements, entry)
    c(
        list(
            set = set_name,
            source = source,
            version = entry_version(entry, software_version_pattern),
            software_version_pattern = software_version_pattern,
            level = reference$level,
            slot = reference$location,
            table = reference$table,
            name = field_name(entry),
            key = entry$key,
            kind = entry$kind
        ),
        qualifier_columns(set, entry$qualifiers),
        list(requirement = requirement, entry_id = entry$entry_id)
    )
}

# Every row of one set: vendor entries once per software version pattern, the
# others once, without a pattern.
set_rows <- function(set_name, set) {
    vendor_rows <- concat(lapply(set$vendors, function(vendor) {
        concat(lapply(vendor$entries, function(entry) {
            lapply(entry$software_version_pattern, function(pattern) {
                entry_row(set_name, set, vendor$label, pattern, entry)
            })
        }))
    }))
    other_rows <- concat(lapply(
        c(set$producers, set$annotations),
        function(source) {
            lapply(source$entries, function(entry) {
                entry_row(set_name, set, source$label, NA_character_, entry)
            })
        }
    ))
    c(vendor_rows, other_rows)
}

# The table's columns around the given qualifier.<name> columns.
table_columns <- function(qualifiers) {
    c(KEY_COLUMNS, "key", "kind", qualifiers, "requirement", "entry_id")
}

# The table's columns for one set: one qualifier.<name> column per qualifier.
set_columns <- function(set) {
    table_columns(sprintf("qualifier.%s", qualifier_names(set)))
}

# A data frame from rows given as named lists; a column a row lacks is NA there.
# `lists` names list columns.
records_frame <- function(rows, columns, lists = character(0)) {
    if (length(rows) == 0L) {
        return(as.data.frame(setNames(
            rep(list(character(0)), length(columns)),
            columns
        )))
    }
    frame <- Reduce(
        function(frame, column) {
            values <- lapply(rows, function(row) row[[column]])
            frame[[column]] <- if (column %in% lists) {
                I(values)
            } else {
                unlist(lapply(values, function(value) value %||% NA))
            }
            frame
        },
        columns,
        data.frame(row.names = seq_along(rows))
    )
    rownames(frame) <- NULL
    frame
}

#' The catalog as one long table
#'
#' The whole catalog, every set of the bundle, or one set: one row per entry
#' and software version it applies to, as `apb-catalog table` prints it.
#'
#' @param set_name One set's name; `NULL` lists every set of the bundle.
#' @return A data frame with columns `set`, `source` (who writes the field: the
#'   quantification software, an app or `annotation:<convention>`), `version`
#'   (the version label; `*` for an app's or an annotation's entry, which
#'   applies to every version), `software_version_pattern` (`NA` for an entry
#'   of every software version), `level`, `slot` (`layers`, `var`, `obs` or
#'   `varm`), `table` (the `varm` table, else `NA`), `name` (the field's name;
#'   `prefix:<prefix>` for a field found by column-name prefix), `key`, `kind`,
#'   one `qualifier.<name>` column per other qualifier, `requirement` and
#'   `entry_id`.
#' @export
#' @examples
#' head(catalog_table("confidence_alpha"))
catalog_table <- function(set_name = NULL) {
    names <- if (is.null(set_name)) set_names() else set_name
    loaded <- lapply(names, load_set)
    rows <- concat(Map(set_rows, names, loaded))
    columns <- unique(unlist(lapply(loaded, set_columns), use.names = FALSE))
    records_frame(
        rows,
        table_columns(columns[startsWith(columns, "qualifier.")])
    )
}
