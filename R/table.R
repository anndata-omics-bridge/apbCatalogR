# Every catalogue set as one long table: one row per entry and software version
# it applies to; apb_catalog.table.

KEY_COLUMNS <- c(
    "set",
    "source",
    "software_version_pattern",
    "level",
    "location",
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

# One table row: where the entry's field lives, its key and qualifiers, and its
# requirement.
entry_row <- function(set_name, set, source, software_version_pattern, entry) {
    reference <- entry$reference
    requirement <- row_requirement(set$requirements, entry)
    qualifiers <- qualifier_names(set)
    c(
        list(
            set = set_name,
            source = source,
            software_version_pattern = software_version_pattern,
            level = reference$level,
            location = reference$location,
            table = reference$table,
            name = field_name(entry),
            key = entry$key,
            kind = entry$kind
        ),
        setNames(
            as.list(unname(entry$qualifiers[qualifiers])),
            sprintf("qualifier.%s", qualifiers)
        ),
        list(requirement = requirement, entry_id = entry$entry_id)
    )
}

# Every row of one set: vendor entries once per software version pattern, the
# others once, without a pattern.
set_rows <- function(set_name, set) {
    vendor_rows <- concat(lapply(set$catalogues, function(vendor) {
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

set_columns <- function(set) {
    c(
        KEY_COLUMNS,
        "key",
        "kind",
        sprintf("qualifier.%s", qualifier_names(set)),
        "requirement",
        "entry_id"
    )
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

#' The catalogue sets as one long table
#'
#' One row per entry and software version it applies to, as `apb-catalog table`
#' prints it.
#'
#' @param catalogue One set's name; `NULL` lists every set of the bundle.
#' @return A data frame with columns `set`, `source` (the conversion software,
#'   an app or `annotation:<convention>`), `software_version_pattern` (`NA` for
#'   an entry of every software version), `level`, `location`, `table`, `name`
#'   (`prefix:<prefix>` for a field found by column-name prefix), `key`, `kind`,
#'   one `qualifier.<name>` column per other qualifier, `requirement` and
#'   `entry_id`.
#' @export
#' @examples
#' head(catalogue_table("confidence_alpha"))
catalogue_table <- function(catalogue = NULL) {
    names <- if (is.null(catalogue)) set_names() else catalogue
    sets <- lapply(names, catalogue_set)
    rows <- concat(Map(set_rows, names, sets))
    columns <- unique(unlist(lapply(sets, set_columns), use.names = FALSE))
    qualifiers <- columns[startsWith(columns, "qualifier.")]
    records_frame(
        rows,
        c(KEY_COLUMNS, "key", "kind", qualifiers, "requirement", "entry_id")
    )
}
