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
    requirement <- NA_character_
    for (row in set$requirements) {
        if (row_describes(row, entry)) {
            requirement <- row$requirement
            break
        }
    }
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
    rows <- list()
    for (vendor in set$catalogues) {
        for (entry in vendor$entries) {
            for (pattern in entry$software_version_pattern) {
                rows[[length(rows) + 1L]] <- entry_row(
                    set_name,
                    set,
                    vendor$label,
                    pattern,
                    entry
                )
            }
        }
    }
    for (source in c(set$producers, set$annotations)) {
        for (entry in source$entries) {
            rows[[length(rows) + 1L]] <- entry_row(
                set_name,
                set,
                source$label,
                NA_character_,
                entry
            )
        }
    }
    rows
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
    frame <- data.frame(row.names = seq_along(rows))
    for (column in columns) {
        values <- lapply(rows, function(row) row[[column]])
        if (column %in% lists) {
            frame[[column]] <- I(values)
        } else {
            frame[[column]] <- unlist(lapply(values, function(value) {
                if (is.null(value)) NA else value
            }))
        }
    }
    if (length(rows) == 0L) {
        frame <- as.data.frame(setNames(
            rep(list(character(0)), length(columns)),
            columns
        ))
    }
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
    rows <- list()
    columns <- character(0)
    for (name in names) {
        set <- catalogue_set(name)
        rows <- c(rows, set_rows(name, set))
        columns <- union(columns, set_columns(set))
    }
    qualifiers <- columns[startsWith(columns, "qualifier.")]
    records_frame(
        rows,
        c(KEY_COLUMNS, "key", "kind", qualifiers, "requirement", "entry_id")
    )
}
