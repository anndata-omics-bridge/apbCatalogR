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
field_name <- function(name, prefix) {
    if_else(is.na(name), paste0("prefix:", prefix), name)
}

# The set's qualifiers, in vocabulary order; the kind is a column of its own.
qualifier_names <- function(set) {
    names(set$vocabulary$qualifiers)
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

# The requirement rows each location of a requirement reads: var reads var and
# varm.
requirement_reads <- function(requirements) {
    reads <- tibble(
        location = rep(names(READS), lengths(READS)),
        read = unlist(READS, use.names = FALSE)
    )
    requirements |>
        mutate(.requirement = seq_len(nrow(requirements))) |>
        inner_join(reads, by = "location", relationship = "many-to-many") |>
        select(
            ".requirement",
            required_level = "level",
            "read",
            "key",
            "requirement"
        )
}

# Each entry's requirement: that of the first requirement row on its level, or
# on every level, whose location reads the entry's field and whose key is the
# entry's; NA when none.
entry_requirements <- function(entries, requirements) {
    found <- entries |>
        mutate(.row = seq_len(nrow(entries))) |>
        select(".row", "level", "location", "key") |>
        inner_join(
            requirement_reads(requirements),
            by = c("key", location = "read"),
            relationship = "many-to-many"
        ) |>
        filter(
            .data$level == .data$required_level | .data$level == ANY_VERSION
        ) |>
        summarise(
            .by = ".row",
            requirement = .data$requirement[which.min(.data$.requirement)]
        )
    requirement <- rep(NA_character_, nrow(entries))
    requirement[found$.row] <- found$requirement
    requirement
}

# The table rows of a set's entries: where each field lives, its key and
# qualifiers, and its requirement; `pattern` gives each row's software version
# pattern.
entry_rows <- function(set, entries, pattern) {
    set_name <- set$name
    requirement <- entry_requirements(entries, set$requirements)
    entries |>
        mutate(
            set = .env$set_name,
            software_version_pattern = .env$pattern,
            name = field_name(.data$name, .data$prefix),
            requirement = .env$requirement
        ) |>
        select(all_of(set_columns(set)))
}

# Every row of one set: vendor entries once per software version pattern, the
# others once, without a pattern.
set_rows <- function(set) {
    entries <- set$entries |>
        left_join(set$patterns, by = "entry_rank") |>
        arrange(
            match(.data$type, c("vendor", "producer", "annotation")),
            .data$source_rank,
            .data$entry_rank,
            .data$pattern_rank
        )
    entry_rows(set, entries, entries$software_version_pattern)
}

# A data frame of the columns asked for, in that order; a column the rows lack
# is NA.
column_frame <- function(rows, columns) {
    rows <- as.data.frame(rows %||% tibble())
    missing <- setdiff(columns, names(rows))
    rows[missing] <- lapply(missing, function(column) {
        rep(NA_character_, nrow(rows))
    })
    rows[columns]
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
    sets <- lapply(catalogue %||% set_names(), catalogue_set)
    qualifiers <- unique(unlist(
        lapply(sets, function(set) {
            sprintf("qualifier.%s", qualifier_names(set))
        }),
        use.names = FALSE
    ))
    column_frame(
        bind_rows(lapply(sets, set_rows)),
        c(KEY_COLUMNS, "key", "kind", qualifiers, "requirement", "entry_id")
    )
}
