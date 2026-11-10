# The software versions apb2 converts, as apb-catalog's build wrote them to
# versions.csv, and the levels the bundle's entries name for each;
# apb_catalog.rules.

# Every software version apb2 has a rule for, in apb2's order: its label, which
# entries name, and the rule's version pattern, which a converted result stores.
software_versions <- function(bundle = current_bundle()) {
    bundle$memo("versions", function() {
        rows <- catalogue_rows(bundle, "versions")
        tibble(
            software_name = rows$software,
            version = rows$version,
            software_version_pattern = rows$software_version_pattern
        )
    })
}

# An entry id's version for every version of its source, later ones included,
# and its level for every level, which only an app's or an annotation
# convention's entries name; and an id's key or column where its attribute has
# none.
ANY_VERSION <- "*"
NO_NAME <- "-"
ATTRIBUTES <- c("obs", "var", "layers", "varm")
ID_FIELDS <- c("source", "version", "level", "attribute", "key", "column")

# An entry id's fields, built as CPE names are:
# <source>/<version>/<level>/<attribute>/<key>/<column>, six fields of one value
# each. A layer's name is its key and a varm column's table is its key; obs and
# var have none. `name` is the layer or column; a trailing `*` matches obs
# columns by prefix.
entry_id_fields <- function(text) {
    fields <- strsplit(text, "/", fixed = TRUE)[[1L]]
    if (length(fields) != 6L || endsWith(text, "/")) {
        stop(
            sprintf("entry id '%s' has %d fields, not 6", text, length(fields)),
            call. = FALSE
        )
    }
    names(fields) <- ID_FIELDS
    attribute <- fields[["attribute"]]
    check_id_fields(text, fields)
    check_id_names(text, attribute, fields[["key"]], fields[["column"]])
    list(
        source = fields[["source"]],
        version = fields[["version"]],
        level = fields[["level"]],
        attribute = attribute,
        table = if (attribute == "varm") fields[["key"]] else NA_character_,
        name = if (attribute == "layers") {
            fields[["key"]]
        } else {
            fields[["column"]]
        }
    )
}

# An id names one of ATTRIBUTES and leaves none of its source, version and level
# open.
check_id_fields <- function(text, fields) {
    attribute <- fields[["attribute"]]
    if (!(attribute %in% ATTRIBUTES)) {
        stop(
            sprintf(
                "entry id '%s': '%s' is none of %s",
                text,
                attribute,
                paste(ATTRIBUTES, collapse = ", ")
            ),
            call. = FALSE
        )
    }
    if (
        any(fields[c("source", "version", "level")] == NO_NAME) ||
            fields[["source"]] == ANY_VERSION ||
            any(fields == "")
    ) {
        stop(
            sprintf(
                "entry id '%s' leaves its source, version or level open",
                text
            ),
            call. = FALSE
        )
    }
}

# An id writes a key and a column exactly where its attribute has them: layers a
# key, varm both, obs and var a column.
check_id_names <- function(text, attribute, key, column) {
    has_key <- attribute %in% c("layers", "varm")
    has_column <- attribute != "layers"
    if ((key != NO_NAME) != has_key || (column != NO_NAME) != has_column) {
        stop(
            sprintf(
                "entry id '%s': %s has %s key and %s column",
                text,
                attribute,
                if (has_key) "a" else "no",
                if (has_column) "a" else "no"
            ),
            call. = FALSE
        )
    }
    if (ANY_VERSION %in% c(key, column)) {
        stop(
            sprintf("entry id '%s' names no layer or column", text),
            call. = FALSE
        )
    }
}

# Every entry id of catalog.csv once, parsed into its fields.
entry_ids <- function(bundle = current_bundle()) {
    bundle$memo("entry_ids", function() {
        ids <- unique(row_entry_ids(catalogue_rows(bundle, "catalog")))
        bind_rows(lapply(ids, function(id) {
            c(list(entry_id = id), entry_id_fields(id))
        }))
    })
}

# Every level an entry names for a software version: versions in versions.csv's
# order, each version's levels in the order the entries first name them; `*`
# names every version of its source.
rule_levels <- function(bundle = current_bundle()) {
    bundle$memo("rule_levels", function() {
        named <- entry_ids(bundle) |>
            filter(.data$level != ANY_VERSION) |>
            select("source", label = "version", "level")
        software_versions(bundle) |>
            inner_join(
                named,
                by = c(software_name = "source"),
                relationship = "many-to-many"
            ) |>
            filter(
                .data$label == ANY_VERSION | .data$label == .data$version
            ) |>
            distinct(
                .data$software_name,
                .data$software_version_pattern,
                .data$version,
                .data$level
            )
    })
}

# The rule levels of one software whose version pattern admits a version.
rules_matching <- function(software, version) {
    rule_levels() |>
        filter(
            tolower(.data$software_name) == tolower(.env$software),
            vapply(
                .data$software_version_pattern,
                grepl,
                logical(1),
                x = .env$version,
                perl = TRUE
            )
        )
}

# The concise reading of each rule's software version pattern: the label of the
# first version with that pattern.
version_label <- function(patterns) {
    versions <- software_versions()
    versions$version[match(patterns, versions$software_version_pattern)]
}
