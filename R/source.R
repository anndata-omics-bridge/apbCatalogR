# Reviewed meanings of retained APB fields, per vendor and software version;
# apb_catalog.source and .vocabulary.

UNKNOWN <- "unknown"

# The locations a request reads: var and varm both hold one value per feature.
READS <- list(
    obs = "obs",
    layers = "layers",
    var = c("var", "varm"),
    varm = "varm"
)

# A request accepts one of the set's kinds, or none, and any subset of the
# qualifiers, each with admitted values; every qualifier with values admits
# "unknown".
check_predicate <- function(vocabulary, kind, qualifiers) {
    if (!is.null(kind)) {
        check_kind(vocabulary, kind)
    }
    for (name in names(qualifiers)) {
        accepted <- qualifiers[[name]]
        if (length(accepted) == 0L) {
            stop(
                sprintf("qualifier '%s' accepts no value", name),
                call. = FALSE
            )
        }
        for (value in accepted) {
            check_value(vocabulary, name, value)
        }
    }
}

check_kind <- function(vocabulary, kind) {
    if (!(kind %in% names(vocabulary$kinds))) {
        stop(
            sprintf(
                "the set does not admit the kind '%s'; kinds: %s",
                kind,
                paste(sort_c(names(vocabulary$kinds)), collapse = ", ")
            ),
            call. = FALSE
        )
    }
}

check_value <- function(vocabulary, name, value) {
    qualifier <- vocabulary$qualifiers[[name]]
    if (is.null(qualifier)) {
        stop(
            sprintf(
                "unknown qualifier '%s'; declared: %s",
                name,
                paste(sort_c(names(vocabulary$qualifiers)), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    if (!(value %in% names(qualifier$values))) {
        stop(
            sprintf(
                "qualifier '%s' does not admit '%s'; admitted: %s",
                name,
                value,
                paste(sort_c(names(qualifier$values)), collapse = ", ")
            ),
            call. = FALSE
        )
    }
}

# The location, varm table and name a level's retained fields are indexed by, as
# one string.
address_key <- function(location, table, name) {
    if (length(name) == 0L) {
        return(character(0))
    }
    paste(location, ifelse(is.na(table), "", table), name, sep = "\x1f")
}

reference_key <- function(reference) {
    if (is.na(reference$name)) {
        stop("the reference is not bound to a column", call. = FALSE)
    }
    address_key(reference$location, reference$table, reference$name)
}

# A ";"-separated list of one CSV cell; none when the cell is empty.
cell_list <- function(text, separator = ";") {
    if (identical(text, "")) {
        character(0)
    } else {
        strsplit(text, separator, fixed = TRUE)[[1L]]
    }
}

# A catalog.csv row's entry id: its software, location and name joined by
# `/`.
row_entry_ids <- function(rows) {
    paste(rows$software, rows$location, rows$name, sep = "/")
}

# A catalog.csv row pairs a qualifier with a value, never qualifies the kind,
# and marks fixed as true or false; the first row that does not is refused.
check_pairs <- function(rows) {
    bad <- (rows$qualifier == "") != (rows$value == "") |
        rows$qualifier == "kind" |
        !(rows$fixed %in% c("true", "false")) |
        (rows$fixed == "true" & rows$qualifier == "")
    first <- which(bad)[1L]
    if (!is.na(first)) {
        check_pair(
            rows$entry_id[[first]],
            rows$qualifier[[first]],
            rows$value[[first]],
            rows$fixed[[first]]
        )
    }
}

check_pair <- function(label, qualifier, value, fixed) {
    if ((qualifier == "") != (value == "")) {
        stop(
            sprintf(
                "catalog.csv: %s has a qualifier and a value only together",
                label
            ),
            call. = FALSE
        )
    }
    if (qualifier == "kind") {
        stop(
            sprintf(
                "catalog.csv: %s qualifies the kind, %s",
                label,
                "which its kind column holds"
            ),
            call. = FALSE
        )
    }
    if (
        !(fixed %in% c("true", "false")) || (fixed == "true" && qualifier == "")
    ) {
        stop(
            sprintf(
                "catalog.csv: %s has fixed '%s'; %s",
                label,
                fixed,
                "true on a row with a qualifier, else false"
            ),
            call. = FALSE
        )
    }
}


# Every key of every set, from terms.csv, the catalogue's root: the name callers
# ask by, its kind and PSI-MS terms, the same on every row of the key.
# apb-catalog checks the bundle's terms when it writes them.
bundle_keys <- function(bundle) {
    bundle$memo("keys", function() {
        keys <- catalogue_rows(bundle, "terms") |>
            summarise(
                .by = all_of(c("set", "key")),
                kinds = list(sort_c(unique(.data$kind))),
                terms = list(sort_c(unique(.data$psi_ms)))
            )
        bad <- which(lengths(keys$kinds) != 1L | lengths(keys$terms) != 1L)
        if (length(bad) > 0L) {
            first <- bad[[1L]]
            stop(
                sprintf(
                    "%s: key '%s' has the kinds %s and the PSI-MS terms %s; %s",
                    keys$set[[first]],
                    keys$key[[first]],
                    paste(keys$kinds[[first]], collapse = ", "),
                    paste(keys$terms[[first]], collapse = ", "),
                    "every row of a key has one of each"
                ),
                call. = FALSE
            )
        }
        keys |>
            mutate(kind = unlist(.data$kinds), psi_ms = unlist(.data$terms)) |>
            select("set", "key", "kind", "psi_ms")
    })
}

# The qualifier values each key fixes: one row per qualifier, the first a key's
# terms.csv rows name.
bundle_fixed <- function(bundle) {
    bundle$memo("fixed", function() {
        catalogue_rows(bundle, "terms") |>
            filter(.data$qualifier != "") |>
            distinct(.data$set, .data$key, .data$qualifier, .keep_all = TRUE) |>
            select("set", "key", "qualifier", "value")
    })
}

# The requirement rows of a set's terms.csv: one per term, a key at one
# location, that states one, as level, location, key and requirement.
set_requirements <- function(bundle, name) {
    catalogue_rows(bundle, "terms") |>
        filter(.data$set == .env$name, .data$requirement != "") |>
        distinct(.data$key, .data$location, .keep_all = TRUE) |>
        mutate(
            level = sub("/.*$", "", .data$location),
            location = sub("/.*$", "", sub("^[^/]*/?", "", .data$location))
        ) |>
        select("level", "location", "key", "requirement")
}

# Every catalogued field of the bundle, one row per field and key: where it
# lives (`level`, `location`, `table`, and `name` or the obs column `prefix`),
# its key and kind, its qualifier values as `qualifier.<name>` columns, and its
# `source` and its `type`: vendor (conversion software), producer (an app) or
# annotation. Rows are in catalogue order: sets as sets.csv lists them, vendors
# in rule-level order, then apps, then annotation conventions, each source's
# fields in the order catalog.csv first names them; `entry_rank` identifies a
# row.
bundle_entries <- function(bundle) {
    bundle$memo("entries", function() {
        rows <- catalogue_rows(bundle, "catalog")
        rows$entry_id <- row_entry_ids(rows)
        check_pairs(rows)
        keys <- bundle_keys(bundle)
        unkeyed <- anti_join(rows, keys, by = c("set", "key"))
        if (nrow(unkeyed) > 0L) {
            stop(
                sprintf(
                    "%s: a field has no key",
                    sub("/.*$", "", unkeyed$software[[1L]])
                ),
                call. = FALSE
            )
        }
        qualifiers <- rows |>
            filter(.data$qualifier != "") |>
            select("set", "entry_id", "key", "qualifier", "value") |>
            pivot_wider(
                names_from = "qualifier",
                values_from = "value",
                names_prefix = "qualifier.",
                values_fn = function(values) values[[1L]]
            )
        entries <- rows |>
            distinct(.data$set, .data$entry_id, .data$key) |>
            left_join(entry_ids(bundle), by = "entry_id") |>
            left_join(
                select(keys, "set", "key", "kind"),
                by = c("set", "key")
            ) |>
            left_join(qualifiers, by = c("set", "entry_id", "key")) |>
            mutate(
                location = .data$attribute,
                prefix = if_else(
                    endsWith(.data$name, "*"),
                    sub("\\*$", "", .data$name),
                    NA_character_
                ),
                name = if_else(is.na(.data$prefix), .data$name, NA_character_)
            ) |>
            select(-"attribute")
        typed_entries(bundle, entries)
    })
}

# Each entry's type and its place in catalogue order. A vendor no packaged rule
# declares is refused.
typed_entries <- function(bundle, entries) {
    producers <- catalogue_rows(bundle, "producers")
    software <- unique(rule_levels(bundle)$software_name)
    typed <- entries |>
        mutate(
            type = if_else(
                startsWith(.data$source, "annotation:"),
                "annotation",
                if_else(
                    paste(.data$set, .data$source) %in%
                        paste(producers$set, producers$producer),
                    "producer",
                    "vendor"
                )
            )
        )
    unknown <- filter(
        typed,
        .data$type == "vendor",
        !(.data$source %in% software)
    )
    if (nrow(unknown) > 0L) {
        set <- unknown$set[[1L]]
        stop(
            sprintf(
                "%s: no packaged rule declares the software %s",
                set,
                paste(
                    sort_c(unique(unknown$source[unknown$set == set])),
                    collapse = ", "
                )
            ),
            call. = FALSE
        )
    }
    typed |>
        mutate(entry_rank = seq_len(nrow(typed))) |>
        mutate(
            .by = "set",
            source_rank = if_else(
                .data$type == "vendor",
                match(.data$source, software),
                match(.data$source, unique(.data$source))
            )
        ) |>
        arrange(
            match(.data$set, catalogue_rows(bundle, "sets")$set),
            match(.data$type, c("vendor", "producer", "annotation")),
            .data$source_rank,
            .data$entry_rank
        )
}

# The rule version patterns each vendor entry was reviewed for: its version's,
# or for `*` every pattern of its software, in versions.csv's order.
vendor_patterns <- function(bundle) {
    bundle$memo("patterns", function() {
        vendor <- bundle_entries(bundle) |>
            filter(.data$type == "vendor") |>
            select("entry_rank", "source", label = "version")
        versions <- software_versions(bundle) |>
            mutate(pattern_rank = seq_along(.data$version))
        matched <- vendor |>
            inner_join(
                versions,
                by = c(source = "software_name"),
                relationship = "many-to-many"
            ) |>
            filter(
                .data$label == ANY_VERSION | .data$label == .data$version
            )
        unmatched <- vendor |>
            filter(.data$label != ANY_VERSION) |>
            anti_join(matched, by = "entry_rank")
        if (nrow(unmatched) > 0L) {
            stop(
                sprintf(
                    "versions.csv has no %s version '%s'",
                    unmatched$source[[1L]],
                    unmatched$label[[1L]]
                ),
                call. = FALSE
            )
        }
        select(
            matched,
            "entry_rank",
            "software_version_pattern",
            "pattern_rank"
        )
    })
}

# The qualifier values a request for a key accepts besides its kind: the values
# it fixes.
key_accepted <- function(found) {
    as.list(found$fixed)
}

# A set's vocabulary from its sets.csv row, its kinds, its qualifiers and their
# values, shaped as apb_catalog's SetVocabulary.
set_vocabulary <- function(head, kind_rows, qualifier_rows, value_rows) {
    qualifiers <- lapply(seq_len(nrow(qualifier_rows)), function(index) {
        values <- value_rows[
            value_rows$qualifier == qualifier_rows$qualifier[[index]],
            ,
            drop = FALSE
        ]
        list(
            definition = qualifier_rows$definition[[index]],
            values = setNames(as.list(values$definition), values$value)
        )
    })
    list(
        prefix = head$prefix,
        definition = head$definition,
        kind_definition = head$kind_definition,
        kinds = setNames(as.list(kind_rows$definition), kind_rows$kind),
        qualifiers = setNames(qualifiers, qualifier_rows$qualifier)
    )
}

# The vendor entries a set reviewed for one software version's level.
entries_for <- function(set, software, pattern, level) {
    reviewed <- filter(set$patterns, .data$software_version_pattern == pattern)
    set$entries |>
        filter(
            .data$type == "vendor",
            .data$source == .env$software,
            .data$level == .env$level
        ) |>
        semi_join(reviewed, by = "entry_rank")
}

# The entries on one level: those of the level, and those of every level (`*`)
# taking the level in their id and reference.
entries_on <- function(entries, level) {
    entries |>
        filter(.data$level %in% c(.env$level, ANY_VERSION)) |>
        mutate(
            entry_id = if_else(
                .data$level == ANY_VERSION,
                paste(
                    sub("^([^/]*/[^/]*)/.*$", "\\1", .data$entry_id),
                    .env$level,
                    sub("^([^/]*/){3}", "", .data$entry_id),
                    sep = "/"
                ),
                .data$entry_id
            ),
            level = .env$level
        )
}

# The entries bound to the level's obs columns: an entry found by prefix binds
# to every column it matches, each named in the entry's id, so a lookup reports
# them as ambiguous; one that matches none drops out.
bind_entries <- function(entries, obs_columns) {
    entries <- mutate(entries, .position = seq_len(nrow(entries)))
    columns <- tibble(.column = obs_columns, .order = seq_along(obs_columns))
    bound <- entries |>
        filter(!is.na(.data$prefix)) |>
        cross_join(columns) |>
        filter(startsWith(tolower(.data$.column), tolower(.data$prefix))) |>
        mutate(
            entry_id = paste0(
                sub("/[^/]*$", "", .data$entry_id),
                "/",
                .data$.column
            ),
            name = .data$.column,
            prefix = NA_character_
        )
    entries |>
        filter(is.na(.data$prefix)) |>
        mutate(.order = 0L) |>
        bind_rows(bound) |>
        arrange(.data$.position, .data$.order) |>
        select(-any_of(c(".position", ".order", ".column")))
}

# The address keys of entries' fields.
entry_addresses <- function(entries) {
    if (is.null(entries) || nrow(entries) == 0L) {
        return(character(0))
    }
    address_key(entries$location, entries$table, entries$name)
}

# The entries whose field the level holds.
held_entries <- function(entries, retained) {
    entries[entry_addresses(entries) %in% retained, ]
}

# Whether a result's metadata records the path a producer's run writes.
producer_ran <- function(requires, metadata) {
    section <- metadata
    for (depth in seq_along(requires)) {
        key <- requires[[depth]]
        if (!(key %in% names(section))) {
            return(FALSE)
        }
        if (depth == length(requires)) {
            break
        }
        section <- section[[key]]
        if (!is.list(section) || is.null(names(section))) {
            return(FALSE)
        }
    }
    TRUE
}

# Why a producer's entries wait: it has not run, and what to run.
pending_reason <- function(producer, instruction) {
    sprintf("%s has not run on this result; %s", producer, instruction)
}

# The annotation convention of an annotation source.
source_convention <- function(source) {
    sub("^annotation:", "", source)
}

# One packaged catalogue set: its vocabulary, keys, requirements, apps and
# annotation conventions, and its entries, the bundle's rows of the set with a
# column for each of its qualifiers. A set with vendor entries answers only for
# a reviewed rule level; one made only of apps' and annotation entries needs no
# review.
CatalogueSet <- R6Class(
    "CatalogueSet",
    public = list(
        name = NULL,
        vocabulary = NULL,
        vocabulary_version = NULL,
        description = NULL,
        keys = NULL,
        fixed = NULL,
        requirements = NULL,
        producers = NULL,
        conventions = NULL,
        entries = NULL,
        patterns = NULL,

        initialize = function(bundle, name) {
            rows <- function(table) {
                found <- catalogue_rows(bundle, table)
                found[found$set == name, , drop = FALSE]
            }
            head <- set_head(rows("sets"), name)
            self$name <- name
            self$vocabulary <- set_vocabulary(
                head,
                rows("kinds"),
                rows("qualifiers"),
                rows("qualifier_values")
            )
            self$vocabulary_version <- bundle$json(
                "catalogues",
                "datapackage.json"
            )$version
            self$description <- list(
                purpose = head$purpose,
                used_by = cell_list(head$used_by)
            )
            self$keys <- filter(bundle_keys(bundle), .data$set == .env$name)
            self$fixed <- filter(bundle_fixed(bundle), .data$set == .env$name)
            self$requirements <- set_requirements(bundle, name)
            self$entries <- set_entries(bundle, name, self$vocabulary)
            self$patterns <- semi_join(
                vendor_patterns(bundle),
                self$entries,
                by = "entry_rank"
            )
            self$producers <- set_producers(rows("producers"), self$entries)
            self$conventions <- unique(source_convention(
                self$entries$source[self$entries$type == "annotation"]
            ))
        },

        # One of the set's keys, with the qualifier values it fixes; an unknown
        # name signals an error naming the set's keys.
        key = function(name) {
            found <- self$keys[self$keys$key == name, , drop = FALSE]
            if (nrow(found) == 0L) {
                known <- paste(sort_c(self$keys$key), collapse = ", ")
                stop(
                    sprintf("unknown key '%s'; keys: %s", name, known),
                    call. = FALSE
                )
            }
            fixed <- self$fixed[self$fixed$key == name, , drop = FALSE]
            list(
                key = name,
                kind = found$kind[[1L]],
                fixed = setNames(fixed$value, fixed$qualifier),
                psi_ms = cell_list(found$psi_ms[[1L]])
            )
        },

        # Whether the set reviewed one software version's level: it has vendor
        # entries and the bundle's entries name that level for the version.
        reviewed = function(software, pattern, level) {
            self$needs_review &&
                any(
                    rule_levels()$software_name == software &
                        rule_levels()$software_version_pattern == pattern &
                        rule_levels()$level == level
                )
        }
    ),
    active = list(
        # Whether a level answers only once a vendor catalogue reviewed its
        # rule.
        needs_review = function() any(self$entries$type == "vendor")
    )
)

# A set's rows of the bundle's entries, with exactly its qualifier columns, in
# vocabulary order.
set_entries <- function(bundle, name, vocabulary) {
    columns <- sprintf("qualifier.%s", names(vocabulary$qualifiers))
    entries <- filter(bundle_entries(bundle), .data$set == .env$name)
    entries[setdiff(columns, names(entries))] <- NA_character_
    others <- grep("^qualifier[.]", names(entries), value = TRUE)
    select(entries, -all_of(setdiff(others, columns)))
}

# The apps a set's entries come from, in catalogue order, with the run each
# waits for (`requires`, a list column) and what to run.
set_producers <- function(runs, entries) {
    sources <- unique(entries$source[entries$type == "producer"])
    found <- runs[match(sources, runs$producer), , drop = FALSE]
    tibble(
        producer = sources,
        requires = lapply(found$requires, cell_list, separator = "."),
        instruction = if_else(
            found$instruction == "",
            NA_character_,
            found$instruction
        )
    )
}

# The sets.csv row describing the set; an unknown set signals an error naming
# the packaged ones.
set_head <- function(head, name) {
    if (nrow(head) != 1L) {
        stop(
            sprintf(
                "unknown catalogue '%s'; packaged: %s",
                name,
                paste(set_names(), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    head
}

catalogue_set <- function(name) {
    bundle <- current_bundle()
    bundle$memo(paste0("set:", name), function() CatalogueSet$new(bundle, name))
}

set_names <- function() {
    catalogue_rows(current_bundle(), "sets")$set
}

# One catalogue table of the bundle, as a data frame of strings with empty cells
# as "", read once; its columns must be those datapackage.json declares.
catalogue_rows <- function(bundle, table) {
    bundle$memo(paste0("table:", table), function() {
        found <- read.csv(
            file.path(bundle$dir, "catalogues", paste0(table, ".csv")),
            colClasses = "character",
            na.strings = character(0),
            check.names = FALSE,
            encoding = "UTF-8"
        )
        resources <- bundle$json("catalogues", "datapackage.json")$resources
        resource <- Filter(
            function(found) identical(found$name, table),
            resources
        )[[1L]]
        declared <- vapply(
            resource$schema$fields,
            function(field) field$name,
            character(1)
        )
        if (!identical(names(found), declared)) {
            stop(
                sprintf(
                    "%s.csv has the columns %s, not %s",
                    table,
                    paste(names(found), collapse = ", "),
                    paste(declared, collapse = ", ")
                ),
                call. = FALSE
            )
        }
        found
    })
}
