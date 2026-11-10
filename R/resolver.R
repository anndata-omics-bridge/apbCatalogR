# Answer a request for a meaning from reviewed entries, without choosing
# silently; apb_catalog.resolver.

# A consumer's question: one level and location, the key asked for (NULL to ask
# for every key), the kind it stands for (NULL with no key) and the qualifier
# values accepted (a named list of character vectors): the values the key fixes
# and any further qualifiers. A request for a key answers with that key's
# entries; an entry of another key only keeps it undecided. Qualifiers left out
# accept any value; an entry that leaves a qualifier unknown is undecided unless
# the request accepts "unknown" there.
field_request <- function(level, location, key, qualifiers, kind = NULL) {
    list(
        level = level,
        location = location,
        key = key,
        kind = kind,
        qualifiers = qualifiers
    )
}

# For each entry, what the request asks that its known qualifier values
# contradict (`mismatched`), and the qualifiers asked that it leaves unknown
# (`undecided`, a list column). A kind is always known; a field whose meaning
# the reviewer could not establish is not catalogued.
qualifier_checks <- function(entries, request) {
    names <- names(request$qualifiers)
    if (length(names) == 0L || nrow(entries) == 0L) {
        return(tibble(
            mismatched = rep(FALSE, nrow(entries)),
            undecided = rep(list(character(0)), nrow(entries))
        ))
    }
    accepted <- tibble(
        qualifier = rep(names, lengths(request$qualifiers)),
        value = unlist(request$qualifiers, use.names = FALSE)
    )
    open <- accepted$qualifier[accepted$value == UNKNOWN]
    entries |>
        mutate(.row = seq_len(nrow(entries))) |>
        select(".row", all_of(sprintf("qualifier.%s", names))) |>
        pivot_longer(
            -".row",
            names_to = "qualifier",
            names_prefix = "qualifier[.]",
            values_to = "value"
        ) |>
        mutate(
            known = !is.na(.data$value) & .data$value != UNKNOWN,
            unknown = .data$value %in% UNKNOWN,
            accepts = paste(.data$qualifier, .data$value) %in%
                paste(accepted$qualifier, accepted$value)
        ) |>
        summarise(
            .by = ".row",
            mismatched = any(.data$known & !.data$accepts),
            undecided = list(.data$qualifier[
                .data$unknown & !(.data$qualifier %in% open)
            ])
        ) |>
        select("mismatched", "undecided")
}

# The entries that answer the request apart from qualifiers they leave unknown,
# each with what it leaves `undecided`: an entry of the key asked for, or of
# another key when it leaves unknown what the request asks.
admitted_entries <- function(entries, request) {
    checks <- qualifier_checks(entries, request)
    entries$undecided <- checks$undecided
    selects <- if (is.na(request$location)) {
        rep(TRUE, nrow(entries))
    } else {
        entries$location %in% READS[[request$location]]
    }
    kind <- if (is.null(request$kind)) {
        rep(FALSE, nrow(entries))
    } else {
        entries$kind != request$kind
    }
    keyed <- is.null(request$key) |
        entries$key %in% request$key |
        lengths(entries$undecided) > 0L
    entries[selects & !kind & !checks$mismatched & keyed, ]
}

# The references of entries, as lists of level, location, name, table and
# prefix.
entry_references <- function(entries) {
    lapply(seq_len(nrow(entries)), function(index) {
        list(
            level = entries$level[[index]],
            location = entries$location[[index]],
            name = entries$name[[index]],
            table = entries$table[[index]],
            prefix = entries$prefix[[index]]
        )
    })
}

# The answer to one request; only a resolved one carries a reference.
# `candidates` are the entries that may answer, with what each leaves
# `undecided`. `requirement` records the requirement a missing field had when
# the catalog enforced it at init.
Resolution <- R6Class(
    "Resolution",
    public = list(
        request = NULL,
        status = NULL,
        reference = NULL,
        candidates = NULL,
        reasons = NULL,
        requirement = NA_character_,

        initialize = function(
            request,
            status,
            reference = NULL,
            candidates = NULL,
            reasons = character(0)
        ) {
            self$request <- request
            self$status <- status
            self$reference <- reference
            self$candidates <- candidates
            self$reasons <- reasons
        },

        # The distinct keys the candidates have; an unreviewed level signals
        # UnresolvedField.
        offered = function() {
            keys <- as.character(self$candidates$key)
            if (self$status == "unknown" && length(keys) == 0L) {
                stop(unresolved_field(self))
            }
            sort_c(unique(keys))
        },

        # The reference, NULL when the field is missing; ambiguous and unknown
        # answers signal UnresolvedField.
        found = function() {
            if (self$status %in% c("ambiguous", "unknown")) {
                stop(unresolved_field(self))
            }
            self$reference
        }
    )
)

# Resolve only when exactly one entry matches, none is undecided and none is
# pending. `pending` entries carry the `reason` they wait.
resolve_entries <- function(entries, pending, request, missing) {
    compatible <- admitted_entries(entries, request)
    waiting <- admitted_entries(pending, request)
    waiting$undecided <- rep(list(character(0)), nrow(waiting))
    decided <- lengths(compatible$undecided) == 0L
    matches <- compatible[decided, ]
    candidates <- bind_rows(compatible, waiting)
    unresolved <- function(status, reasons) {
        Resolution$new(
            request,
            status,
            candidates = candidates,
            reasons = reasons
        )
    }
    if (nrow(matches) > 1L) {
        return(unresolved("ambiguous", ambiguous_reason(matches)))
    }
    if (nrow(waiting) > 0L) {
        return(unresolved("unknown", unique(waiting$reason)))
    }
    if (!all(decided)) {
        return(unresolved("unknown", undecided_reasons(compatible[!decided, ])))
    }
    if (nrow(matches) == 1L) {
        return(Resolution$new(
            request,
            "resolved",
            reference = entry_references(matches)[[1L]],
            candidates = candidates
        ))
    }
    Resolution$new(request, "missing", reasons = missing)
}

# Why several entries match: their names.
ambiguous_reason <- function(matches) {
    sprintf(
        "%d entries match: %s",
        nrow(matches),
        paste(matches$name, collapse = ", ")
    )
}

# Why entries leave a request unknown: the qualifiers each leaves open.
undecided_reasons <- function(undecided) {
    sprintf(
        "%s leaves %s unknown",
        undecided$name,
        vapply(undecided$undecided, paste, character(1), collapse = ", ")
    )
}

# How a level is bound to its set. Each view answers resolve(request) and
# carries its level, entries and software version pattern.

# A level with a reviewed rule (`rule`: software, pattern and level); `entries`
# are those whose fields the level holds. `pending` entries belong to producers
# that have not run; a request they would answer is unknown until the producer
# runs.
ReviewedLevel <- R6Class(
    "ReviewedLevel",
    public = list(
        level = NULL,
        rule = NULL,
        entries = NULL,
        pending = NULL,

        initialize = function(level, rule, entries, pending) {
            self$level <- level
            self$rule <- rule
            self$entries <- entries
            self$pending <- pending
        },

        resolve = function(request) {
            missing <- sprintf(
                "no reviewed %s %s entry on %s matches",
                self$rule$software,
                self$rule$pattern,
                self$level
            )
            resolve_entries(self$entries, self$pending, request, missing)
        }
    ),
    active = list(
        software_version_pattern = function() self$rule$pattern
    )
)

# A level of a set without vendor catalogues, answered by APB's own fields
# alone.
ProducedLevel <- R6Class(
    "ProducedLevel",
    public = list(
        level = NULL,
        software_name = NULL,
        software_version_pattern = NULL,
        entries = NULL,
        pending = NULL,

        initialize = function(level, software, pattern, entries, pending) {
            self$level <- level
            self$software_name <- software
            self$software_version_pattern <- pattern
            self$entries <- entries
            self$pending <- pending
        },

        resolve = function(request) {
            resolve_entries(
                self$entries,
                self$pending,
                request,
                sprintf("no entry on %s matches", self$level)
            )
        }
    )
)

# A level whose meanings cannot be established, so every answer is unknown.
UnreviewedLevel <- R6Class(
    "UnreviewedLevel",
    public = list(
        level = NULL,
        software_name = NULL,
        software_version_pattern = NULL,
        reason = NULL,
        entries = NULL,

        initialize = function(level, software, pattern, reason) {
            self$level <- level
            self$software_name <- software
            self$software_version_pattern <- pattern
            self$reason <- reason
        },

        resolve = function(request) {
            Resolution$new(request, "unknown", reasons = self$reason)
        }
    )
)

# A requested level the result does not contain.
AbsentLevel <- R6Class(
    "AbsentLevel",
    public = list(
        level = NULL,

        initialize = function(level) {
            self$level <- level
        },

        resolve = function(request) {
            Resolution$new(
                request,
                "missing",
                reasons = sprintf("the result has no %s level", self$level)
            )
        }
    )
)
