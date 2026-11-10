# Answer a request for a meaning from reviewed entries, without choosing
# silently; apb_catalog.resolver.

# A consumer's question: one level and location, the key asked, NULL to ask for
# every key, the kind it stands for, NULL with no key, and the qualifier
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

# Whether an entry answers the request apart from qualifiers it leaves unknown:
# an entry of the key asked for, or of another key when it leaves unknown what
# the request asks.
request_admits <- function(request, entry) {
    selects <- is.na(request$location) ||
        entry$reference$location %in% READS[[request$location]]
    keyed <- is.null(request$key) ||
        identical(entry$key, request$key) ||
        length(request_undecided(request, entry)) > 0L
    selects && length(request_mismatched(request, entry)) == 0L && keyed
}

# What the request asks that an entry's kind or known values contradict.
request_mismatched <- function(request, entry) {
    kind <- !is.null(request$kind) && entry$kind != request$kind
    c(
        if (kind) "kind",
        unlist(Filter(
            function(name) {
                entry$qualifiers[[name]] != UNKNOWN &&
                    !(entry$qualifiers[[name]] %in% request$qualifiers[[name]])
            },
            names(request$qualifiers)
        ))
    )
}

# What the request asks that an entry leaves unknown: its qualifiers with the
# value unknown. A kind is always known; a field whose meaning the reviewer
# could not establish is not catalogued.
request_undecided <- function(request, entry) {
    unlist(Filter(
        function(name) {
            entry$qualifiers[[name]] == UNKNOWN &&
                !(UNKNOWN %in% request$qualifiers[[name]])
        },
        names(request$qualifiers)
    ))
}

new_candidate <- function(entry, undecided = character(0)) {
    list(
        entry_id = entry$entry_id,
        reference = entry$reference,
        key = entry$key,
        kind = entry$kind,
        qualifiers = entry$qualifiers,
        undecided = undecided
    )
}

# The answer to one request; only a resolved one carries a reference.
# `requirement` records the requirement a missing field had when the catalog
# enforced it at init.
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
            candidates = list(),
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
            if (self$status == "unknown" && length(self$candidates) == 0L) {
                stop(unresolved_field(self))
            }
            sort_c(unique(vapply(
                self$candidates,
                function(candidate) candidate$key,
                character(1)
            )))
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
# pending.
resolve_entries <- function(entries, pending, request, missing) {
    compatible <- lapply(
        Filter(function(entry) request_admits(request, entry), entries),
        function(entry) {
            new_candidate(entry, request_undecided(request, entry))
        }
    )
    waiting <- Filter(
        function(found) request_admits(request, found$entry),
        pending
    )
    decided <- vapply(
        compatible,
        function(candidate) length(candidate$undecided) == 0L,
        logical(1)
    )
    matches <- compatible[decided]
    candidates <- c(
        compatible,
        lapply(waiting, function(found) new_candidate(found$entry))
    )
    unresolved <- function(status, reasons) {
        Resolution$new(
            request,
            status,
            candidates = candidates,
            reasons = reasons
        )
    }
    if (length(matches) > 1L) {
        return(unresolved("ambiguous", ambiguous_reason(matches)))
    }
    if (length(waiting) > 0L) {
        return(unresolved("unknown", pending_reasons(waiting)))
    }
    if (!all(decided)) {
        return(unresolved("unknown", undecided_reasons(compatible[!decided])))
    }
    if (length(matches) == 1L) {
        return(Resolution$new(
            request,
            "resolved",
            reference = matches[[1L]]$reference,
            candidates = candidates
        ))
    }
    Resolution$new(request, "missing", reasons = missing)
}

# Why several entries match: their names.
ambiguous_reason <- function(matches) {
    names <- vapply(
        matches,
        function(candidate) candidate$reference$name,
        character(1)
    )
    sprintf(
        "%d entries match: %s",
        length(matches),
        paste(names, collapse = ", ")
    )
}

# Why pending entries leave a request unknown, each reason once.
pending_reasons <- function(waiting) {
    unique(vapply(waiting, function(found) found$reason, character(1)))
}

# Why entries leave a request unknown: the qualifiers each leaves open.
undecided_reasons <- function(undecided) {
    vapply(
        undecided,
        function(candidate) {
            sprintf(
                "%s leaves %s unknown",
                candidate$reference$name,
                paste(candidate$undecided, collapse = ", ")
            )
        },
        character(1)
    )
}

# How a level is bound to its set. Each view answers resolve(request) and
# carries its level, entries and software version pattern.

# A level with a reviewed rule; `entries` are those whose fields the level
# holds. `pending` entries belong to producers that have not run; a request they
# would answer is unknown until the producer runs.
ReviewedLevel <- R6Class(
    "ReviewedLevel",
    public = list(
        level = NULL,
        rule = NULL,
        entries = NULL,
        pending = NULL,

        initialize = function(level, rule, entries, pending = list()) {
            self$level <- level
            self$rule <- rule
            self$entries <- entries
            self$pending <- pending
        },

        resolve = function(request) {
            variant <- self$rule$variant
            missing <- sprintf(
                "no reviewed %s %s entry on %s matches",
                self$rule$catalogue$software_name,
                variant$software_version_pattern,
                self$level
            )
            resolve_entries(self$entries, self$pending, request, missing)
        }
    ),
    active = list(
        software_version_pattern = function() {
            self$rule$variant$software_version_pattern
        }
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

        initialize = function(
            level,
            software,
            pattern,
            entries,
            pending = list()
        ) {
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
        entries = list(),

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
