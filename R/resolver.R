# Answer a request for a meaning from reviewed entries, without choosing silently; apb_catalog.resolver.

# A consumer's question: one level and location, constrained by accepted qualifier values (a named list of character
# vectors). Qualifiers left out accept any value; accepting "unknown" admits entries that leave it unknown.
field_request <- function(level, location, qualifiers) {
  list(level = level, location = location, qualifiers = qualifiers)
}

request_admits <- function(request, entry) {
  selects <- is.na(request$location) || entry$reference$location %in% READS[[request$location]]
  selects && length(request_mismatched(request, entry$qualifiers)) == 0L
}

# The requested qualifiers an entry's known values contradict.
request_mismatched <- function(request, qualifiers) {
  Filter(
    function(name) {
      qualifiers[[name]] != UNKNOWN && !(qualifiers[[name]] %in% request$qualifiers[[name]])
    },
    names(request$qualifiers)
  )
}

# The requested qualifiers an entry leaves unknown.
request_undecided <- function(request, qualifiers) {
  Filter(
    function(name) {
      qualifiers[[name]] == UNKNOWN && !(UNKNOWN %in% request$qualifiers[[name]])
    },
    names(request$qualifiers)
  )
}

new_candidate <- function(entry, undecided = character(0)) {
  list(entry_id = entry$entry_id, reference = entry$reference, qualifiers = entry$qualifiers, undecided = undecided)
}

# The answer to one request; only a resolved one carries a reference. `requirement` records the requirement a missing
# field had when the lookup enforced it.
Resolution <- R6::R6Class(
  "Resolution",
  public = list(
    request = NULL,
    status = NULL,
    reference = NULL,
    candidates = NULL,
    reasons = NULL,
    requirement = NA_character_,

    initialize = function(request, status, reference = NULL, candidates = list(), reasons = character(0)) {
      self$request <- request
      self$status <- status
      self$reference <- reference
      self$candidates <- candidates
      self$reasons <- reasons
    },

    # The distinct values the candidates carry; an unreviewed level signals UnresolvedField.
    offered = function(qualifier) {
      if (self$status == "unknown" && length(self$candidates) == 0L) {
        stop(unresolved_field(self))
      }
      values <- vapply(self$candidates, function(candidate) candidate$qualifiers[[qualifier]], character(1))
      sort_c(unique(values))
    },

    # The reference, NULL when the field is missing; ambiguous and unknown answers signal UnresolvedField.
    found = function() {
      if (self$status %in% c("ambiguous", "unknown")) {
        stop(unresolved_field(self))
      }
      self$reference
    }
  )
)

# Resolve only when exactly one entry matches, none is undecided and none is pending.
resolve_entries <- function(entries, pending, request, missing) {
  compatible <- lapply(Filter(function(entry) request_admits(request, entry), entries), function(entry) {
    new_candidate(entry, request_undecided(request, entry$qualifiers))
  })
  waiting <- Filter(function(found) request_admits(request, found$entry), pending)
  decided <- vapply(compatible, function(candidate) length(candidate$undecided) == 0L, logical(1))
  matches <- compatible[decided]
  undecided <- compatible[!decided]
  candidates <- c(compatible, lapply(waiting, function(found) new_candidate(found$entry)))
  if (length(matches) > 1L) {
    names <- vapply(matches, function(candidate) candidate$reference$name, character(1))
    reason <- sprintf("%d entries match: %s", length(matches), paste(names, collapse = ", "))
    return(Resolution$new(request, "ambiguous", candidates = candidates, reasons = reason))
  }
  if (length(waiting) > 0L) {
    reasons <- unique(vapply(waiting, function(found) found$reason, character(1)))
    return(Resolution$new(request, "unknown", candidates = candidates, reasons = reasons))
  }
  if (length(undecided) > 0L) {
    reasons <- vapply(
      undecided,
      function(candidate) {
        sprintf("%s leaves %s unknown", candidate$reference$name, paste(candidate$undecided, collapse = ", "))
      },
      character(1)
    )
    return(Resolution$new(request, "unknown", candidates = candidates, reasons = reasons))
  }
  if (length(matches) == 1L) {
    return(Resolution$new(request, "resolved", reference = matches[[1L]]$reference, candidates = candidates))
  }
  Resolution$new(request, "missing", reasons = missing)
}

# How a level is bound to its set. Each view answers resolve(request) and carries its level, entries, software version
# pattern and rule_name (NULL unless reviewed).

# A level with a reviewed rule; `entries` are those whose fields the level holds. `pending` entries belong to producers
# that have not run; a request they would answer is unknown until the producer runs.
ReviewedLevel <- R6::R6Class(
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
      missing <- sprintf("no reviewed %s entry on %s matches", self$rule$variant$rule, self$level)
      resolve_entries(self$entries, self$pending, request, missing)
    }
  ),
  active = list(
    rule_name = function() self$rule$variant$rule,
    software_version_pattern = function() self$rule$variant$software_version_pattern
  )
)

# A level of a set without vendor catalogues, answered by APB's own fields alone.
ProducedLevel <- R6::R6Class(
  "ProducedLevel",
  public = list(
    level = NULL,
    software_name = NULL,
    software_version_pattern = NULL,
    entries = NULL,
    pending = NULL,
    rule_name = NULL,

    initialize = function(level, software, pattern, entries, pending = list()) {
      self$level <- level
      self$software_name <- software
      self$software_version_pattern <- pattern
      self$entries <- entries
      self$pending <- pending
    },

    resolve = function(request) {
      resolve_entries(self$entries, self$pending, request, sprintf("no entry on %s matches", self$level))
    }
  )
)

# A level whose meanings cannot be established, so every answer is unknown.
UnreviewedLevel <- R6::R6Class(
  "UnreviewedLevel",
  public = list(
    level = NULL,
    software_name = NULL,
    software_version_pattern = NULL,
    reason = NULL,
    entries = list(),
    rule_name = NULL,

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
AbsentLevel <- R6::R6Class(
  "AbsentLevel",
  public = list(
    level = NULL,

    initialize = function(level) {
      self$level <- level
    },

    resolve = function(request) {
      Resolution$new(request, "missing", reasons = sprintf("the result has no %s level", self$level))
    }
  )
)
