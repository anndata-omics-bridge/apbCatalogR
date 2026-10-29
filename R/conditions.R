# The conditions apb-catalog raises as exceptions in Python.

wanted_text <- function(qualifiers) {
  paste(
    vapply(
      names(qualifiers),
      function(name) {
        paste0(name, "=", paste(qualifiers[[name]], collapse = "|"))
      },
      character(1)
    ),
    collapse = ", "
  )
}

unresolved_field <- function(resolution) {
  request <- resolution$request
  names <- vapply(resolution$candidates, function(candidate) candidate$reference$name, character(1))
  message <- sprintf(
    "%s on %s is %s: %s%s",
    wanted_text(request$qualifiers),
    request$level,
    resolution$status,
    paste(resolution$reasons, collapse = "; "),
    if (length(names) > 0L) paste0("; candidates: ", paste(names, collapse = ", ")) else ""
  )
  errorCondition(message, resolution = resolution, class = "UnresolvedField", call = NULL)
}

missing_required_text <- function(resolution, row) {
  sprintf(
    "%s field %s on %s is missing: %s",
    row$requirement,
    wanted_text(resolution$request$qualifiers),
    row$level,
    paste(resolution$reasons, collapse = "; ")
  )
}

missing_required_field <- function(resolution, row) {
  errorCondition(
    missing_required_text(resolution, row),
    resolution = resolution,
    row = row,
    class = "MissingRequiredField",
    call = NULL
  )
}

missing_required_warning <- function(resolution, row) {
  warningCondition(
    missing_required_text(resolution, row),
    resolution = resolution,
    row = row,
    class = "MissingRequiredField",
    call = NULL
  )
}

stale_field <- function(reference, stored, current) {
  message <- sprintf(
    "%s: this result reads %s, the catalog's rule %s; reconvert the result, or run update_catalog() if it is newer",
    reference$name,
    stored,
    current
  )
  errorCondition(message, reference = reference, class = "StaleField", call = NULL)
}
