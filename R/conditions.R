# The conditions apb-catalog raises as exceptions in Python.

# The key asked for and the qualifiers the request accepts, for messages.
wanted_text <- function(request) {
    accepted <- paste(
        vapply(
            names(request$qualifiers),
            function(name) {
                paste0(
                    name,
                    "=",
                    paste(request$qualifiers[[name]], collapse = "|")
                )
            },
            character(1)
        ),
        collapse = ", "
    )
    if (is.null(request$key)) {
        accepted
    } else {
        sprintf("%s (%s)", request$key, accepted)
    }
}

unresolved_field <- function(resolution) {
    request <- resolution$request
    names <- vapply(
        resolution$candidates,
        function(candidate) candidate$reference$name,
        character(1)
    )
    message <- sprintf(
        "%s on %s is %s: %s%s",
        wanted_text(request),
        request$level,
        resolution$status,
        paste(resolution$reasons, collapse = "; "),
        if (length(names) > 0L) {
            paste0("; candidates: ", paste(names, collapse = ", "))
        } else {
            ""
        }
    )
    errorCondition(
        message,
        resolution = resolution,
        class = "UnresolvedField",
        call = NULL
    )
}

missing_required_text <- function(resolution, row) {
    sprintf(
        "%s field %s on %s is missing: %s",
        row$requirement,
        wanted_text(resolution$request),
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
