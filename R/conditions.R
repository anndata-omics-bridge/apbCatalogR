# The conditions apb-catalog raises as exceptions in Python.

# The key asked for, its kind and the qualifiers the request accepts, for
# messages.
wanted_text <- function(request) {
    accepted <- paste(
        c(
            if (!is.null(request$kind)) paste0("kind=", request$kind),
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
            )
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

# One missing field: its requirement, what was asked, on which level, and why
# it is absent.
missing_required_text <- function(resolution, row) {
    sprintf(
        "%s field %s on %s is missing: %s",
        row$requirement,
        wanted_text(resolution$request),
        row$level,
        paste(resolution$reasons, collapse = "; ")
    )
}

# Fields the named sets require are missing, every one listed, one line each:
# `missing` holds each field's resolution and requirement row.
missing_required_lines <- function(missing) {
    paste(
        vapply(
            missing,
            function(found) missing_required_text(found$resolution, found$row),
            character(1)
        ),
        collapse = "\n"
    )
}

missing_required_field <- function(missing) {
    errorCondition(
        missing_required_lines(missing),
        missing = missing,
        class = "MissingRequiredField",
        call = NULL
    )
}

missing_required_warning <- function(missing) {
    warningCondition(
        missing_required_lines(missing),
        missing = missing,
        class = "MissingRequiredField",
        call = NULL
    )
}
