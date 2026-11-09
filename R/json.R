# JSON values as jsonlite reads them without simplification: objects are named
# lists, arrays unnamed lists.

read_json_file <- function(path) {
    fromJSON(path, simplifyVector = FALSE)
}

`%||%` <- function(value, default) {
    if (is.null(value)) default else value
}

# Python's sorted(): code-point order, independent of the locale.
sort_c <- function(values) {
    sort(values, method = "radix")
}

# Equality of two JSON values, ignoring the order of object members, as Python's
# dict equality does.
json_equal <- function(left, right) {
    if (!is.list(left) || !is.list(right)) {
        return(identical(left, right))
    }
    if (
        length(left) != length(right) ||
            is.null(names(left)) != is.null(names(right))
    ) {
        return(FALSE)
    }
    if (is.null(names(left))) {
        return(all(vapply(
            seq_along(left),
            function(index) json_equal(left[[index]], right[[index]]),
            logical(1)
        )))
    }
    setequal(names(left), names(right)) &&
        all(vapply(
            names(left),
            function(name) json_equal(left[[name]], right[[name]]),
            logical(1)
        ))
}

# One JSON value as Python's json.dumps writes it, for messages and constants.
json_text <- function(value) {
    if (is.null(value)) {
        return("null")
    }
    if (
        is.double(value) &&
            length(value) == 1L &&
            is.finite(value) &&
            value == trunc(value)
    ) {
        return(format(value, nsmall = 1L, scientific = FALSE))
    }
    as.character(toJSON(
        value,
        auto_unbox = TRUE,
        digits = NA,
        null = "null"
    ))
}
