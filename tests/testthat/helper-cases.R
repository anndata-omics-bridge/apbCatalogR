# The test bundle's expected answers, computed by apb-catalog in Python, and ways to compare R's answers with them.

bundle_file <- function(...) {
    system.file(
        "extdata",
        "bundle",
        ...,
        package = "apbCatalogR",
        mustWork = TRUE
    )
}

test_bundle_file <- function(...) {
    system.file(
        "extdata",
        "test-bundle",
        ...,
        package = "apbCatalogR",
        mustWork = TRUE
    )
}

read_cases <- function(name) {
    jsonlite::fromJSON(test_bundle_file("cases", name), simplifyVector = FALSE)
}

results <- new.env(parent = emptyenv())

# The synthetic results, unpacked once per test run.
results_dir <- function() {
    if (is.null(results$dir)) {
        results$dir <- tempfile("results")
        utils::untar(test_bundle_file("results.tar.gz"), exdir = results$dir)
    }
    results$dir
}

result_path <- function(name) {
    file.path(results_dir(), paste0(name, ".parquet"))
}

synthetic_result <- function(name) {
    if (is.null(results[[name]])) {
        results[[name]] <- read_parsed_levels(result_path(name))
    }
    results[[name]]
}

# A value as JSON would hold it: NA and NULL become null, vectors arrays where `arrays` says so.
as_json <- function(value) {
    jsonlite::fromJSON(
        jsonlite::toJSON(
            value,
            auto_unbox = TRUE,
            null = "null",
            na = "null",
            digits = NA
        ),
        simplifyVector = FALSE
    )
}

expect_json_equal <- function(actual, expected, label) {
    same <- apbCatalogR:::json_equal(as_json(actual), expected)
    if (!same) {
        message(
            label,
            "\nactual:   ",
            jsonlite::toJSON(
                actual,
                auto_unbox = TRUE,
                null = "null",
                na = "null"
            ),
            "\nexpected: ",
            jsonlite::toJSON(expected, auto_unbox = TRUE, null = "null")
        )
    }
    expect_true(same, label = label)
}

# The rows of a data frame as named lists; list columns become arrays.
frame_records <- function(frame) {
    lapply(seq_len(nrow(frame)), function(index) {
        stats::setNames(
            lapply(names(frame), function(column) {
                value <- frame[[column]][[index]]
                if (is.list(frame[[column]])) {
                    if (is.null(value)) NULL else as.list(value)
                } else if (is.na(value)) {
                    NULL
                } else {
                    value
                }
            }),
            names(frame)
        )
    })
}

# What one lookup gives: the answer, or the UnresolvedField it signals.
outcome_of <- function(lookup) {
    tryCatch(
        list(value = lookup()),
        UnresolvedField = function(condition) {
            list(
                error = "UnresolvedField",
                status = condition$resolution$status
            )
        }
    )
}

# What opening a catalog gives: the catalog, or the condition it signals as the
# recording names it, with every missing field as [level, location, key,
# requirement] or the message; and whether it warned. A plain error is Python's
# ValueError.
opened_of <- function(open) {
    opened <- stats::setNames(list(), character(0))
    warned <- FALSE
    catalog <- tryCatch(
        withCallingHandlers(
            open(),
            MissingRequiredField = function(condition) {
                if (inherits(condition, "warning")) {
                    warned <<- TRUE
                    invokeRestart("muffleWarning")
                }
            }
        ),
        error = function(condition) {
            if (inherits(condition, "MissingRequiredField")) {
                opened$error <<- "MissingRequiredField"
                opened$missing <<- lapply(condition$missing, function(found) {
                    row <- found$row
                    list(row$level, row$location, row$key, row$requirement)
                })
            } else if (inherits(condition, "simpleError")) {
                opened$error <<- "ValueError"
                opened$message <<- conditionMessage(condition)
            } else {
                stop(condition)
            }
            NULL
        }
    )
    if (warned) {
        opened$warning <- "MissingRequiredField"
    }
    list(catalog = catalog, opened = opened)
}

# What a case records that opening its catalog signalled or warned.
recorded_opening <- function(case) {
    case[intersect(c("error", "missing", "message", "warning"), names(case))]
}

# The slot one request reads: a level's layers, var or obs.
slot_of <- function(catalog, request) {
    catalog$levels[[request$level]][[request$location]]
}

slot_arguments <- function(request) {
    c(list(request$key), request$qualifiers)
}

keys_outcome <- function(catalog, case) {
    tryCatch(
        list(
            keys = as.list(apbCatalogR:::sort_c(catalog$levels[[case$level]][[
                case$location
            ]]$keys()))
        ),
        UnresolvedField = function(condition) {
            list(
                error = "UnresolvedField",
                status = condition$resolution$status
            )
        }
    )
}
