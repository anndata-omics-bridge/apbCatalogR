# The test bundle's expected answers, computed by apb-catalog in Python, and ways to compare R's answers with them.

bundle_file <- function(...) {
  system.file("extdata", "bundle", ..., package = "apbCatalogR", mustWork = TRUE)
}

test_bundle_file <- function(...) {
  system.file("extdata", "test-bundle", ..., package = "apbCatalogR", mustWork = TRUE)
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
    jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null", digits = NA),
    simplifyVector = FALSE
  )
}

expect_json_equal <- function(actual, expected, label) {
  same <- apbCatalogR:::json_equal(as_json(actual), expected)
  if (!same) {
    message(
      label,
      "\nactual:   ",
      jsonlite::toJSON(actual, auto_unbox = TRUE, null = "null", na = "null"),
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

# What one lookup gives: the answer, or the class of the condition it signals.
outcome_of <- function(lookup) {
  warned <- FALSE
  answer <- tryCatch(
    withCallingHandlers(
      list(value = lookup()),
      MissingRequiredField = function(condition) {
        if (inherits(condition, "warning")) {
          warned <<- TRUE
          invokeRestart("muffleWarning")
        }
      }
    ),
    UnresolvedField = function(condition) list(error = "UnresolvedField", status = condition$resolution$status),
    StaleField = function(condition) list(error = "StaleField"),
    MissingRequiredField = function(condition) list(error = "MissingRequiredField")
  )
  if (!is.null(answer$error)) {
    return(answer)
  }
  c(answer, if (warned) list(warning = "MissingRequiredField"))
}

lookup_arguments <- function(request) {
  qualifiers <- request$qualifiers
  c(list(request$level, kind = qualifiers$kind), qualifiers[names(qualifiers) != "kind"])
}

method_for <- function(location) {
  switch(location, layers = "layer", obs = "obs", var = "var")
}

kinds_outcome <- function(catalog, case) {
  tryCatch(
    list(kinds = as.list(catalog$kinds(case$level, case$location))),
    UnresolvedField = function(condition) list(error = "UnresolvedField", status = condition$resolution$status)
  )
}
