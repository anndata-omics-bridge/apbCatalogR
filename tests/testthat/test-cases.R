# Every answer apb-catalog gives in Python on the bundle, R gives too.

test_that("Catalog answers every bundle case as apb-catalog does", {
  for (case in read_cases("catalog.json")) {
    label <- paste(case$catalogue, case$software, case$version)
    if (!is.null(case$error)) {
      expect_error(Catalog$new(case$software, case$version, case$catalogue), label = label)
      next
    }
    catalog <- Catalog$new(case$software, case$version, case$catalogue)
    for (request in case$requests) {
      method <- catalog[[method_for(request$location)]]
      outcome <- outcome_of(function() do.call(method, lookup_arguments(request)))
      if (is.null(outcome$error)) {
        found <- outcome$value
        outcome$value <- NULL
        reference <- if (!is.null(found)) {
          list(level = found$level, location = found$location, table = found$table, name = found$name)
        }
        outcome <- c(list(reference = reference), outcome)
      }
      expect_json_equal(outcome, request$outcome, paste(label, request$level, jsonlite::toJSON(request$qualifiers)))
    }
    for (kinds in case$kinds) {
      expect_json_equal(
        kinds_outcome(catalog, kinds),
        kinds$outcome,
        paste(label, "kinds", kinds$level, kinds$location)
      )
    }
    expect_json_equal(frame_records(catalog$fields()), case$fields, paste(label, "fields"))
  }
})

# The data a reference names on a result, as DataCatalog hands it back.
referenced_data <- function(result, reference) {
  if (is.null(reference)) {
    return(NULL)
  }
  level <- result$levels[[reference$level]]
  switch(
    reference$location,
    layers = apbCatalogR:::level_layer(level, reference$name),
    obs = level$obs[[reference$name]],
    var = level$var[[reference$name]],
    varm = level$varm[[reference$table]][[reference$name]]
  )
}

test_that("DataCatalog answers every bundle case as apb-catalog does", {
  for (case in read_cases("data_catalog.json")) {
    label <- paste(case$result, case$catalogue, case$enforce %||% "")
    result <- synthetic_result(case$result)
    if (!is.null(case$error)) {
      expect_error(DataCatalog$new(result, case$catalogue, case$enforce), label = label)
      next
    }
    catalog <- DataCatalog$new(result, case$catalogue, case$enforce)
    for (request in case$requests) {
      method <- catalog[[method_for(request$location)]]
      outcome <- outcome_of(function() do.call(method, lookup_arguments(request)))
      request_label <- paste(label, request$level, jsonlite::toJSON(request$qualifiers))
      expected <- request$outcome
      if (!is.null(expected$error)) {
        expect_json_equal(outcome, expected, request_label)
        next
      }
      expect_identical(outcome$value, referenced_data(result, expected$reference), label = request_label)
      expect_identical(outcome$warning, expected$warning, label = request_label)
    }
    for (kinds in case$kinds) {
      expect_json_equal(
        kinds_outcome(catalog, kinds),
        kinds$outcome,
        paste(label, "kinds", kinds$level, kinds$location)
      )
    }
    if (!is.null(case$fields)) {
      expect_json_equal(frame_records(catalog$fields()), case$fields, paste(label, "fields"))
    }
  }
})

test_that("the catalogue tables match apb-catalog's", {
  tables <- read_cases("tables.json")
  expect_json_equal(frame_records(catalogues()), tables$catalogues, "catalogues")
  for (name in names(tables$rule_coverage)) {
    expect_json_equal(frame_records(rule_coverage(name)), tables$rule_coverage[[name]], paste("rule_coverage", name))
  }
  expect_json_equal(frame_records(catalogue_table()), tables$catalogue_table$all, "catalogue_table")
  for (name in setdiff(names(tables$catalogue_table), "all")) {
    expect_json_equal(
      frame_records(catalogue_table(name)),
      tables$catalogue_table[[name]],
      paste("catalogue_table", name)
    )
  }
})
