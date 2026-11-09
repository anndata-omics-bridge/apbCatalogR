# Every answer apb-catalog gives in Python on the bundle, R gives too.

test_that("Catalog answers every bundle case as apb-catalog does", {
    for (case in read_cases("catalog.json")) {
        label <- paste(case$catalogue, case$software, case$version)
        if (!is.null(case$error)) {
            expect_error(
                Catalog$new(case$software, case$version, case$catalogue),
                label = label
            )
            next
        }
        catalog <- Catalog$new(case$software, case$version, case$catalogue)
        for (request in case$requests) {
            slot <- slot_of(catalog, request)
            outcome <- outcome_of(function() {
                do.call(slot$get, slot_arguments(request))
            })
            if (is.null(outcome$error)) {
                outcome <- list(name = outcome$value)
            }
            expect_json_equal(
                outcome,
                request$outcome,
                paste(
                    label,
                    request$level,
                    jsonlite::toJSON(request$qualifiers)
                )
            )
        }
        for (keys in case$keys) {
            expect_json_equal(
                keys_outcome(catalog, keys),
                keys$outcome,
                paste(label, "keys", keys$level, keys$location)
            )
        }
        expect_json_equal(
            frame_records(catalog$fields()),
            case$fields,
            paste(label, "fields")
        )
    }
})

# The data a field's name names on a result, as a level hands it back; `table/column` is a varm column.
named_data <- function(result, request, name) {
    if (is.null(name)) {
        return(NULL)
    }
    level <- result$levels[[request$level]]
    if (request$location == "layers") {
        return(apbCatalogR:::level_layer(level, name))
    }
    if (request$location == "obs") {
        return(level$obs[[name]])
    }
    parts <- strsplit(name, "/", fixed = TRUE)[[1L]]
    if (length(parts) == 2L) {
        level$varm[[parts[[1L]]]][[parts[[2L]]]]
    } else {
        level$var[[name]]
    }
}

test_that("DataCatalog answers every bundle case as apb-catalog does", {
    for (case in read_cases("data_catalog.json")) {
        label <- paste(case$result, case$catalogue, case$enforce %||% "")
        result <- synthetic_result(case$result)
        found <- opened_of(function() {
            DataCatalog$new(result, case$catalogue, enforce = case$enforce)
        })
        expect_json_equal(
            found$opened,
            recorded_opening(case),
            paste(label, "opening")
        )
        catalog <- found$catalog
        if (is.null(catalog)) {
            next
        }
        for (request in case$requests) {
            slot <- slot_of(catalog, request)
            outcome <- outcome_of(function() {
                do.call(slot$get, slot_arguments(request))
            })
            request_label <- paste(
                label,
                request$level,
                jsonlite::toJSON(request$qualifiers)
            )
            expected <- request$outcome
            if (!is.null(expected$error)) {
                expect_json_equal(outcome, expected, request_label)
                next
            }
            expect_identical(
                outcome$value,
                named_data(result, request, expected$name),
                label = request_label
            )
            if (!is.null(expected$name)) {
                expect_identical(
                    do.call(slot$name, slot_arguments(request)),
                    expected$name,
                    label = request_label
                )
            }
        }
        for (keys in case$keys) {
            expect_json_equal(
                keys_outcome(catalog, keys),
                keys$outcome,
                paste(label, "keys", keys$level, keys$location)
            )
        }
        if (!is.null(case$fields)) {
            expect_json_equal(
                frame_records(catalog$fields()),
                case$fields,
                paste(label, "fields")
            )
        }
    }
})

test_that("the catalogue tables match apb-catalog's", {
    tables <- read_cases("tables.json")
    expect_json_equal(
        frame_records(catalogues()),
        tables$catalogues,
        "catalogues"
    )
    for (name in names(tables$rule_coverage)) {
        expect_json_equal(
            frame_records(rule_coverage(name)),
            tables$rule_coverage[[name]],
            paste("rule_coverage", name)
        )
    }
    expect_json_equal(
        frame_records(catalogue_table()),
        tables$catalogue_table$all,
        "catalogue_table"
    )
    for (name in setdiff(names(tables$catalogue_table), "all")) {
        expect_json_equal(
            frame_records(catalogue_table(name)),
            tables$catalogue_table[[name]],
            paste("catalogue_table", name)
        )
    }
})
