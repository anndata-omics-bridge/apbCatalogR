levels_of <- function() {
  result <- synthetic_result("diann_v2-peptidoform_ion")
  list(
    result = result$levels$peptidoform_ion,
    level = DataCatalog$new(result, "abundance", "confidence")$levels$peptidoform_ion
  )
}

test_that("a level answers a key with the matrix the AnnData holds", {
  found <- levels_of()
  expect_identical(found$level$layers[["q_value"]], found$result$layers[["Q_Value"]])
  expect_identical(found$level$layers[["normalized_ms1_intensity"]], found$level$X)
  expect_identical(found$level$X, found$result$X)
})

test_that("a name that is no key reads the level's own field, else an error", {
  found <- levels_of()
  expect_identical(found$level$layers[["Q_Value"]], found$result$layers[["Q_Value"]])
  expect_identical(found$level$var[["peptidoform_ion"]], found$result$var[["ProForma_ion"]])
  expect_error(found$level$layers[["nonsense"]], "no key or field 'nonsense'")
})

test_that("keys lists the catalog's keys only", {
  found <- levels_of()
  keys <- found$level$layers$keys()
  expect_true(all(c("intensity", "normalized_intensity", "q_value") %in% keys))
  expect_false("Q_Value" %in% keys)
  expect_true("peptidoform_ion" %in% found$level$var$keys())
})

test_that("apb is always in, and the names are the AnnData's", {
  found <- levels_of()
  expect_identical(found$level$obs_names, found$result$obs_names)
  expect_identical(found$level$var_names, found$result$var_names)
  expect_false(is.null(DataCatalog$new(synthetic_result("diann_v2-peptidoform_ion"), "confidence")$var(
    "peptidoform_ion",
    "peptidoform_ion"
  )))
})
