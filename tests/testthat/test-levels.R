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

test_that("several names read every column a key names, as a data frame", {
  result <- synthetic_result("diann_v2-peptidoform_ion")
  level <- result$levels$peptidoform_ion
  obs <- level$obs
  obs$group_x <- rep_len(c("A", "B"), nrow(obs))
  obs$bait <- rep_len(c("b1", "b2"), nrow(obs))
  obs$subject <- rep_len(c("s1", "s2"), nrow(obs))
  level$obs <- obs
  result$metadata$prolfquapp <- list(schema_version = "4")
  ion <- DataCatalog$new(result, "apb")$levels$peptidoform_ion
  expect_error(ion$obs[["group"]], "ambiguous")
  design <- ion$obs[c("group", "subject", "Run")]
  expect_identical(colnames(design), c("group_x", "bait", "subject", "Run"))
  expect_identical(design$Run, obs$Run)
  expect_error(ion$obs[c("group", "nonsense")], "no key or field 'nonsense'")
})

test_that("a Catalog's levels hold names", {
  ion <- Catalog$new("DIA-NN", "2.2.0", "abundance")$levels$peptidoform_ion
  expect_identical(ion$layers[["intensity"]], "Precursor_Quantity")
  expect_true("normalized_intensity" %in% ion$layers$keys())
  expect_error(ion$layers[["Precursor_Quantity"]], "no key or field 'Precursor_Quantity'")
  apb <- Catalog$new("DIA-NN", "2.2.0", "apb")$levels$peptidoform_ion
  expect_identical(apb$var[c("peptidoform_ion", "protein_assignment")], c("ProForma_ion", "Protein_Group"))
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
