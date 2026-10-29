# Every synthetic result reads into anndataR levels laid out as apb2's h5ad writer lays them out.

test_that("every synthetic result reads with its levels, layers and stored rule", {
  for (path in list.files(results_dir(), pattern = "\\.parquet$", full.names = TRUE)) {
    manifest <- jsonlite::fromJSON(file.path(path, "manifest.json"), simplifyVector = FALSE)
    result <- read_parsed_levels(path)
    expect_s3_class(result, "ParsedLevels")
    expect_identical(names(result$levels), unlist(manifest$level_order), label = basename(path))
    for (name in names(result$levels)) {
      level <- result$levels[[name]]
      entry <- manifest$levels[[name]]
      expect_s3_class(level, "AbstractAnnData")
      expect_identical(colnames(level$var), unlist(entry$var$columns), label = paste(basename(path), name))
      expect_identical(colnames(level$obs), unlist(entry$obs$columns), label = paste(basename(path), name))
      expect_setequal(c(entry$primary_layer, names(level$layers)), unlist(entry$layer_order))
      expect_identical(dim(level$X), c(level$n_obs(), level$n_vars()))
      expect_identical(level$uns$apb$storage$primary_layer, entry$primary_layer)
      expect_identical(level$uns$apb$parse$provenance$rule_json, entry$apb$parse$provenance$rule_json)
    }
  }
})

test_that("a layer is the transposed features-by-observations table", {
  result <- read_parsed_levels(result_path("edge-two-levels"))
  protein <- result$levels$protein
  stored <- arrow::read_parquet(file.path(
    result_path("edge-two-levels"),
    "levels",
    "protein",
    "layers",
    "PG_Q_Value.parquet"
  ))
  expect_equal(unname(protein$layers$PG_Q_Value), unname(t(as.matrix(stored))))
  expect_identical(rownames(protein$layers$PG_Q_Value), rownames(protein$obs))
  expect_identical(colnames(protein$layers$PG_Q_Value), rownames(protein$var))
})

test_that("root records split into the parse record and the post-parse metadata", {
  result <- read_parsed_levels(result_path("edge-sdrf"))
  expect_identical(names(result$metadata), "sdrf")
  expect_null(result$hierarchy)
})

test_that("a directory that is no APB result is refused", {
  expect_error(read_parsed_levels(tempdir()), "not an APB Parquet result")
})
