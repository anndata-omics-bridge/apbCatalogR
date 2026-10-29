# Paths the packaged catalogue sets do not reach: no packaged producer waits for a run.

producer <- list(
  type = "producer",
  label = "producer:apb-fasta",
  producer = "apb-fasta",
  requires = c("fasta", "provenance"),
  instruction = "run apb-fasta verify",
  entries = list()
)

test_that("a producer has run when the metadata records its path", {
  expect_true(apbCatalogR:::producer_ran(producer, list(fasta = list(provenance = list()))))
  expect_false(apbCatalogR:::producer_ran(producer, list(fasta = list())))
  expect_false(apbCatalogR:::producer_ran(producer, list(fasta = "ran")))
  expect_false(apbCatalogR:::producer_ran(producer, list()))
})

test_that("a request a pending producer would answer is unknown, with what to run", {
  entry <- list(
    entry_id = "fasta-contaminant",
    rules = character(0),
    reference = list(
      level = "protein",
      location = "varm",
      name = "is_contaminant",
      table = "fasta",
      role = NA_character_,
      prefix = NA_character_
    ),
    qualifiers = c(kind = "contaminant")
  )
  pending <- list(list(entry = entry, reason = apbCatalogR:::pending_reason(producer)))
  request <- apbCatalogR:::field_request("protein", "var", list(kind = "contaminant"))
  resolution <- apbCatalogR:::resolve_entries(list(), pending, request, "missing")
  expect_identical(resolution$status, "unknown")
  expect_identical(resolution$reasons, "apb-fasta has not run on this result; run apb-fasta verify")
  expect_error(resolution$found(), class = "UnresolvedField")
})
