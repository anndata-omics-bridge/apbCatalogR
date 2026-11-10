# Paths the packaged catalogue sets do not reach: no packaged producer waits for a run.

requires <- c("fasta", "provenance")

test_that("a producer has run when the metadata records its path", {
    expect_true(apbCatalogR:::producer_ran(
        requires,
        list(fasta = list(provenance = list()))
    ))
    expect_false(apbCatalogR:::producer_ran(requires, list(fasta = list())))
    expect_false(apbCatalogR:::producer_ran(requires, list(fasta = "ran")))
    expect_false(apbCatalogR:::producer_ran(requires, list()))
})

test_that("a request a pending producer would answer is unknown, with what to run", {
    pending <- data.frame(
        entry_id = "fasta-contaminant",
        level = "protein",
        location = "varm",
        name = "is_contaminant",
        table = "fasta",
        prefix = NA_character_,
        key = "contaminant",
        kind = "contaminant",
        reason = apbCatalogR:::pending_reason(
            "apb-fasta",
            "run apb-fasta verify"
        )
    )
    request <- apbCatalogR:::field_request(
        "protein",
        "var",
        "contaminant",
        list(),
        kind = "contaminant"
    )
    resolution <- apbCatalogR:::resolve_entries(
        pending[0L, names(pending) != "reason"],
        pending,
        request,
        "missing"
    )
    expect_identical(resolution$status, "unknown")
    expect_identical(
        resolution$reasons,
        "apb-fasta has not run on this result; run apb-fasta verify"
    )
    expect_error(resolution$found(), class = "UnresolvedField")
})
