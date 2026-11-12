# Entry ids, CPE style: <source>/<version>/<level>/<attribute>/<key>/<column>; apb_catalog.rules.entry_id.

test_that("an entry id names where its field lives", {
    layer <- apbCatalogR:::entry_id_fields(
        "AlphaDIA/2.x/peptidoform_ion/layers/Precursor_Intensity/-"
    )
    expect_identical(
        c(layer$source, layer$version, layer$attribute),
        c("AlphaDIA", "2.x", "layers")
    )
    expect_identical(layer$name, "Precursor_Intensity")
    expect_true(is.na(layer$table))
    varm <- apbCatalogR:::entry_id_fields(
        "apb-fasta/*/peptidoform_ion/varm/fasta_validation/fasta_matches_contaminant"
    )
    expect_identical(
        c(varm$table, varm$name),
        c("fasta_validation", "fasta_matches_contaminant")
    )
    prefix <- apbCatalogR:::entry_id_fields(
        "annotation:sdrf/*/peptidoform_ion/obs/-/factor_value_*"
    )
    expect_identical(prefix$name, "factor_value_*")
})

test_that("a malformed entry id is refused", {
    malformed <- c(
        "DIA-NN/2.x/peptidoform_ion/layers/PEP",
        "DIA-NN/2.x/peptidoform_ion/obsm/X_pca/-",
        "DIA-NN/-/peptidoform_ion/layers/PEP/-",
        "*/2.x/peptidoform_ion/layers/PEP/-",
        "DIA-NN/2.x/peptidoform_ion/layers/-/PEP",
        "DIA-NN/2.x/peptidoform_ion/var/Q/Q_Value",
        "DIA-NN/2.x/peptidoform_ion/var/-/*"
    )
    for (text in malformed) {
        expect_error(
            apbCatalogR:::entry_id_fields(text),
            "entry id",
            label = text
        )
    }
})

test_that("an entry of every level takes the level it answers on", {
    expect_identical(
        apbCatalogR:::entry_id_fields(
            "annotation:prolfquapp/*/*/obs/-/subject*"
        )$level,
        "*"
    )
    entries <- unlist(
        lapply(
            apbCatalogR:::load_set("obs_annotation_alpha")$annotations,
            function(found) found$entries
        ),
        FALSE
    )
    group <- Find(
        function(entry) identical(entry$reference$prefix, "group"),
        entries
    )
    expect_identical(group$entry_id, "annotation:prolfquapp/*/*/obs/-/group*")
    bound <- apbCatalogR:::bound_entry(
        apbCatalogR:::entry_on(group, "protein"),
        "group_x"
    )
    expect_identical(
        bound$entry_id,
        "annotation:prolfquapp/*/protein/obs/-/group_x"
    )
    expect_identical(
        c(bound$reference$level, bound$reference$name),
        c("protein", "group_x")
    )
})
