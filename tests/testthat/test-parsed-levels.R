# Every synthetic result reads into anndataR levels laid out as apb2's h5ad writer lays them out.

test_that("every synthetic result reads with its levels, layers and stored rule", {
    for (path in list.files(
        results_dir(),
        pattern = "\\.parquet$",
        full.names = TRUE
    )) {
        manifest <- jsonlite::fromJSON(
            file.path(path, "manifest.json"),
            simplifyVector = FALSE
        )
        result <- read_parsed_levels(path)
        expect_s3_class(result, "ParsedLevels")
        expect_identical(
            names(result$levels),
            unlist(manifest$level_order),
            label = basename(path)
        )
        for (name in names(result$levels)) {
            level <- result$levels[[name]]
            entry <- manifest$levels[[name]]
            expect_s3_class(level, "AbstractAnnData")
            expect_identical(
                colnames(level$var),
                unlist(entry$var$columns),
                label = paste(basename(path), name)
            )
            expect_identical(
                colnames(level$obs),
                unlist(entry$obs$columns),
                label = paste(basename(path), name)
            )
            expect_setequal(
                c(entry$primary_layer, names(level$layers)),
                unlist(entry$layer_order)
            )
            expect_identical(dim(level$X), c(level$n_obs(), level$n_vars()))
            expect_identical(
                level$uns$apb$storage$primary_layer,
                entry$primary_layer
            )
            expect_identical(
                level$uns$apb$parse$provenance$rule_json,
                entry$apb$parse$provenance$rule_json
            )
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
    expect_equal(
        unname(protein$layers$PG_Q_Value),
        unname(t(as.matrix(stored)))
    )
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

# The parts of one level every format stores alike.
level_parts <- function(level) {
    list(
        X = level$X,
        layers = level$layers,
        obs = level$obs,
        var = level$var,
        obsm = level$obsm,
        varm = level$varm,
        obsp = level$obsp,
        varp = level$varp
    )
}

# A result read from another format holds what its Parquet result holds; JSON
# records compare as mappings, since h5ad stores their keys in name order.
expect_same_result <- function(actual, expected, label) {
    expect_identical(
        names(actual$levels),
        names(expected$levels),
        label = label
    )
    for (name in names(expected$levels)) {
        expect_identical(
            level_parts(actual$levels[[name]]),
            level_parts(expected$levels[[name]]),
            label = paste(label, name)
        )
        expect_true(
            json_equal(
                actual$levels[[name]]$uns$apb,
                expected$levels[[name]]$uns$apb
            ),
            label = paste(label, name, "uns$apb")
        )
    }
    for (record in c("uns", "metadata", "hierarchy")) {
        expect_true(
            json_equal(actual[[record]], expected[[record]]),
            label = paste(label, record)
        )
    }
    expect_identical(
        actual$annotation_tables,
        expected$annotation_tables,
        label = label
    )
    expect_identical(
        actual$feature_relations,
        expected$feature_relations,
        label = label
    )
}

# Each Parquet result's copy in another format, `suffix`, read as the Parquet
# result reads; `levels` limits it to results of at most that many levels.
expect_format_reads_as_parquet <- function(suffix, levels = Inf) {
    for (path in list.files(
        results_dir(),
        pattern = "\\.parquet$",
        full.names = TRUE
    )) {
        expected <- read_parsed_levels(path)
        if (length(expected$levels) > levels) {
            next
        }
        other <- sub("\\.parquet$", suffix, path)
        expect_true(file.exists(other), label = basename(other))
        expect_same_result(read_parsed_levels(other), expected, basename(other))
    }
}

test_that("every DuckDB result reads as its Parquet result", {
    skip_if_not_installed("duckdb")
    skip_if_not_installed("DBI")
    expect_format_reads_as_parquet(".duckdb")
})

test_that("every one-level h5ad result reads as its Parquet result", {
    skip_if_not_installed("rhdf5")
    expect_format_reads_as_parquet(".h5ad", levels = 1L)
})

test_that("a path is one existing file or directory", {
    missing <- file.path(tempdir(), "missing.h5ad")
    expect_error(read_parsed_levels(missing), "missing.h5ad does not exist")
    expect_error(
        read_parsed_levels(c("a.parquet", "b.parquet")),
        "path is one character string"
    )
})
