# An APB result as anndataR objects, read from apb2's Parquet result directory.

PARQUET_FORMAT <- "apb2-parsed-levels-parquet"
PARQUET_FORMAT_VERSION <- "9"

#' One APB result: its levels on one observation axis
#'
#' The R counterpart of apb2's `ParsedLevels`, as [read_parsed_levels()] returns it.
#'
#' Each level is an anndataR `AnnData` laid out as apb2's `.h5ad` writer lays it out: the primary layer in `X`, the
#' other layers in `layers`, obs and var frames named by their key columns, and the level's APB part in
#' `uns$apb`: `parse` (with `provenance$rule_json`), `roles`, `storage` and post-parse records.
#'
#' @field levels A named list of anndataR `AnnData`, one per level.
#' @field uns The root parse record.
#' @field metadata The root post-parse records, such as `sdrf` or `fasta`.
#' @field hierarchy How the levels nest, or `NULL`.
#' @field annotation_tables A named list of data frames; each carries its metadata as the attribute `apb`.
#' @field feature_relations A named list of data frames of `source`, `row`, `target`, `column` and `value`; each
#'   carries its metadata as the attribute `apb`.
#' @return The class generator; `ParsedLevels$new()` returns a `ParsedLevels` object.
#' @export
#' @examples
#' results <- tempfile()
#' archive <- system.file("extdata", "test-bundle", "results.tar.gz", package = "apbCatalogR")
#' utils::untar(archive, exdir = results)
#' result <- read_parsed_levels(file.path(results, "edge-two-levels.parquet"))
#' names(result$levels)
#' result$levels$protein
ParsedLevels <- R6::R6Class(
  "ParsedLevels",
  public = list(
    levels = NULL,
    uns = NULL,
    metadata = NULL,
    hierarchy = NULL,
    annotation_tables = NULL,
    feature_relations = NULL,

    #' @description Collect the levels and records of one result.
    #' @param levels A named list of anndataR `AnnData`.
    #' @param uns The root parse record.
    #' @param metadata The root post-parse records.
    #' @param hierarchy How the levels nest, or `NULL`.
    #' @param annotation_tables A named list of data frames.
    #' @param feature_relations A named list of data frames.
    initialize = function(
      levels,
      uns = list(),
      metadata = list(),
      hierarchy = NULL,
      annotation_tables = list(),
      feature_relations = list()
    ) {
      self$levels <- levels
      self$uns <- uns
      self$metadata <- metadata
      self$hierarchy <- hierarchy
      self$annotation_tables <- annotation_tables
      self$feature_relations <- feature_relations
    },

    #' @description Print the levels and their shapes.
    #' @param ... Ignored.
    print = function(...) {
      shapes <- vapply(
        names(self$levels),
        function(name) {
          level <- self$levels[[name]]
          sprintf(
            "  %s: %d x %d, layers: %s",
            name,
            level$n_obs(),
            level$n_vars(),
            paste(level_layer_names(level), collapse = ", ")
          )
        },
        character(1)
      )
      writeLines(c("<ParsedLevels>", shapes))
      invisible(self)
    }
  )
)

#' Read an APB result
#'
#' Reads a result apb2 wrote into a [ParsedLevels] object, one anndataR `AnnData` per level, the same for every
#' format: a `.h5ad` file, a `.duckdb` file, or the directory apb2 writes for a `.parquet` result. Reading DuckDB
#' needs the `duckdb` and `DBI` packages.
#'
#' @param path The result, such as `converted.h5ad`, `converted.duckdb` or the directory `converted.parquet`.
#' @return A [ParsedLevels] object.
#' @export
#' @examples
#' results <- tempfile()
#' archive <- system.file("extdata", "test-bundle", "results.tar.gz", package = "apbCatalogR")
#' utils::untar(archive, exdir = results)
#' read_parsed_levels(file.path(results, "edge-two-levels.parquet"))
read_parsed_levels <- function(path) {
  if (grepl("\\.h5ad$", path)) {
    return(read_h5ad_levels(path))
  }
  if (grepl("\\.duckdb$", path)) {
    return(read_duckdb_levels(path))
  }
  manifest_path <- file.path(path, "manifest.json")
  if (!file.exists(manifest_path)) {
    stop(sprintf("%s is not an APB Parquet result directory", path), call. = FALSE)
  }
  manifest <- read_json_file(manifest_path)
  if (!identical(manifest$format, PARQUET_FORMAT) || !identical(manifest$format_version, PARQUET_FORMAT_VERSION)) {
    stop(
      sprintf(
        "%s is %s version %s; apbCatalogR reads %s version %s",
        path,
        manifest$format,
        manifest$format_version,
        PARQUET_FORMAT,
        PARQUET_FORMAT_VERSION
      ),
      call. = FALSE
    )
  }
  manifest_levels(manifest, path, read_table)
}

# The result a Parquet or DuckDB manifest describes; read(directory, entry) returns the table an entry names.
manifest_levels <- function(manifest, path, read) {
  order <- as.character(unlist(manifest$level_order))
  levels <- stats::setNames(lapply(order, function(name) read_level(path, manifest$levels[[name]], name, read)), order)
  apb <- manifest$apb %||% list()
  ParsedLevels$new(
    levels = levels,
    uns = apb$parse %||% list(),
    metadata = apb[setdiff(names(apb), c("parse", "hierarchy"))],
    hierarchy = apb$hierarchy,
    annotation_tables = read_named_tables(
      file.path(path, "annotation_tables"),
      manifest$annotation_table_order,
      manifest$annotation_tables,
      read
    ),
    feature_relations = read_named_tables(
      file.path(path, "feature_relations"),
      manifest$feature_relation_order,
      manifest$feature_relations,
      read
    )
  )
}

read_table <- function(directory, entry) {
  as.data.frame(arrow::read_parquet(file.path(directory, entry$file)))
}

# A table with the text and category columns its schema declares, for a store that keeps neither: anndata stores
# repeated strings as categories, DuckDB stores categories as text. Categories are ordered, as Parquet stores them.
declared_types <- function(frame, table) {
  columns <- as.character(unlist(table$columns))
  for (index in seq_along(columns)) {
    dtype <- table$schema[[index]]
    column <- columns[[index]]
    if (!is.null(dtype$enum_categories)) {
      categories <- as.character(unlist(dtype$enum_categories))
      frame[[column]] <- factor(as.character(frame[[column]]), levels = categories, ordered = TRUE)
    } else if (identical(dtype$name, "String") && is.factor(frame[[column]])) {
      frame[[column]] <- as.character(frame[[column]])
    }
  }
  frame[columns]
}

read_named_tables <- function(directory, order, entries, read) {
  names <- as.character(unlist(order))
  stats::setNames(
    lapply(names, function(name) {
      entry <- entries[[name]]
      table <- read(directory, entry)
      attr(table, "apb") <- entry[setdiff(names(entry), c("file", "columns", "schema"))]
      table
    }),
    names
  )
}

# Row names from the key columns: the key itself, or the keys joined by "|".
axis_names <- function(frame, keys) {
  if (length(keys) == 1L) {
    return(as.character(frame[[keys]]))
  }
  do.call(paste, c(lapply(keys, function(key) as.character(frame[[key]])), sep = "|"))
}

read_level <- function(path, entry, name, read) {
  directory <- file.path(path, "levels", entry$directory %||% name)
  obs_keys <- as.character(unlist(entry$obs$key_columns))
  var_keys <- as.character(unlist(entry$var$key_columns))
  obs <- read(directory, entry$obs)
  var <- read(directory, entry$var)
  obs_names <- axis_names(obs, obs_keys)
  var_names <- axis_names(var, var_keys)
  rownames(obs) <- obs_names
  rownames(var) <- var_names
  layer_order <- as.character(unlist(entry$layer_order))
  layers <- stats::setNames(
    lapply(layer_order, function(layer) {
      values <- as.matrix(read(file.path(directory, "layers"), entry$layers[[layer]]))
      matrix <- t(values)
      dimnames(matrix) <- list(obs_names, var_names)
      matrix
    }),
    layer_order
  )
  primary <- entry$primary_layer
  apb <- entry$apb %||% list()
  apb$storage <- list(
    level = name,
    primary_layer = primary,
    layer_order = layer_order,
    obs_key_columns = obs_keys,
    var_key_columns = var_keys
  )
  anndataR::AnnData(
    X = layers[[primary]],
    obs = obs,
    var = var,
    layers = layers[setdiff(layer_order, primary)],
    obsm = read_aligned(directory, entry, "obsm", obs_names, read),
    varm = read_aligned(directory, entry, "varm", var_names, read),
    obsp = read_pairwise(directory, entry, "obsp", obs_names, read),
    varp = read_pairwise(directory, entry, "varp", var_names, read),
    uns = c(list(apb = apb), entry$export_uns %||% list())
  )
}

# The obsm or varm tables of one level, named like the axis they align with.
read_aligned <- function(directory, entry, slot, names, read) {
  tables <- read_named_tables(file.path(directory, slot), entry[[paste0(slot, "_order")]], entry[[slot]], read)
  lapply(tables, function(table) {
    rownames(table) <- names
    table
  })
}

# The obsp or varp tables of one level: row, column and value of each stored cell, as sparse matrices.
read_pairwise <- function(directory, entry, slot, names, read) {
  tables <- read_named_tables(file.path(directory, slot), entry[[paste0(slot, "_order")]], entry[[slot]], read)
  lapply(tables, function(table) {
    Matrix::sparseMatrix(
      i = table$row + 1L,
      j = table$column + 1L,
      x = as.numeric(table$value),
      dims = c(length(names), length(names)),
      dimnames = list(names, names)
    )
  })
}

# The level's APB part, its layers and its stored rule.

level_apb <- function(level) {
  level$uns$apb
}

level_layer_names <- function(level) {
  level_apb(level)$storage$layer_order
}

level_layer <- function(level, name) {
  if (identical(name, level_apb(level)$storage$primary_layer)) level$X else level$layers[[name]]
}

level_stored_rule <- function(level) {
  text <- level_apb(level)$parse$provenance$rule_json
  if (is.character(text)) jsonlite::fromJSON(text, simplifyVector = FALSE) else NULL
}

# The software name, version pattern and level a level's stored rule declares, or NULLs.
level_rule_version <- function(level) {
  rule <- level_stored_rule(level)
  found <- list(
    software = rule$software_name,
    pattern = rule$software_version_pattern,
    level = rule$quantification_level
  )
  if (all(vapply(found, is.character, logical(1)))) found else list(software = NULL, pattern = NULL, level = NULL)
}

level_roles <- function(level) {
  string_map(level_apb(level)$roles$columns)
}

# The address keys of the fields a level holds.
level_retained <- function(level) {
  found <- c(
    address_key("layers", NA, level_layer_names(level)),
    address_key("var", NA, colnames(level$var)),
    address_key("obs", NA, colnames(level$obs))
  )
  for (table in names(level$varm)) {
    found <- c(found, address_key("varm", table, colnames(level$varm[[table]])))
  }
  found
}

# The single value of each field apb2 left out of the level, by address key.
level_left_out <- function(level) {
  single <- level_apb(level)$parse$result$uninformative_fields$single_valued
  found <- list()
  for (location in names(single)) {
    for (name in names(single[[location]])) {
      found[[address_key(location, NA, name)]] <- json_text(single[[location]][[name]]$value)
    }
  }
  found
}
