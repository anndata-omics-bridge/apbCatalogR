# An APB result as anndataR objects, read from apb2's h5ad file.

H5_FORMAT <- "apb2-parsed-levels"
H5_FORMAT_VERSION <- "8"

# Reads the one-level h5ad apb2 writes into a ParsedLevels object laid out as read_parsed_levels() lays out a
# Parquet result: the level's APB part, at uns[[level]]$apb in the file, becomes the level's uns$apb; the root's
# part, at uns$apb in the file, becomes the result's uns, metadata and hierarchy.
read_h5ad_levels <- function(path) {
  stored <- anndataR::read_h5ad(path, as = "InMemoryAnnData")
  root <- h5_namespace(stored$uns$apb, path, "uns$apb")
  entries <- root$storage$levels
  if (length(entries) != 1L) {
    stop(sprintf("%s declares %d levels; an h5ad holds exactly one", path, length(entries)), call. = FALSE)
  }
  name <- entries[[1]]$name
  part <- stored$uns[[entries[[1]]$physical_name]]$apb
  level <- h5_namespace(part, path, sprintf("uns$%s$apb", entries[[1]]$physical_name))
  apb <- root$apb
  ParsedLevels$new(
    levels = stats::setNames(list(h5_level(stored, level, name)), name),
    uns = apb$parse %||% list(),
    metadata = apb[setdiff(names(apb), c("parse", "hierarchy"))],
    hierarchy = apb$hierarchy,
    annotation_tables = list(),
    feature_relations = list()
  )
}

# One stored APB part: its storage descriptor, and its metadata with the values stored as JSON text decoded.
h5_namespace <- function(namespace, path, where) {
  if (!is.character(namespace$storage)) {
    stop(sprintf("%s has no APB storage descriptor at %s", path, where), call. = FALSE)
  }
  storage <- jsonlite::fromJSON(namespace$storage, simplifyVector = FALSE)
  if (!identical(storage$format, H5_FORMAT) || !identical(storage$format_version, H5_FORMAT_VERSION)) {
    stop(
      sprintf(
        "%s is %s version %s; apbCatalogR reads %s version %s",
        path,
        storage$format,
        storage$format_version,
        H5_FORMAT,
        H5_FORMAT_VERSION
      ),
      call. = FALSE
    )
  }
  apb <- json_shape(namespace[setdiff(names(namespace), "storage")])
  for (where in storage$json_values) {
    keys <- as.character(unlist(where))
    apb[[keys]] <- jsonlite::fromJSON(apb[[keys]], simplifyVector = FALSE)
  }
  list(storage = storage, apb = apb)
}

# A uns value as jsonlite reads JSON: arrays, which anndataR reads with a dim, become unnamed lists.
json_shape <- function(value) {
  if (is.list(value)) {
    return(lapply(value, json_shape))
  }
  if (is.atomic(value) && (!is.null(dim(value)) || length(value) != 1L)) as.list(as.vector(value)) else value
}

h5_level <- function(stored, level, name) {
  storage <- level$storage
  obs_keys <- as.character(unlist(storage$obs_key_columns))
  var_keys <- as.character(unlist(storage$var_key_columns))
  obs <- declared_types(stored$obs, storage$obs)
  var <- declared_types(stored$var, storage$var)
  obs_names <- axis_names(obs, obs_keys)
  var_names <- axis_names(var, var_keys)
  rownames(obs) <- obs_names
  rownames(var) <- var_names
  entries <- storage$layers
  layer_order <- vapply(entries, function(entry) entry$name, character(1))
  layers <- stats::setNames(
    lapply(entries, function(entry) {
      matrix <- if (identical(entry$location, "X")) stored$X else stored$layers[[entry$physical_name]]
      matrix <- as.matrix(matrix)
      dimnames(matrix) <- list(obs_names, var_names)
      matrix
    }),
    layer_order
  )
  primary <- layer_order[vapply(entries, function(entry) identical(entry$location, "X"), logical(1))]
  apb <- level$apb
  apb$storage <- list(
    level = name,
    primary_layer = primary,
    layer_order = layer_order,
    obs_key_columns = obs_keys,
    var_key_columns = var_keys
  )
  export_keys <- as.character(unlist(storage$export_uns))
  anndataR::AnnData(
    X = layers[[primary]],
    obs = obs,
    var = var,
    layers = layers[setdiff(layer_order, primary)],
    obsm = h5_aligned(stored$obsm, storage$obsm, obs_names),
    varm = h5_aligned(stored$varm, storage$varm, var_names),
    obsp = h5_pairwise(stored$obsp, storage$obsp, obs_names),
    varp = h5_pairwise(stored$varp, storage$varp, var_names),
    uns = c(list(apb = apb), json_shape(stored$uns[export_keys]))
  )
}

# The obsm or varm tables of one level under their logical names, named like the axis they align with.
h5_aligned <- function(stored, entries, names) {
  tables <- lapply(entries, function(entry) {
    table <- as.data.frame(stored[[entry$physical_name]])
    rownames(table) <- names
    table
  })
  stats::setNames(tables, vapply(entries, function(entry) entry$name, character(1)))
}

# The obsp or varp matrices of one level under their logical names.
h5_pairwise <- function(stored, entries, names) {
  matrices <- lapply(entries, function(entry) {
    matrix <- stored[[entry$physical_name]]
    dimnames(matrix) <- list(names, names)
    matrix
  })
  stats::setNames(matrices, vapply(entries, function(entry) entry$name, character(1)))
}
