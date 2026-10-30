# An APB result as anndataR objects, read from apb2's DuckDB file.

DUCKDB_FORMAT <- "apb2-parsed-levels-duckdb"
DUCKDB_FORMAT_VERSION <- "8"
DUCKDB_METADATA_TABLE <- "apb2_result_metadata"

# Reads the DuckDB file apb2 writes into a ParsedLevels object. Its manifest is the Parquet manifest, kept in one
# table; each table it names is a database table instead of a Parquet file.
read_duckdb_levels <- function(path) {
  for (package in c("duckdb", "DBI")) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop(sprintf("reading %s needs the %s package", path, package), call. = FALSE)
    }
  }
  # duckdb announces where it would keep extensions; reading a result loads none.
  connection <- suppressMessages(DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = TRUE))
  on.exit(DBI::dbDisconnect(connection, shutdown = TRUE), add = TRUE)
  if (!DBI::dbExistsTable(connection, DUCKDB_METADATA_TABLE)) {
    stop(sprintf("%s is not an APB DuckDB result", path), call. = FALSE)
  }
  text <- DBI::dbGetQuery(connection, sprintf("SELECT manifest_json FROM %s", DUCKDB_METADATA_TABLE))$manifest_json
  manifest <- jsonlite::fromJSON(text, simplifyVector = FALSE)
  if (!identical(manifest$format, DUCKDB_FORMAT) || !identical(manifest$format_version, DUCKDB_FORMAT_VERSION)) {
    stop(
      sprintf(
        "%s is %s version %s; apbCatalogR reads %s version %s",
        path,
        manifest$format,
        manifest$format_version,
        DUCKDB_FORMAT,
        DUCKDB_FORMAT_VERSION
      ),
      call. = FALSE
    )
  }
  manifest_levels(manifest, path, function(directory, entry) {
    if (!grepl("^data_[0-9]{6}$", entry$file)) {
      stop(sprintf("%s names an unexpected table %s", path, entry$file), call. = FALSE)
    }
    declared_types(DBI::dbReadTable(connection, entry$file), entry)
  })
}
