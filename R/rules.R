# The levels apb2 converts for each software version range, as apb-catalog's build wrote them to rule_levels.csv;
# apb_catalog.rules.

# Every rule level with its software name, version pattern and the pattern's label, in apb2's order.
rule_levels <- function() {
  bundle <- current_bundle()
  bundle$memo("rule_levels", function() {
    rows <- catalogue_rows(bundle, "rule_levels")
    lapply(seq_len(nrow(rows)), function(index) {
      list(
        software_name = rows$software[[index]],
        software_version_pattern = rows$software_version_pattern[[index]],
        version = rows$version[[index]],
        level = rows$level[[index]]
      )
    })
  })
}

# The rule levels of one software whose version pattern admits a version.
rules_matching <- function(software, version) {
  Filter(
    function(found) {
      tolower(found$software_name) == tolower(software) && grepl(found$software_version_pattern, version, perl = TRUE)
    },
    rule_levels()
  )
}

# The concise reading of one rule's software version pattern.
version_label <- function(pattern) {
  for (found in rule_levels()) {
    if (identical(found$software_version_pattern, pattern)) {
      return(found$version)
    }
  }
  stop(sprintf("no packaged rule declares the version pattern '%s'", pattern), call. = FALSE)
}
