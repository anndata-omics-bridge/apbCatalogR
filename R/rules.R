# What apb2's rules declare: the fields each rule level keeps, its roles, its versions; apb_catalog.rules.

# Every effective declaration of a rule level, in apb2's order; none for an unknown one.
rule_declarations <- function(rule, level) {
  bundle <- current_bundle()
  index <- bundle$memo("declarations", function() {
    rows <- bundle$json("rules", "declarations.json")
    stats::setNames(
      lapply(rows, function(row) {
        lapply(row$declarations, function(text) jsonlite::fromJSON(text, simplifyVector = FALSE))
      }),
      vapply(rows, function(row) paste(row$rule, row$level, sep = "\x1f"), character(1))
    )
  })
  index[[paste(rule, level, sep = "\x1f")]] %||% list()
}

# The first effective declaration of a rule level.
rule_declaration <- function(rule, level) {
  found <- rule_declarations(rule, level)
  if (length(found) == 0L) {
    stop(sprintf("no packaged rule %s declares the level %s", rule, level), call. = FALSE)
  }
  found[[1L]]
}

# Every rule level with its software name and version pattern, in apb2's order.
rule_levels <- function() {
  bundle <- current_bundle()
  bundle$memo("rule_levels", function() {
    lapply(bundle$json("rules", "declarations.json"), function(row) {
      stored <- rule_declaration(row$rule, row$level)
      list(
        rule = row$rule,
        level = row$level,
        software_name = stored$software_name,
        software_version_pattern = stored$software_version_pattern
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

# One field's declaration without its note, or NULL; varm fields come from producers, so no rule declares them.
rule_field <- function(stored, location, name) {
  candidates <- if (location == "layers") {
    stored$measurements$layers
  } else if (location == "varm") {
    list()
  } else {
    stored$columns[[location]]
  }
  for (candidate in candidates) {
    if (identical(candidate$name, name)) {
      return(candidate[names(candidate) != "note"])
    }
  }
  NULL
}

# The obs, var and layer fields a rule level declares, as address keys.
rule_retained <- function(rule, level) {
  bundle <- current_bundle()
  bundle$memo(paste("retained", rule, level, sep = "\x1f"), function() {
    stored <- rule_declaration(rule, level)
    found <- vapply(stored$measurements$layers, function(layer) address_key("layers", NA, layer$name), character(1))
    for (location in c("obs", "var")) {
      columns <- stored$columns[[location]] %||% list()
      found <- c(found, vapply(columns, function(column) address_key(location, NA, column$name), character(1)))
    }
    unique(found)
  })
}

# The var column each role names on a rule level.
rule_roles <- function(rule, level) {
  found <- character(0)
  for (column in rule_declaration(rule, level)$columns$var %||% list()) {
    for (role in unlist(column$roles)) {
      found[[role]] <- column$name
    }
  }
  found
}

# The vendor columns apb2 computes one var column of a rule level from.
rule_var_inputs <- function(rule, level, name) {
  column <- rule_field(rule_declaration(rule, level), "var", name)
  if (is.null(column)) character(0) else as.character(unlist(column$inputs))
}

version_label <- function(pattern) {
  label <- current_bundle()$json("rules", "version_labels.json")[[pattern]]
  if (is.null(label)) {
    stop(sprintf("no version label for '%s' in the bundle", pattern), call. = FALSE)
  }
  label
}
