# Reviewed meanings of retained APB fields, per vendor and software version; apb_catalog.source and .vocabulary.

UNKNOWN <- "unknown"

# The locations a request reads: var and varm both hold one value per feature.
READS <- list(obs = "obs", layers = "layers", var = c("var", "varm"), varm = "varm")

# A request accepts any subset of the qualifiers, each with admitted values; every qualifier admits "unknown".
check_predicate <- function(vocabulary, qualifiers) {
  for (name in names(qualifiers)) {
    accepted <- qualifiers[[name]]
    if (length(accepted) == 0L) {
      stop(sprintf("qualifier '%s' accepts no value", name), call. = FALSE)
    }
    for (value in accepted) {
      check_value(vocabulary, name, value)
    }
  }
}

check_value <- function(vocabulary, name, value) {
  qualifier <- vocabulary$qualifiers[[name]]
  if (is.null(qualifier)) {
    stop(
      sprintf(
        "unknown qualifier '%s'; declared: %s",
        name,
        paste(sort_c(names(vocabulary$qualifiers)), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!(value %in% names(qualifier$values))) {
    stop(
      sprintf(
        "qualifier '%s' does not admit '%s'; admitted: %s",
        name,
        value,
        paste(sort_c(names(qualifier$values)), collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

new_reference <- function(found) {
  list(
    level = found$level,
    location = found$location,
    name = found$name %||% NA_character_,
    table = found$table %||% NA_character_,
    prefix = found$prefix %||% NA_character_
  )
}

# The location, varm table and name a level's retained fields are indexed by, as one string.
address_key <- function(location, table, name) {
  if (length(name) == 0L) {
    return(character(0))
  }
  paste(location, ifelse(is.na(table), "", table), name, sep = "\x1f")
}

reference_key <- function(reference) {
  if (is.na(reference$name)) {
    stop("the reference is not bound to a column", call. = FALSE)
  }
  address_key(reference$location, reference$table, reference$name)
}

# A ";"-separated list of one CSV cell; none when the cell is empty.
cell_list <- function(text, separator = ";") {
  if (identical(text, "")) character(0) else strsplit(text, separator, fixed = TRUE)[[1L]]
}

# The columns of keys.csv besides those of the qualifiers a key fixes.
KEY_TABLE_COLUMNS <- c("set", "key", "kind", "psi_ms")

# A set's keys from its keys.csv rows: each the name callers ask by, the kind and the qualifier values it fixes (an
# empty cell leaves that qualifier open), and its PSI-MS terms.
set_keys <- function(rows) {
  fixed_columns <- setdiff(names(rows), KEY_TABLE_COLUMNS)
  keys <- lapply(seq_len(nrow(rows)), function(index) {
    row <- as.list(rows[index, , drop = FALSE])
    values <- unlist(row[fixed_columns])
    list(key = row$key, kind = row$kind, fixed = values[values != ""], psi_ms = cell_list(row$psi_ms))
  })
  stats::setNames(keys, rows$key)
}

# The qualifier values a request for a key accepts: its kind and the values it fixes.
key_predicate <- function(found) {
  c(list(kind = found$kind), as.list(found$fixed))
}

# The keys an entry with these qualifiers, a named character vector, has: exactly one in a consistent set.
keys_of <- function(keys, qualifiers) {
  described <- Filter(
    function(found) {
      identical(unname(qualifiers["kind"]), found$kind) &&
        all(vapply(
          names(found$fixed),
          function(name) identical(unname(qualifiers[name]), unname(found$fixed[name])),
          logical(1)
        ))
    },
    keys
  )
  vapply(described, function(found) found$key, character(1), USE.NAMES = FALSE)
}

# One entry from its entries.csv row, its other qualifiers, a named character vector, and the key they match.
row_entry <- function(row, qualifiers, keys) {
  if (!(row$reference_by %in% c("name", "prefix"))) {
    stop(sprintf("%s: reference_by is name or prefix", row$entry_id), call. = FALSE)
  }
  reference <- list(level = row$level, location = row$location)
  reference[[row$reference_by]] <- row$reference
  if (row$table != "") {
    reference$table <- row$table
  }
  described <- c(kind = row$kind, qualifiers)
  matching <- keys_of(keys, described)
  if (length(matching) != 1L) {
    stop(
      sprintf("%s: matches the keys %s, not exactly one", row$entry_id, paste(matching, collapse = ", ")),
      call. = FALSE
    )
  }
  list(
    entry_id = row$entry_id,
    software_version_pattern = cell_list(row$software_version_pattern),
    reference = new_reference(reference),
    key = matching,
    qualifiers = described
  )
}

# The entry with its reference bound to one level's column.
bound_entry <- function(entry, name) {
  reference <- entry$reference
  entry$reference <- list(
    level = reference$level,
    location = reference$location,
    name = name,
    table = reference$table,
    prefix = NA_character_
  )
  entry
}

# One catalogue per software of rule_levels.csv, each reviewing every rule level of that software, when the set has
# vendor entries; none otherwise.
vendor_catalogues <- function(name, entries) {
  if (length(entries) == 0L) {
    return(list())
  }
  levels <- rule_levels()
  software <- unique(vapply(levels, function(found) found$software_name, character(1)))
  unknown <- setdiff(names(entries), software)
  if (length(unknown) > 0L) {
    stop(
      sprintf("%s: no packaged rule declares the software %s", name, paste(sort_c(unknown), collapse = ", ")),
      call. = FALSE
    )
  }
  lapply(software, function(vendor) {
    variants <- Filter(function(found) found$software_name == vendor, levels)
    list(
      type = "vendor",
      label = vendor,
      software_name = vendor,
      variants = lapply(variants, function(found) {
        list(level = found$level, software_version_pattern = found$software_version_pattern)
      }),
      entries = entries[[vendor]] %||% list()
    )
  })
}

# An app's catalogue, such as apb-fasta's; its producers.csv row names the run it waits for and what to run.
producer_catalogue <- function(producer, entries, run) {
  list(
    type = "producer",
    label = producer,
    producer = producer,
    requires = cell_list(run$requires, "."),
    instruction = if (identical(run$instruction, "")) NA_character_ else run$instruction,
    entries = entries
  )
}

annotation_catalogue <- function(convention, entries) {
  list(type = "annotation", label = paste0("annotation:", convention), convention = convention, entries = entries)
}

# A set's vocabulary from its vocabulary.csv rows, shaped as apb_catalog's SetVocabulary.
set_vocabulary <- function(rows, head) {
  qualifier_rows <- rows[rows$qualifier != "" & rows$value == "", , drop = FALSE]
  qualifiers <- lapply(seq_len(nrow(qualifier_rows)), function(index) {
    values <- rows[rows$qualifier == qualifier_rows$qualifier[[index]] & rows$value != "", , drop = FALSE]
    list(
      definition = qualifier_rows$definition[[index]],
      values = stats::setNames(as.list(values$definition), values$value)
    )
  })
  list(definition = head$definition, qualifiers = stats::setNames(qualifiers, qualifier_rows$qualifier))
}

# The entries a vendor catalogue reviewed for one of its rule variants.
entries_for <- function(vendor, variant) {
  Filter(
    function(entry) {
      variant$software_version_pattern %in% entry$software_version_pattern && entry$reference$level == variant$level
    },
    vendor$entries
  )
}

# The entries a producer or annotation catalogue supplies on one level.
entries_on <- function(source, level) {
  Filter(function(entry) entry$reference$level == level, source$entries)
}

# Whether a result's metadata records a producer's run.
producer_ran <- function(producer, metadata) {
  section <- metadata
  for (depth in seq_along(producer$requires)) {
    key <- producer$requires[[depth]]
    if (!(key %in% names(section))) {
      return(FALSE)
    }
    if (depth == length(producer$requires)) {
      break
    }
    section <- section[[key]]
    if (!is.list(section) || is.null(names(section))) {
      return(FALSE)
    }
  }
  TRUE
}

pending_reason <- function(producer) {
  sprintf("%s has not run on this result; %s", producer$producer, producer$instruction)
}

annotation_applies <- function(annotation, metadata) {
  annotation$convention %in% names(metadata)
}

# Whether an entry on a requirement row's level answers the row.
row_describes <- function(row, entry) {
  entry$reference$level == row$level &&
    entry$reference$location %in% READS[[row$location]] &&
    identical(entry$key, row$key)
}

# Whether a request on one level and location asks for a requirement row's key.
row_asked <- function(row, level, location, key) {
  level == row$level && location == row$location && identical(key, row$key)
}

# One packaged catalogue set: its vocabulary, keys, vendor, producer and annotation catalogues and requirements, indexed
# by software and version. Producers hold the fields apps write after conversion and annotations the sample-annotation
# fields; both apply to every software version. A set made only of them has no vendor catalogue and needs no review.
CatalogueSet <- R6::R6Class(
  "CatalogueSet",
  public = list(
    name = NULL,
    vocabulary = NULL,
    vocabulary_version = NULL,
    keys = NULL,
    description = NULL,
    catalogues = NULL,
    producers = NULL,
    annotations = NULL,
    requirements = NULL,

    initialize = function(bundle, name) {
      rows <- function(table) {
        found <- catalogue_rows(bundle, table)
        found[found$set == name, , drop = FALSE]
      }
      vocabulary <- rows("vocabulary")
      head <- vocabulary[vocabulary$qualifier == "", , drop = FALSE]
      if (nrow(head) != 1L) {
        stop(
          sprintf("unknown catalogue '%s'; packaged: %s", name, paste(set_names(), collapse = ", ")),
          call. = FALSE
        )
      }
      keys <- set_keys(rows("keys"))
      qualifier_rows <- rows("entry_qualifiers")
      entry_rows <- rows("entries")
      entries <- lapply(seq_len(nrow(entry_rows)), function(index) {
        row <- as.list(entry_rows[index, , drop = FALSE])
        own <- qualifier_rows[qualifier_rows$entry_id == row$entry_id, , drop = FALSE]
        row_entry(row, stats::setNames(own$value, own$qualifier), keys)
      })
      by_source <- split(entries, factor(entry_rows$source, levels = unique(entry_rows$source)))
      sources <- names(by_source)
      runs <- rows("producers")
      annotation <- startsWith(sources, "annotation:")
      app <- !annotation & sources %in% runs$producer
      self$name <- name
      self$vocabulary <- set_vocabulary(vocabulary, head)
      self$vocabulary_version <- bundle$json("catalogues", "datapackage.json")$version
      self$keys <- keys
      self$description <- list(purpose = head$purpose, used_by = cell_list(head$used_by))
      self$catalogues <- vendor_catalogues(name, by_source[!annotation & !app])
      self$producers <- lapply(which(app), function(index) {
        run <- runs[runs$producer == sources[[index]], , drop = FALSE]
        producer_catalogue(sources[[index]], by_source[[index]], run)
      })
      self$annotations <- lapply(which(annotation), function(index) {
        annotation_catalogue(sub("^annotation:", "", sources[[index]]), by_source[[index]])
      })
      requirement_rows <- rows("requirements")
      self$requirements <- lapply(seq_len(nrow(requirement_rows)), function(index) {
        row <- requirement_rows[index, , drop = FALSE]
        if (!(row$key %in% names(keys))) {
          stop(sprintf("requirement on %s: unknown key '%s'", row$level, row$key), call. = FALSE)
        }
        list(level = row$level, location = row$location, key = row$key, requirement = row$requirement)
      })
      for (vendor in self$catalogues) {
        for (variant in vendor$variants) {
          key <- paste(vendor$software_name, variant$software_version_pattern, variant$level, sep = "\x1f")
          private$by_version[[key]] <- list(catalogue = vendor, variant = variant)
        }
      }
    },

    # Every catalogue of the set: vendor, producer and annotation.
    sources = function() {
      c(self$catalogues, self$producers, self$annotations)
    },

    # The rule reviewed for one software version's level, or NULL.
    reviewed = function(software, pattern, level) {
      private$by_version[[paste(software, pattern, level, sep = "\x1f")]]
    },

    # One of the set's keys; an unknown name signals an error naming the set's keys.
    key = function(name) {
      found <- self$keys[[name]]
      if (is.null(found)) {
        known <- paste(sort_c(names(self$keys)), collapse = ", ")
        stop(sprintf("unknown key '%s'; keys: %s", name, known), call. = FALSE)
      }
      found
    },

    # The requirement row a request asks for, or NULL.
    requirement_for = function(level, location, key) {
      for (row in self$requirements) {
        if (row_asked(row, level, location, key)) {
          return(row)
        }
      }
      NULL
    }
  ),
  active = list(
    # Whether a level answers only once a vendor catalogue reviewed its rule.
    needs_review = function() length(self$catalogues) > 0L
  ),
  private = list(by_version = list())
)

catalogue_set <- function(name) {
  bundle <- current_bundle()
  bundle$memo(paste0("set:", name), function() CatalogueSet$new(bundle, name))
}

set_names <- function() {
  found <- catalogue_rows(current_bundle(), "vocabulary")
  found$set[found$qualifier == ""]
}

# One catalogue table of the bundle, as a data frame of strings with empty cells as "", read once; its columns must be
# those datapackage.json declares.
catalogue_rows <- function(bundle, table) {
  bundle$memo(paste0("table:", table), function() {
    found <- utils::read.csv(
      file.path(bundle$dir, "catalogues", paste0(table, ".csv")),
      colClasses = "character",
      na.strings = character(0),
      check.names = FALSE,
      encoding = "UTF-8"
    )
    resources <- bundle$json("catalogues", "datapackage.json")$resources
    resource <- Filter(function(found) identical(found$name, table), resources)[[1L]]
    declared <- vapply(resource$schema$fields, function(field) field$name, character(1))
    if (!identical(names(found), declared)) {
      stop(
        sprintf(
          "%s.csv has the columns %s, not %s",
          table,
          paste(names(found), collapse = ", "),
          paste(declared, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    found
  })
}
