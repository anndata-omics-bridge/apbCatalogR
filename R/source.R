# Reviewed meanings of retained APB fields, per vendor and rule variant; apb_catalog.source and .vocabulary.

UNKNOWN <- "unknown"

# The locations a request reads: var and varm both hold one value per feature.
READS <- list(obs = "obs", layers = "layers", var = c("var", "varm"), varm = "varm")

# A request accepts any subset of the qualifiers, each with admitted values or "unknown".
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
  if (value != UNKNOWN && !(value %in% names(qualifier$values))) {
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
    role = found$role %||% NA_character_,
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

new_entry <- function(found) {
  list(
    entry_id = found$entry_id,
    rules = as.character(unlist(found$rules)),
    reference = new_reference(found$reference),
    qualifiers = string_map(found$qualifiers)
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
    role = NA_character_,
    prefix = NA_character_
  )
  entry
}

new_vendor <- function(found) {
  list(
    type = "vendor",
    label = found$software_name,
    catalogue_id = found$catalogue_id,
    catalogue_version = found$catalogue_version,
    software_name = found$software_name,
    variants = lapply(found$variants, function(variant) {
      list(rule = variant$rule, level = variant$level, software_version_pattern = variant$software_version_pattern)
    }),
    entries = lapply(found$entries, new_entry)
  )
}

new_producer <- function(found) {
  list(
    type = "producer",
    label = paste0("producer:", found$producer),
    producer = found$producer,
    requires = as.character(unlist(found$requires)),
    instruction = found$instruction %||% NA_character_,
    entries = lapply(found$entries, new_entry)
  )
}

new_annotation <- function(found) {
  list(
    type = "annotation",
    label = paste0("annotation:", found$convention),
    convention = found$convention,
    entries = lapply(found$entries, new_entry)
  )
}

new_requirement <- function(found) {
  list(
    level = found$level,
    location = found$location,
    kind = found$kind,
    qualifiers = string_map(found$qualifiers),
    requirement = found$requirement
  )
}

# The entries a vendor catalogue reviewed for one of its rule variants.
entries_for <- function(vendor, variant) {
  Filter(function(entry) variant$rule %in% entry$rules && entry$reference$level == variant$level, vendor$entries)
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
  qualifiers <- entry$qualifiers
  entry$reference$level == row$level &&
    entry$reference$location %in% READS[[row$location]] &&
    identical(unname(qualifiers["kind"]), row$kind) &&
    all(vapply(
      names(row$qualifiers),
      function(name) {
        identical(unname(qualifiers[name]), unname(row$qualifiers[name]))
      },
      logical(1)
    ))
}

# Whether a request on one level and location asks for a requirement row's meaning.
row_asked <- function(row, level, location, qualifiers) {
  level == row$level &&
    location == row$location &&
    identical(qualifiers[["kind"]], row$kind) &&
    all(vapply(
      names(row$qualifiers),
      function(name) {
        identical(qualifiers[[name]], unname(row$qualifiers[name]))
      },
      logical(1)
    ))
}

# One packaged catalogue set: its vocabulary, vendor, producer and annotation catalogues and requirements, indexed by
# software and version. Producers hold the fields APB itself writes and annotations the sample-annotation fields; both
# apply to every level. A set made only of them has no vendor catalogue and needs no review.
CatalogueSet <- R6::R6Class(
  "CatalogueSet",
  public = list(
    name = NULL,
    vocabulary = NULL,
    vocabulary_version = NULL,
    description = NULL,
    catalogues = NULL,
    producers = NULL,
    annotations = NULL,
    requirements = NULL,

    initialize = function(bundle, name) {
      descriptions <- bundle$json("catalogues", "catalogues.json")
      if (!(name %in% names(descriptions))) {
        stop(
          sprintf("unknown catalogue '%s'; packaged: %s", name, paste(names(descriptions), collapse = ", ")),
          call. = FALSE
        )
      }
      vocabularies <- bundle$json("catalogues", "vocabulary.json")
      read_folder <- function(kind, make) {
        folder <- file.path(bundle$dir, "catalogues", kind, name)
        files <- sort_c(list.files(folder, pattern = "\\.json$"))
        lapply(files, function(file) make(bundle$json("catalogues", kind, name, file)))
      }
      description <- descriptions[[name]]
      self$name <- name
      self$vocabulary <- vocabularies$sets[[name]]
      self$vocabulary_version <- vocabularies$vocabulary_version
      self$description <- list(purpose = description$purpose, used_by = as.character(unlist(description$used_by)))
      self$catalogues <- read_folder("sources", new_vendor)
      self$producers <- read_folder("producers", new_producer)
      self$annotations <- read_folder("annotations", new_annotation)
      self$requirements <- lapply(bundle$json("catalogues", "requirements.json")[[name]], new_requirement)
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

    # The requirement row a request asks for, or NULL.
    requirement_for = function(level, location, qualifiers) {
      for (row in self$requirements) {
        if (row_asked(row, level, location, qualifiers)) {
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
  names(current_bundle()$json("catalogues", "catalogues.json"))
}
