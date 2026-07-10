# Quotes and escapes a string (or vector of strings) as SQL literal(s), e.g. "a's" -> 'a''s'
.sql_quote <- function(x) {
    paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

# Builds a `field in (v1, v2, ...)` condition; numeric values are left unquoted,
# everything else is quoted via .sql_quote()
.sql_in <- function(field, values) {
    vals <- if (is.numeric(values)) {
        paste(values, collapse = ", ")
    } else {
        paste(.sql_quote(values), collapse = ", ")
    }
    paste0(field, " in (", vals, ")")
}

# Builds the OR-ed `scientificname` match condition used by occurrence_db(): checks
# `interpreted.scientificName` plus every taxonomic rank column, so a higher taxon name
# (e.g. a family) matches too, not just species-level names
.sql_taxon_match <- function(names) {
    fields <- c("interpreted.scientificName", paste0("interpreted.", .taxon_rank_fields()))
    conds <- vapply(fields, .sql_in, character(1), values = names)
    paste0("(", paste(conds, collapse = " or "), ")")
}

# True if the list column `field` shares at least one element with `values`
# (used for `nodeid`, which is matched against the `node_ids` list column)
.sql_list_overlap <- function(field, values) {
    paste0("len(list_intersect(", field, ", [", paste(.sql_quote(values), collapse = ", "), "])) > 0")
}

# Converts a Date/character date (or vector of dates) to a Unix epoch (seconds, UTC),
# matching how `interpreted.date_mid`/etc. are stored
.sql_epoch <- function(date) {
    if (is.character(date)) date <- as.Date(date)
    as.numeric(as.POSIXct(date, tz = "UTC"))
}

# The `extensions` column is a struct keyed by extension type URI, each value
# a list of extension records with the raw Darwin Core fields under `source`
.sql_extension_field <- function(name) {
    paste0('extensions."', .ext_uri(name), '"')
}

# Flattens each extension record down to its `source` fields plus `level`,
# dropping the `_id`/`_event_id`/`_occurrence_id`/`id` wrapper fields, to match
# the shape of robis::occurrence()'s `mof`/`dna` list-columns
.sql_extension_select <- function(name) {
    paste0(
        "list_transform(", .sql_extension_field(name), ", x -> struct_insert(x.source, level := x.level))"
    )
}

# True if any of the requested extension types (`extensions`, a vector of
# "MeasurementOrFact"/"DNADerivedData") is present (non-empty) on the record
.sql_has_extensions <- function(extensions) {
    conds <- sprintf("len(%s) > 0", vapply(extensions, .sql_extension_field, character(1)))
    paste0("(", paste(conds, collapse = " or "), ")")
}

# True if at least one MeasurementOrFact record on the occurrence has `field` (e.g.
# "measurementType") equal to one of `values`; used by occurrence_db()'s
# measurementtype/measurementvalue/etc. parameters
.sql_mof_match <- function(field, values) {
    conds <- paste(sprintf("x.source.%s = %s", field, .sql_quote(as.character(values))), collapse = " or ")
    paste0(
        "len(list_filter(", .sql_extension_field("MeasurementOrFact"), ", x -> (", conds, "))) > 0"
    )
}

# absence/dropped follow the same tri-state semantics as robis::occurrence():
# NULL (default) excludes them, TRUE keeps only them, "include" applies no filter
.sql_ternary <- function(field, value) {
    if (is.null(value)) {
        paste(field, "is not true")
    } else if (isTRUE(value)) {
        paste(field, "is true")
    } else if (identical(value, "include")) {
        NULL
    } else {
        .a("`{field}` should be `TRUE`, `NULL`, or `'include'`")
    }
}

# Builds the WHERE conditions shared between occurrence_db() and checklist_db():
# scientificname/taxonid/datasetid/nodeid/date/depth/geometry/redlist/flags/exclude.
# absence/dropped/measurement/extension filters are caller-specific and stay in each
# function, since their semantics differ (e.g. checklist_db() always excludes absence
# records; occurrence_db() makes it a tri-state parameter).
.sql_common_filters <- function(scientificname = NULL, taxonid = NULL, datasetid = NULL,
                                 nodeid = NULL, startdate = NULL, enddate = NULL,
                                 startdepth = NULL, enddepth = NULL, geometry = NULL,
                                 redlist = NULL, flags = NULL, exclude = NULL) {
    where_q <- c()

    if (!is.null(scientificname)) {
        where_q <- c(where_q, .sql_taxon_match(scientificname))
    }

    if (!is.null(taxonid)) {
        if (!is.numeric(taxonid)) .a("`taxonid` should be numeric (AphiaID)")
        where_q <- c(where_q, .sql_in("interpreted.aphiaid", taxonid))
    }

    if (!is.null(datasetid)) {
        where_q <- c(where_q, .sql_in("dataset_id", datasetid))
    }

    if (!is.null(nodeid)) {
        where_q <- c(where_q, .sql_list_overlap("node_ids", nodeid))
    }

    if (!is.null(startdate)) {
        where_q <- c(where_q, paste("interpreted.date_mid >=", .sql_epoch(startdate)))
    }

    if (!is.null(enddate)) {
        where_q <- c(where_q, paste("interpreted.date_mid <=", .sql_epoch(enddate)))
    }

    if (!is.null(startdepth)) {
        if (!is.numeric(startdepth)) .a("`startdepth` should be numeric")
        where_q <- c(where_q, paste("interpreted.minimumDepthInMeters >=", startdepth))
    }

    if (!is.null(enddepth)) {
        if (!is.numeric(enddepth)) .a("`enddepth` should be numeric")
        where_q <- c(where_q, paste("interpreted.maximumDepthInMeters <=", enddepth))
    }

    if (!is.null(geometry)) {
        if (!is.character(geometry) || length(geometry) > 1) .a("`geometry` should be a single WKT string")
        where_q <- c(where_q, paste0("ST_Intersects(geometry, ST_GeomFromText(", .sql_quote(geometry), "))"))
    }

    if (isTRUE(redlist)) {
        where_q <- c(where_q, "interpreted.redlist_category is not null")
    }

    if (!is.null(flags)) {
        where_q <- c(where_q, paste(sprintf("list_contains(flags, %s)", .sql_quote(flags)), collapse = " and "))
    }

    if (!is.null(exclude)) {
        where_q <- c(where_q, paste(sprintf("not list_contains(flags, %s)", .sql_quote(exclude)), collapse = " and "))
    }

    where_q
}
