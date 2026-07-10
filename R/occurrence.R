#' Find occurrence records from the OBIS open-data parquet dataset
#'
#' @description
#' Mirrors [robis::occurrence()], but instead of paging through the OBIS API it
#' builds a single SQL query against the `obis` view created by [connect()] on top
#' of the OBIS open-data parquet dataset
#' (see <https://github.com/iobis/obis-open-data>). This is generally much faster
#' for bulk downloads, but a few API-only filters have no equivalent column in the
#' parquet schema and are ignored with a warning: `instituteid`, `areaid`, `hab`,
#' `wrims` and `event`.
#'
#' @param connection a `robisdb_conn` object created with [connect()] (or
#'   [connect_opendata()]/[connect_opendata_local()]).
#' @param scientificname the scientific name. Matched against `interpreted.scientificName`
#'   as well as every taxonomic rank column (kingdom, phylum, class, order, family,
#'   genus, species), so higher taxon names (e.g. a family) work as well.
#' @param taxonid the taxon identifier (WoRMS AphiaID). Only matches the resolved,
#'   lowest-rank AphiaID stored per record (`interpreted.aphiaid`).
#' @param datasetid the dataset identifier.
#' @param nodeid the OBIS node identifier.
#' @param instituteid not available in the open-data dataset; ignored with a warning.
#' @param areaid not available in the open-data dataset; ignored with a warning.
#' @param startdate the earliest date on which occurrence took place.
#' @param enddate the latest date on which the occurrence took place.
#' @param startdepth the minimum depth below the sea surface.
#' @param enddepth the maximum depth below the sea surface.
#' @param geometry a WKT geometry string.
#' @param measurementtype the measurement type to be included in the measurements data.
#' @param measurementtypeid the measurement type ID to be included in the measurements data.
#' @param measurementvalue the measurement value to be included in the measurements data.
#' @param measurementvalueid the measurement value ID to be included in the measurements data.
#' @param measurementunit the measurement unit to be included in the measurements data.
#' @param measurementunitid the measurement unit ID to be included in the measurements data.
#' @param redlist include only records with an IUCN Red List category assigned.
#' @param hab not available in the open-data dataset; ignored with a warning.
#' @param wrims not available in the open-data dataset; ignored with a warning.
#' @param extensions which extensions to include (`MeasurementOrFact`, `DNADerivedData`).
#' @param hasextensions which extensions need to be present.
#' @param mof include measurements data (shorthand for `extensions = "MeasurementOrFact"`).
#' @param dna include DNA data (shorthand for `extensions = "DNADerivedData"`).
#' @param absence only include absence records (`TRUE`), exclude them (`NULL`, default)
#'   or include both (`"include"`).
#' @param event not available in the open-data dataset; ignored with a warning.
#' @param dropped only include dropped records (`TRUE`), exclude them (`NULL`, default)
#'   or include both (`"include"`).
#' @param flags quality flags which need to be set.
#' @param exclude quality flags to be excluded from the results.
#' @param fields Darwin Core fields (from `interpreted`) to be included in the results.
#'   Defaults to a curated set of commonly used fields (a fixed list, since unlike the
#'   OBIS API this does not vary per query); pass `"all"` to include every field instead.
#' @param qcfields include the `missing` and `invalid` quality-control field lists.
#' @param verbose logical. Print the SQL query before running it (default = `FALSE`).
#' @return The occurrence records in a `tibble`, mirroring [robis::occurrence()]'s output
#'   (unprefixed, lower/camelCase Darwin Core field names; `mof`/`dna`, when requested,
#'   are list-columns of data frames that can be unnested).
#' @export
occurrence_db <- function(
    connection,
    scientificname = NULL, taxonid = NULL, datasetid = NULL,
    nodeid = NULL, instituteid = NULL, areaid = NULL, startdate = NULL,
    enddate = NULL, startdepth = NULL, enddepth = NULL, geometry = NULL,
    measurementtype = NULL, measurementtypeid = NULL, measurementvalue = NULL,
    measurementvalueid = NULL, measurementunit = NULL, measurementunitid = NULL,
    redlist = NULL, hab = NULL, wrims = NULL, extensions = NULL,
    hasextensions = NULL, mof = NULL, dna = NULL, absence = NULL,
    event = NULL, dropped = NULL, flags = NULL, exclude = NULL,
    fields = NULL, qcfields = NULL, verbose = FALSE
) {

    if (!inherits(connection, "robisdb_conn")) .a("Connection should be of type `robisdb_conn`")

    if (!connection$details$type %in% c("open-data", "open-data-local")) {
        .a("This function can only be used with connections with the open-data dataset")
    }

    unsupported <- c("instituteid", "areaid", "hab", "wrims", "event")
    unsupported_used <- unsupported[!vapply(mget(unsupported), is.null, logical(1))]
    if (length(unsupported_used) > 0) {
        .w("Not available in the open-data dataset, ignoring: {.field {unsupported_used}}")
    }

    if (!is.null(extensions)) .ext_uri(extensions)

    where_q <- .sql_common_filters(scientificname, taxonid, datasetid, nodeid, startdate,
                                   enddate, startdepth, enddepth, geometry, redlist, flags, exclude)

    if (!is.null(measurementtype)) where_q <- c(where_q, .sql_mof_match("measurementType", measurementtype))
    if (!is.null(measurementtypeid)) where_q <- c(where_q, .sql_mof_match("measurementTypeID", measurementtypeid))
    if (!is.null(measurementvalue)) where_q <- c(where_q, .sql_mof_match("measurementValue", measurementvalue))
    if (!is.null(measurementvalueid)) where_q <- c(where_q, .sql_mof_match("measurementValueID", measurementvalueid))
    if (!is.null(measurementunit)) where_q <- c(where_q, .sql_mof_match("measurementUnit", measurementunit))
    if (!is.null(measurementunitid)) where_q <- c(where_q, .sql_mof_match("measurementUnitID", measurementunitid))

    if (!is.null(hasextensions)) {
        where_q <- c(where_q, .sql_has_extensions(hasextensions))
    }

    where_q <- c(where_q, .sql_ternary("absence", absence))
    where_q <- c(where_q, .sql_ternary("dropped", dropped))

    select_q <- c("_id as id", "dataset_id", "node_ids")

    if (!is.null(fields)) {
        if (identical(fields, "all")) {
            select_q <- c(select_q, "interpreted.*")
        } else {
            select_q <- c(select_q, paste0("interpreted.", unique(fields)))
        }
    } else {
        select_q <- c(select_q, paste0("interpreted.", .default_fields()))
    }

    select_q <- c(select_q, "flags", "dropped", "absence")

    if (isTRUE(qcfields)) {
        select_q <- c(select_q, "missing", "invalid")
    }

    # `extensions` holds nested lists and is costly to scan/return, so it is only
    # ever referenced in the select list when explicitly requested via `mof`,
    # `dna`, or `extensions`.
    want_mof <- isTRUE(mof) || "MeasurementOrFact" %in% extensions
    want_dna <- isTRUE(dna) || "DNADerivedData" %in% extensions

    if (want_mof) {
        select_q <- c(select_q, paste0(.sql_extension_select("MeasurementOrFact"), " as mof"))
    }

    if (want_dna) {
        select_q <- c(select_q, paste0(.sql_extension_select("DNADerivedData"), " as dna"))
    }

    query <- paste0("select\n    ", paste(select_q, collapse = ",\n    "), "\nfrom obis")
    if (length(where_q) > 0) {
        query <- paste0(query, "\nwhere\n    ", paste(where_q, collapse = "\n    and "))
    }
    query <- paste0(query, ";")

    if (isTRUE(verbose)) {
        cli::cli_h3("Query")
        cli::cli_code(query)
    }

    tibble::as_tibble(DBI::dbGetQuery(connection$connection, query))
}
