#' Find a taxon checklist from the OBIS open-data parquet dataset
#'
#' @description
#' Mirrors [robis::checklist()], but instead of paging through the OBIS API it builds a
#' single SQL query against the `obis` view created by [connect()] on top of the OBIS
#' open-data parquet dataset. Unlike [occurrence_db()], this returns one row per distinct
#' taxon matched by the filters (not one row per occurrence), with a `records` column
#' counting how many occurrence records that taxon has, sorted by `records` descending.
#'
#' A few API-only filters have no equivalent column in the parquet schema and are ignored
#' with a warning: `instituteid`, `areaid`, `hab`, `wrims`. There is no `absence` parameter
#' (matching [robis::checklist()], which doesn't have one either) -- absence records would
#' corrupt the `records` counts, so they are always excluded. There is no `ncbi_id` column
#' in the output ([robis::checklist()] has one): it's an external WoRMS-to-NCBI
#' cross-reference not present anywhere in the open-data parquet dataset.
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
#' @param redlist include only records with an IUCN Red List category assigned.
#' @param hab not available in the open-data dataset; ignored with a warning.
#' @param wrims not available in the open-data dataset; ignored with a warning.
#' @param dropped only include dropped records (`TRUE`), exclude them (`NULL`, default)
#'   or include both (`"include"`).
#' @param flags quality flags which need to be set.
#' @param exclude quality flags to be excluded from the results.
#' @param verbose logical. Print the SQL query before running it (default = `FALSE`).
#' @return A `tibble` with one row per distinct taxon and a `records` count, sorted by
#'   `records` descending.
#' @export
checklist_db <- function(
    connection,
    scientificname = NULL, taxonid = NULL, datasetid = NULL,
    nodeid = NULL, instituteid = NULL, areaid = NULL, startdate = NULL,
    enddate = NULL, startdepth = NULL, enddepth = NULL, geometry = NULL,
    redlist = NULL, hab = NULL, wrims = NULL,
    dropped = NULL, flags = NULL, exclude = NULL, verbose = FALSE
) {

    if (!inherits(connection, "robisdb_conn")) .a("Connection should be of type `robisdb_conn`")

    if (!connection$details$type %in% c("open-data", "open-data-local")) {
        .a("This function can only be used with connections with the open-data dataset")
    }

    unsupported <- c("instituteid", "areaid", "hab", "wrims")
    unsupported_used <- unsupported[!vapply(mget(unsupported), is.null, logical(1))]
    if (length(unsupported_used) > 0) {
        .w("Not available in the open-data dataset, ignoring: {.field {unsupported_used}}")
    }

    where_q <- .sql_common_filters(scientificname, taxonid, datasetid, nodeid, startdate,
                                   enddate, startdepth, enddepth, geometry, redlist, flags, exclude)
    where_q <- c(where_q, "absence is not true")
    where_q <- c(where_q, .sql_ternary("dropped", dropped))

    taxon_fields <- c(
        "scientificName", "scientificNameAuthorship", "taxonRank", "taxonomicStatus",
        "acceptedNameUsage", "acceptedNameUsageID",
        "kingdom", "kingdomid", "phylum", "phylumid", "subphylum", "subphylumid",
        "superclass", "superclassid", "class", "classid", "subclass", "subclassid",
        "superorder", "superorderid", "order", "orderid", "suborder", "suborderid",
        "infraorder", "infraorderid", "section", "sectionid", "subsection", "subsectionid",
        "superfamily", "superfamilyid", "family", "familyid", "subfamily", "subfamilyid",
        "genus", "genusid", "species", "speciesid"
    )

    group_q <- c(paste0("interpreted.", taxon_fields), "interpreted.aphiaid", "interpreted.marine")
    select_q <- c(
        paste0("interpreted.", taxon_fields),
        'interpreted.aphiaid AS "taxonID"',
        'interpreted.marine AS "is_marine"',
        "count(*) as records"
    )

    query <- paste0("select\n    ", paste(select_q, collapse = ",\n    "), "\nfrom obis")
    if (length(where_q) > 0) {
        query <- paste0(query, "\nwhere\n    ", paste(where_q, collapse = "\n    and "))
    }
    query <- paste0(query, "\ngroup by\n    ", paste(group_q, collapse = ", "))
    query <- paste0(query, "\norder by records desc;")

    if (isTRUE(verbose)) {
        cli::cli_h3("Query")
        cli::cli_code(query)
    }

    tibble::as_tibble(DBI::dbGetQuery(connection$connection, query))
}
