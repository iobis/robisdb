# The dataset types connect() knows how to build a parquet path/URI for
.av_meths <- function() {
    c("open-data", "speciesgrids", "obistherm")
}

# Resolves the `read_parquet()` glob/URI for a given connection type: a local glob under
# `path` for the `*-local` variants, or the dataset's public S3 URI otherwise
.get_path <- function(which_con, path = NULL) {
    if (!is.null(path)) {
        if (which_con == "speciesgrids-local") {
            parquet_path <- file.path(path, "*")
        } else if (which_con == "open-data-local") {
            parquet_path <- file.path(path, "*.parquet")
        } else if (which_con == "obistherm-local") {
            parquet_path <- file.path(path, "*/*")
        } else {
            .a("Connection should be one of {.var {(.av_meths())}}")
        }
    } else {
        if (which_con == "speciesgrids") {
            parquet_path <- "s3://obis-products/speciesgrids/h3_7/*"
        } else if (which_con == "open-data") {
            parquet_path <- "s3://obis-open-data/occurrence/*.parquet"
        } else if (which_con == "obistherm") {
            parquet_path <- "s3://obis-products/obistherm/*/*"
        } else {
            .a("Connection should be one of {.var {(.av_meths())}}")
        }
    }
    return(parquet_path)
}

# Wraps a raw DBI/duckdb connection into a `robisdb_conn` object: records connection
# metadata (`$details`) and initializes the empty query-builder state (`$query`) used by
# the manual_query verbs (select_db()/mutate_db()/filter_db()/etc.)
.new_robisdb_conn <- function(conn, con_type, path = NULL, select = NULL, where = NULL) {
    conn <- list(
        connection = conn
    )
    details <- list(
        creation = Sys.time(),
        active = TRUE,
        type = con_type
    )
    if (!is.null(path)) {
        details$path <- path
    }
    if (!is.null(select)) {
        details$select <- select
    }
    if (!is.null(where)) {
        details$select <- where
    }
    conn$details <- details
    conn$query <- list(select = "*", where = character(0), group_by = character(0), summarized = FALSE)
    class(conn) <- c("robisdb_conn", class(conn))
    return(conn)
}

#' Print a `robisdb_conn`
#'
#' @description
#' Prints a short summary of a `robisdb_conn`: which dataset it's connected to, whether
#' it's still active, the local folder (if any), any fixed `select`/`where` terms baked
#' into the underlying view, and when the connection was created.
#'
#' @param x a `robisdb_conn` object, as returned by [connect()] (or one of its wrappers).
#' @return `NULL`, invisibly (called for its printed side effect).
#' @export
print.robisdb_conn <- function(x) {
    details <- x$details
    pm <- paste(
        paste("Connected with\033[1m", details$type, "\033[22m"),
        paste("Status:", if (details$active) "\033[32mactive\033[39m" else "\033[35minnactive\033[39m"),
        sep = "\n"
    )
    if (!is.null(details$path)) {
        pm <- paste(pm, paste("Local folder:", details$path), sep = "\n")
    }
    if (!is.null(details$select)) {
        pm <- paste(pm, paste(
            "Select terms\033[3m\n", paste("\t", details$select, collapse = "\n"), "\033[23m"
        ), sep = "\n")
    }
    if (!is.null(details$where)) {
        pm <- paste(pm, paste(
            "Where terms\033[3m\n", paste("\t", details$where, collapse = "\n"), "\033[23m"
        ), sep = "\n")
    }
    pm <- paste(pm, paste("Connection created at", details$creation), sep = "\n")
    cli::cli_h2("robisdb connection")
    cat(pm)
    return(invisible(NULL))
}

#' Connect to an OBIS parquet dataset
#'
#' @description
#' Opens a DuckDB connection and creates a view named `obis` over one of OBIS's public
#' parquet datasets -- either read directly from its S3 bucket, or from a local copy (see
#' [sync_opendata()]/[sync_speciesgrids()]/[sync_obistherm()]) when `path` is supplied.
#' All of the package's querying functions ([occurrence_db()], the `select_db()`/
#' `mutate_db()`/... verbs) work against this `obis` view.
#'
#' [connect_opendata()], [connect_opendata_local()], [connect_speciesgrids()],
#' [connect_speciesgrids_local()], [connect_obistherm()], and [connect_obistherm_local()]
#' are convenience wrappers around `connect()` for each dataset.
#'
#' @param con_type the dataset to connect to: one of `"open-data"`, `"speciesgrids"`,
#'   `"obistherm"`.
#' @param path path to a local copy of the dataset. If supplied, connects to the local
#'   files instead of streaming from S3 (recommended -- see [connect()]'s description).
#' @param select optional column list baked into the `obis` view's `select` clause
#'   (defaults to `*`, i.e. every column).
#' @param where optional filter condition(s) baked into the `obis` view itself (applied
#'   to every subsequent query against it).
#' @return a `robisdb_conn` object.
#' @export
connect <- function(con_type, path = NULL, select = NULL, where = NULL) {

    if (length(con_type) > 1) {
        .w("`con_type` must be length one. Ignoring additional values.")
        con_type <- con_type[1]
    }

    assertthat::validate_that(
        con_type %in% .av_meths(),
        msg = .a("`con_type` must be one of {.var {(.av_meths())}}")
    )

    if (!is.null(path)) {
        if (assertthat::validate_that(
            dir.exists(path),
            msg = .a("`path` ({.file {path}}) was supplied but folder was not found.")
        )) {
            con_type <- paste0(con_type, "-local")
        }
    }

    con <- switch(ifelse(grepl("-local", con_type), "local", "standard"),
           local = .create_connection(con_type, path, select, where),
           standard = .create_connection(con_type, path = NULL, select, where))

    return(.new_robisdb_conn(con, con_type, path, select, where))
}

# Opens a raw duckdb connection, installs the extensions needed for the requested
# dataset (spatial always; httpfs too for remote/S3 access), and creates the `obis` view
# over the resolved parquet path. Returns the raw DBI connection (not yet wrapped in
# `robisdb_conn` -- that happens in connect() via .new_robisdb_conn()).
.create_connection <- function(which_con, path = NULL, select = NULL, where = NULL) {
    require(duckdb)
    con <- DBI::dbConnect(duckdb())

    parquet_path <- .get_path(which_con, path)

    if (grepl("-local", which_con)) {
        lq <- "
            install spatial; load spatial;
        "
    } else {
         lq <- "
            install spatial; load spatial;
            install httpfs; load httpfs;
        "
    }

    DBI::dbSendQuery(con, lq)

    if (which_con == "open-data") {
        .w("Connecting to the OBIS open data through S3 is not recommended and can be very slow.
        You can download a local copy using {.fn sync_open_data}.")
    }

    if (!is.null(select)) {
        sel <- paste(select, collapse = ", ")
    } else {
        sel <- "*"
    }
    if (!is.null(where)) {
        where <- paste("where", paste(where, collapse = " and "))
    } else {
        where <- ""
    }

    DBI::dbSendQuery(con, glue::glue("
        create view obis as 
            select {sel} from read_parquet('{parquet_path}')
            {where};
        "))

    return(con)
}

#' Connect to a specific OBIS dataset
#'
#' @description
#' Thin wrappers around [connect()] for each of the three OBIS datasets it supports, in
#' their local (`_local`, given a `path` to a synced copy) and remote (S3-streamed) forms.
#'
#' @param path path to a local copy of the dataset (see [sync_speciesgrids()],
#'   [sync_opendata()], [sync_obistherm()]).
#' @param ... further arguments passed to [connect()] (`select`/`where`).
#' @return a `robisdb_conn` object.
#' @name connect_dataset
NULL

#' @rdname connect_dataset
#' @export
connect_speciesgrids_local <- function(path, ...) {
    connect("speciesgrids", path, ...)
}

#' @rdname connect_dataset
#' @export
connect_speciesgrids <- function(...) {
    connect("speciesgrids", ...)
}

#' @rdname connect_dataset
#' @export
connect_opendata_local <- function(path, ...) {
    connect("open-data", path, ...)
}

#' @rdname connect_dataset
#' @export
connect_opendata <- function(...) {
    connect("open-data", ...)
}

#' @rdname connect_dataset
#' @export
connect_obistherm_local <- function(path, ...) {
    connect("obistherm", path, ...)
}

#' @rdname connect_dataset
#' @export
connect_obistherm <- function(...) {
    connect("obistherm", ...)
}

#' Sync a public, unauthenticated S3 folder to a local directory
#'
#' @description
#' Downloads every object under `s3_prefix` in `bucket` into `local_dir`, skipping files
#' whose local copy already matches the remote size (a cheap up-to-date check, not a
#' full checksum). Connects anonymously (no AWS credentials needed, since OBIS's data
#' buckets are public). [sync_speciesgrids()], [sync_obistherm()], and [sync_opendata()]
#' are wrappers around this for each OBIS dataset.
#'
#' @param bucket the S3 bucket name.
#' @param s3_prefix the folder (key prefix) within the bucket to sync.
#' @param local_dir local destination directory (created if it doesn't exist).
#' @return `NULL`, invisibly (called for its side effect of downloading files).
#' @export
sync_public_s3_folder <- function(bucket, s3_prefix, local_dir) {
    s3 <- paws::s3(credentials = list(anonymous = TRUE))

    if (!grepl("/$", s3_prefix)) s3_prefix <- paste0(s3_prefix, "/")

    if (!dir.exists(local_dir)) {
        dir.create(local_dir, recursive = TRUE)
    }

    message(sprintf("Checking s3://%s/%s for updates...", bucket, s3_prefix))

    is_truncated <- TRUE
    next_token <- NULL

    while (is_truncated) {
        s3_objects <- s3$list_objects_v2(
            Bucket = bucket,
            Prefix = s3_prefix,
            ContinuationToken = next_token
        )

        all_items <- s3_objects$Contents

        if (length(all_items) == 0 && is.null(next_token)) {
            stop("No files found at the specified S3 path. Check your bucket name or prefix.")
        }

        for (item in all_items) {
            if (gsub("/$", "", item$Key) == gsub("/$", "", s3_prefix)) next

            relative_path <- gsub(paste0("^", s3_prefix), "", item$Key)
            local_file_path <- file.path(local_dir, relative_path)

            if (!dir.exists(dirname(local_file_path))) {
                dir.create(dirname(local_file_path), recursive = TRUE)
            }

            should_download <- TRUE
            if (file.exists(local_file_path)) {
                local_size <- file.info(local_file_path)$size
                if (local_size == item$Size) {
                    should_download <- FALSE
                }
            }

            if (should_download) {
                message("Downloading: ", relative_path)
                s3$download_file(Bucket = bucket, Key = item$Key, Filename = local_file_path)
            } else {
                message("Up to date: ", relative_path)
            }
        }

        is_truncated <- isTRUE(s3_objects$IsTruncated)
        next_token <- s3_objects$NextContinuationToken
    }

    message("Sync complete!\n")
}

# True if the `aws` CLI is available on PATH
.has_aws_cli <- function() {
    nzchar(Sys.which("aws"))
}

#' Sync a public, unauthenticated S3 folder to a local directory, via the AWS CLI
#'
#' @description
#' Same end result as [sync_public_s3_folder()] (downloads every object under
#' `s3_prefix` in `bucket` into `local_dir`), but shells out to `aws s3 sync
#' --no-sign-request` instead of using `paws`. Requires the
#' [AWS CLI](https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html)
#' to be installed and on `PATH`; aborts with a clear message if it isn't. `aws s3 sync`
#' is often faster and more robust for a dataset this size (parallel transfers, built-in
#' retry), and needs no AWS credentials (`--no-sign-request`), since OBIS's data buckets
#' are public.
#'
#' @param bucket the S3 bucket name.
#' @param s3_prefix the folder (key prefix) within the bucket to sync.
#' @param local_dir local destination directory (created if it doesn't exist).
#' @return `NULL`, invisibly (called for its side effect of downloading files).
#' @export
sync_public_s3_folder_cli <- function(bucket, s3_prefix, local_dir) {
    if (!.has_aws_cli()) {
        .a("The AWS CLI ({.field aws}) was not found on your PATH. Install it
           (see https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html)
           or use {.fn sync_public_s3_folder} instead.")
    }

    if (!grepl("/$", s3_prefix)) s3_prefix <- paste0(s3_prefix, "/")

    if (!dir.exists(local_dir)) {
        dir.create(local_dir, recursive = TRUE)
    }

    s3_uri <- sprintf("s3://%s/%s", bucket, s3_prefix)
    status <- system2("aws", c("s3", "sync", shQuote(s3_uri), shQuote(local_dir), "--no-sign-request"))
    if (status != 0) .a("`aws s3 sync` exited with a non-zero status ({status})")

    message("Sync complete!\n")
}

#' Sync a local copy of an OBIS dataset
#'
#' @description
#' Downloads (or updates) a local copy of one of OBIS's public datasets from S3.
#' `sync_speciesgrids()`, `sync_obistherm()`, and `sync_opendata()` use
#' [sync_public_s3_folder()] (via `paws`, no extra software needed); the `_cli`
#' variants use [sync_public_s3_folder_cli()] (via the AWS CLI's `aws s3 sync`, if
#' installed -- often faster for a dataset this size). Once synced, connect to the local
#' copy with [connect_speciesgrids_local()]/[connect_obistherm_local()]/
#' [connect_opendata_local()] (much faster than streaming from S3 directly).
#'
#' @param path local destination directory (created if it doesn't exist).
#' @return `NULL`, invisibly (called for its side effect of downloading files).
#' @name sync_dataset
NULL

#' @rdname sync_dataset
#' @export
sync_speciesgrids <- function(path) {
  sync_public_s3_folder(
    bucket = "obis-products",
    s3_prefix = "speciesgrids/h3_7/",
    local_dir = path
  )
}

#' @rdname sync_dataset
#' @export
sync_obistherm <- function(path) {
  sync_public_s3_folder(
    bucket = "obis-products",
    s3_prefix = "obistherm/",
    local_dir = path
  )
}

#' @rdname sync_dataset
#' @export
sync_opendata <- function(path) {
  sync_public_s3_folder(
    bucket = "obis-open-data",
    s3_prefix = "occurrence/",
    local_dir = path
  )
}

#' @rdname sync_dataset
#' @export
sync_speciesgrids_cli <- function(path) {
  sync_public_s3_folder_cli(
    bucket = "obis-products",
    s3_prefix = "speciesgrids/h3_7/",
    local_dir = path
  )
}

#' @rdname sync_dataset
#' @export
sync_obistherm_cli <- function(path) {
  sync_public_s3_folder_cli(
    bucket = "obis-products",
    s3_prefix = "obistherm/",
    local_dir = path
  )
}

#' @rdname sync_dataset
#' @export
sync_opendata_cli <- function(path) {
  sync_public_s3_folder_cli(
    bucket = "obis-open-data",
    s3_prefix = "occurrence/",
    local_dir = path
  )
}

#' Disconnect a `robisdb_conn` (or a raw duckdb connection)
#'
#' @description
#' Closes the underlying DuckDB connection. If `.obj` is a `robisdb_conn`, its
#' `details$active` flag is set to `FALSE` and, if `.obj` is a variable in the caller's
#' environment, that variable is updated in place to reflect the now-inactive state.
#'
#' @param .obj a `robisdb_conn`, or a raw `duckdb_connection`.
#' @return `NULL`, invisibly.
#' @export
disconnect <- function(.obj) {
    obj_name <- deparse(substitute(.obj))
    if (inherits(.obj, "robisdb_conn")) {
        DBI::dbDisconnect(.obj$connection)
        .obj$details$active <- FALSE
        if (exists(obj_name, envir = parent.frame(), inherits = FALSE)) {
            assign(obj_name, .obj, envir = parent.frame())
        }
    } else if (inherits(.obj, "duckdb_connection")) {
        DBI::dbDisconnect(.obj)
    } else {
        .a("Object with no recognizable connection.")
    }
    return(invisible(NULL))
}

#' Run a raw SQL query against a connection
#'
#' @description
#' Thin wrappers around [DBI::dbGetQuery()]/[DBI::dbSendQuery()] for running arbitrary SQL,
#' for anything not covered by [occurrence_db()] or the `select_db()`/`mutate_db()`/...
#' verbs.
#'
#' @param connection a `robisdb_conn` (its underlying connection is used automatically)
#'   or a raw `duckdb_connection`.
#' @param query a SQL string.
#' @return `get_query()` returns the query result as a `data.frame`; `send_query()`
#'   returns the `duckdb_result` from [DBI::dbSendQuery()].
#' @name raw_query
NULL

#' @rdname raw_query
#' @export
get_query <- function(connection, query) {
    DBI::dbGetQuery(.resolve_connection(connection), query)
}

#' @rdname raw_query
#' @export
send_query <- function(connection, query) {
    DBI::dbSendQuery(.resolve_connection(connection), query)
}

# Resolves either a `robisdb_conn` or an already-raw connection to the raw duckdb
# connection, so callers can pass either without spelling out `connection$connection`
.resolve_connection <- function(connection) {
    if (inherits(connection, "robisdb_conn")) return(connection$connection)
    connection
}

#' Install and load DuckDB extensions
#'
#' @description
#' `install_extension()`/`install_community_extension()` install (from DuckDB's core or
#' community extension repository, respectively) and load an extension by name.
#' `install_h3()`, `install_a5()`, and `install_httpfs()` are convenience wrappers around
#' these for extensions not loaded by default by [connect()]: `install_h3()` for H3
#' hexagonal indexing, `install_a5()` for the A5 extension (plus ICU, which it depends
#' on), and `install_httpfs()` for reading remote (S3/HTTP) files -- already loaded
#' automatically by [connect()] for non-local connections, so only needed manually for a
#' local connection that also needs to read remote files.
#'
#' @param connection a `robisdb_conn` (its underlying connection is used automatically)
#'   or a raw `duckdb_connection`.
#' @param name the extension name, for `install_extension()`/`install_community_extension()`.
#' @return the `duckdb_result` from the underlying [DBI::dbSendQuery()] call.
#' @name install_extension
NULL

#' @rdname install_extension
#' @export
install_extension <- function(connection, name) {
    DBI::dbSendQuery(.resolve_connection(connection), sprintf("install %s; load %s;", name, name))
}

#' @rdname install_extension
#' @export
install_community_extension <- function(connection, name) {
    DBI::dbSendQuery(.resolve_connection(connection), sprintf("install %s from community; load %s;", name, name))
}

#' @rdname install_extension
#' @export
install_h3 <- function(connection) {
    install_community_extension(connection, "h3")
}

#' @rdname install_extension
#' @export
install_a5 <- function(connection) {
    install_community_extension(connection, "a5")
    install_extension(connection, "icu")
}

#' @rdname install_extension
#' @export
install_httpfs <- function(connection) {
    install_extension(connection, "httpfs")
}

#' Connect to an arbitrary local parquet dataset
#'
#' @description
#' Like [connect()], but for a parquet path that isn't one of OBIS's three named
#' datasets -- e.g. a custom extract or a dataset produced outside this package. Creates
#' the same `obis` view (with `spatial`/`httpfs` loaded) so [occurrence_db()] and the
#' `select_db()`/`mutate_db()`/... verbs work against it unchanged.
#'
#' @param path path (or glob) to the parquet file(s), passed to `read_parquet()`.
#' @param select optional column list baked into the `obis` view's `select` clause
#'   (defaults to `*`, i.e. every column).
#' @param where optional filter condition(s) baked into the `obis` view itself (applied
#'   to every subsequent query against it).
#' @return a `robisdb_conn` object.
#' @export
connect_duckdb <- function(path, select = NULL, where = NULL) {

    require(duckdb)
    con <- DBI::dbConnect(duckdb())

    lq <- "
            install spatial; load spatial;
            install httpfs; load httpfs;
        "

    DBI::dbSendQuery(con, lq)

    if (!is.null(select)) {
        sel <- paste(select, collapse = ", ")
    } else {
        sel <- "*"
    }
    if (!is.null(where)) {
        where <- paste("where", paste(where, collapse = " and "))
    } else {
        where <- ""
    }

    DBI::dbSendQuery(con, glue::glue("
        create view obis as 
            select {sel} from read_parquet('{path}')
            {where};
        "))

    return(.new_robisdb_conn(con, con_type = "user", path = path, select, where))
}