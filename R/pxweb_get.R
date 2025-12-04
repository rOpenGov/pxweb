#' Do a GET call to PXWEB API
#'
#' @param url a \code{pxweb} object or url that can be coherced to a \code{pxweb} object.
#' @param query a json string, json file or list object that can be coherced to a \code{pxweb_query} object.
#' @param verbose should large queries print out progress.
#'
#' @examples
#' \dontrun{
#' url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
#' px_meta_data <- pxweb_get(url)
#'
#' url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101"
#' px_levels <- pxweb_get(url)
#'
#' url <- "https://api.scb.se/OV0104/v1/doris/sv"
#' px_levels <- pxweb_get(url)
#'
#' url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
#' query <- file.path(
#'   system.file(package = "pxweb"),
#'   "extdata", "examples", "json_query_example.json"
#' )
#' px_data <- pxweb_get(url = url, query = query)
#'
#' # Convert to data.frame
#' as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
#'
#' # Get raw data
#' as.matrix(px_data, column.name.type = "code", variable.value.type = "code")
#'
#' # Get data comments
#' pxweb_data_comments(px_data)
#'
#' # Get jsonstat data
#' jstat <- query <- file.path(
#'   system.file(package = "pxweb"),
#'   "extdata", "examples", "json-stat_query_example.json"
#' )
#' jstat_data <- pxweb_get(url = url, query = query)
#'
#' # Get very large datasets (multiple downloads needed)
#' big_query <- file.path(
#'   system.file(package = "pxweb"),
#'   "extdata", "examples", "json_big_query_example.json"
#' )
#' px_data <- pxweb_get(url = url, query = big_query)
#' 
#' # Get json-stat2 data from statfin using downloaded json query
#' jstat2_url <- "https://pxdata.stat.fi:443/PxWeb/api/v1/fi/StatFin/eot/statfin_eot_pxt_132a.px"
#' jstat2_query <- file.path(
#'   system.file(package = "pxweb"),
#'   "extdata", "examples", "sq-api_table_statfin_eot_pxt_132a.px.json"
#' )
#' jstat2_data <- pxweb_get(url = jstat2_url, query = jstat2_query)
#' }
#'
#' @export
pxweb_get <- function(url, query = NULL, verbose = TRUE) {
  pxweb_advanced_get(url = url, query = query, verbose = verbose)
}

#' Do a GET call to PXWEB API and return a data.frame
#'
#' @details
#' The functions use will do a \code{pxweb_query} to a PXWEB \code{url} and return a \code{data.frame}.
#' This is a wrapper for the \code{pxweb_get} function.
#'
#' @seealso See \code{\link{pxweb_get}} for mor general usage and \code{\link{pxweb_query}} for details on PXWEB queries.
#'
#' @inheritParams pxweb_get
#' @inheritParams pxweb_as_data_frame
#'
#' @examples
#' \dontrun{
#' url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
#' query <- file.path(
#'   system.file(package = "pxweb"),
#'   "extdata", "examples", "json_query_example.json"
#' )
#' df <- pxweb_get_data(url = url, query = query)
#' }
#'
#' @export
pxweb_get_data <- function(url, query, verbose = TRUE, column.name.type = "text", variable.value.type = "text") {
  d <- pxweb_advanced_get(url = url, query = query, verbose = verbose)
  as.data.frame(d, column.name.type = column.name.type, variable.value.type = variable.value.type)
}

#' Do a GET call to PXWEB API for advanced users
#'
#' @details
#' This function is intended for more advanced users that want to supply specific arguments in
#' \code{httr} calls or what to debug \code{httr} calls.
#'
#' \code{pxweb_get()} is a wrapper for standard use.
#'
#' @inheritParams pxweb_get
#' @param log_http_calls Should the http calls to the API be logged (for debugging reasons).
#'                       If TRUE, all calls and responses are logged and written to "log_pxweb_api_http_calls.txt" in the working directory.
#' @param pxmdo A \code{pxweb_metadata} object to use for query.
#' @param ... Further arguments sent to \code{httr::POST} (for queries) or \code{httr::GET} (for query = \code{NULL}).
#'            If used with query, also supply a \code{pxweb_metadata} object. Otherwise the same parameters are sent to
#'            both \code{httr::POST} and \code{httr::GET}.
#' @export
pxweb_advanced_get <- function(url, query = NULL, verbose = TRUE, log_http_calls = FALSE, pxmdo = NULL, ...) {
  checkmate::assert_flag(log_http_calls)
  checkmate::assert_class(pxmdo, classes = "pxweb_metadata", null.ok = TRUE)
  if (log_http_calls) {
    pxweb_http_log_on()
  }
  if(!is.pxweb(url)){
    url_parsed <- parse_url_or_fail(x = url)
    if(!has_internet(url_parsed$hostname)){
      message(no_internet_msg(url_parsed$hostname))
      return(NULL)
    }
  }

  px <- pxweb(url)
  if (!is.null(query)) {
    pxq <- pxweb_query(query)
    if (is.null(pxmdo)) {
      pxmd <- pxweb_get(px)
    } else {
      pxmd <- pxmdo
    }
    pxq <- pxweb_add_mandatory_variables(pxq, pxmd)
    pxq <- pxweb_add_metadata_to_query(pxq, pxmd)
    pxweb_validate_query_with_metadata(pxq, pxmd)
    pxqs <- pxweb_split_query(pxq, px, pxmd)
  } else {
    pxq <- NULL
  }

  if (is.null(pxq)) {
    px <- pxweb_add_call(px)
    r <- httr::GET(
      build_pxweb_url(px),
      pxweb_user_agent(),
      ...
    )
    pxweb_http_log_response(r)
    httr::stop_for_status(r)
    pxr <- pxweb_parse_response(x = r)
    pxr <- pxweb_metadata_add_null_values(x = pxr, px)
  } else {
    pxr <- list()
    if (length(pxqs) > 1 & verbose) {
      cat("  Downloading large query (in", length(pxqs), "batches):\n")
      pb <- utils::txtProgressBar(min = 0, max = length(pxqs), style = 3)
    }
    for (i in seq_along(pxqs)) {
      px <- pxweb_add_call(px)
      pxurl <- build_pxweb_url(px)
      pxqs[[i]] <- pxweb_remove_metadata_from_query(pxqs[[i]], pxmd)
      r <- httr::RETRY(
        verb = "POST",
        url = pxurl,
        body = pxweb_as_json(x = pxqs[[i]]),
        times = 5,
        pause_base = 2,
        pause_cap = 30,
        pause_min = 1,
        terminate_on = c(400, 401, 403),
        quiet = !verbose,
        httr::add_headers(`Content-Type` = "application/json"),
        pxweb_user_agent(),
        ...
      )
      pxweb_http_log_response(r)
      httr::stop_for_status(r)
      pxr[[i]] <- pxweb_parse_response(x = r)
      if (length(pxqs) > 1 & verbose) {
        utils::setTxtProgressBar(pb, i)
      }
    }
    if (length(pxqs) > 1 & verbose) {
      close(pb)
    }
    pxr <- pxweb_c(pxr)
    if (inherits(pxr, "pxweb_data")) {
      pxr$pxweb_metadata <- pxmd
      pxr$url <- pxurl
      pxr$time_stamp <- Sys.time()
    }
  }
  if (log_http_calls) {
    pxweb_http_log_off()
  }
  pxr
}

#' Add values to NULL value variables in PXWEB metadata objects
#'
#' @details
#' Some metadata objects may have NULL values. In these cases the values are downloaded and added
#' to the metadata object.
#'
#' @param x an object to check if is a \code{pxweb_metadata} object to which we should add values.
#' @param px a \code{pxweb} object
#'
#' @keywords internal
pxweb_metadata_add_null_values <- function(x, px) {
  if (!inherits(x, "pxweb_metadata")) {
    return(x)
  }
  # Create minimal query
  # Check if any NULL values, if not just return the metadata
  miss_values <- unlist(lapply(lapply(x$variables, names), function(x) !"values" %in% x))
  if (!any(miss_values)) {
    return(x)
  }

  # Create minimal query with all NULL values and 1 obs per non-elimination variables
  qlist <- list()
  null_variable <- rep(FALSE, length(x$variables))
  code_variable <- character(length(x$variables))
  for (i in seq_along(x$variables)) {
    code_variable[i] <- x$variables[[i]]$code
    if (is.null(x$variables[[i]]$values)) {
      qlist[[x$variables[[i]]$code]] <- "*"
      null_variable[i] <- TRUE
      next
    }
    if (x$variables[[i]]$elimination) {
      next
    }
    qlist[[x$variables[[i]]$code]] <- x$variables[[i]]$values[1]
  }
  pxq <- pxweb_query(qlist)

  # download the values and add it to the meta-data object
  pxd <- suppressWarnings(pxweb_advanced_get(url = build_pxweb_url(px), query = pxq, verbose = FALSE, pxmdo = x))
  pxd <- as.data.frame(pxd, column.name.type = "code", variable.value.type = "code", stringsAsFactors = FALSE)

  # assert that the meta-data object is correct.
  null_idx <- which(null_variable)
  for (i in seq_along(null_idx)) {
    x$variables[[null_idx[i]]]$valueTexts <-
      x$variables[[null_idx[i]]]$values <-
      unique(pxd[[code_variable[null_idx[i]]]])
  }
  assert_pxweb_metadata(x)
  x
}


pxweb_user_agent <- function() {
  httr::user_agent("https://github.com/rOpenGov/pxweb")
}

#' Add mandatory variables to query
#'
#' @details
#' Complement queries that lack explicit requests for variables with requests for every value of these variables.
#'
#' @param pxq a \code{pxweb_query} object.
#' @param pxmd a \code{pxweb_metadata} object.
#'
#' @keywords internal
pxweb_add_mandatory_variables <- function(pxq, pxmd) {
  checkmate::assert_class(pxq, "pxweb_query")
  checkmate::assert_class(pxmd, "pxweb_metadata")

  mandatory_variables <- vapply(pxmd$variables, function(x) x$code, character(1))
  provided_variables <- vapply(pxq$query, function(x) x$code, character(1))
  eliminationable_variables <- pxweb_metadata_elimination(pxmd)
  non_eliminationable_variables <- names(eliminationable_variables)[!eliminationable_variables]

  missing_variables <- setdiff(mandatory_variables, provided_variables)
  missing_mandatory_variables <- intersect(missing_variables, non_eliminationable_variables)

  if (length(missing_mandatory_variables) > 0) {
    select_everything_template <- function(x) {
      list(
        code = x,
        selection = list(
          filter = "all",
          values = "*"
        )
      )
    }

    query_appendix <- lapply(missing_mandatory_variables, select_everything_template)
    pxq$query <- append(pxq$query, query_appendix)
    warning(sprintf(
      "Missing mandatory variable(s): %s. \nAll values are requested in query.",
      paste(paste0("'", missing_mandatory_variables, "'"), collapse = ", ")
    ), call. = FALSE)
  }
  return(pxq)
}
