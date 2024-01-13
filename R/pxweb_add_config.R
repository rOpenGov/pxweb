#' Add the config slot to a pxweb object
#'
#' @details
#' Checks if there exist a config object in the object.
#' Otherwise it query the api to get it and add that call to the call stack.
#'
#' @param obj an object to add config to
#'
#' @keywords internal
pxweb_add_config <- function(obj) {
  assert_pxweb_url(obj)
  assert_pxweb_rda_file_path(obj)

  if (inherits(obj, "pxweb")) {
    return(obj)
  }

  if (file.exists(obj$paths$rda_file_path)) {
    obj$config <- load_pxweb_config(obj)
    return(obj)
  }

  # Do a call to get the cfg and check if PXWEB API
  # Add that one call has been made
  call_time_stamp <- Sys.time()
  cfg_url <- build_pxweb_config_url(obj)
  r <- httr::GET(cfg_url)
  pxweb_http_log_response(r)

  # Check that we get a config back

  if (!is_pxweb_config_response(r)) {
    base_url <- build_pxweb_url(obj)
    r2 <- httr::GET(base_url)
    pxweb_http_log_response(r2)
    if (is_pxweb_response(r2)) {
      stop(paste0("\nThis is an old PXWEB API not supported by pxweb R package\n(version PX-Web 2014 Dec R1 or later is supported): \n", httr::build_url(obj$url)), call. = FALSE)
    } else {
      stop(paste0("\nThis is not a PXWEB API: \n", httr::build_url(obj$url)), call. = FALSE)
    }
  }

  httpwr <- http_was_redirected(r)
  if (httpwr$was_redirected) {
    warning("PXWEB URL CHANGE:\n from: ", httpwr$redirected_from, "\n to:   ", httpwr$redirected_to, call. = FALSE)
  }

  cfg <- httr::content(r, "parsed")
  if(!is.null(cfg$maxCells)){
    mvtd <- cfg$maxCells
  } else {
    mvtd <- cfg$maxValues
  }

  obj$config <- list(
    calls_per_period = cfg$maxCalls,
    period_in_seconds = cfg$timeWindow,
    max_values_to_download = mvtd,
    CORS = cfg$CORS
  )

  # Add the call to the stack
  obj <- pxweb_add_call(obj, call_time_stamp)
  obj
}


#' Check if a response is a pxweb config response
#'
#' @param x a response object
#' @keywords internal
is_pxweb_config_response <- function(x) {
  checkmate::assert_class(x, "response")
  if (httr::http_error(x)) {
    return(FALSE)
  }
  cfg <- suppressMessages(try(httr::content(x, "parsed"), silent = TRUE))
  (all(c("maxValues", "maxCalls", "timeWindow", "CORS") %in% names(cfg))) & !inherits(cfg, "try-error")
}



#' http_was_redirected
#'
#' @param r an httr response object, e.g. from a call to httr::GET()
#'
#' @return list with slots \code{was_redirected}, \code{redirected_from} and  \code{redirected_to}
#'
#' @references
#' Function in large parts taken from \url{https://petermeissner.de/blog/2018/11/07/using-httr-to-detect-redirects/}.
#'
#' @examples
#' \dontrun{
#' r <- httr::GET("http://httpbin.org/redirect/2")
#' pxweb:::http_was_redirected(r)
#' }
#' @keywords internal
http_was_redirected <- function(r) {
  checkmate::assert_class(r, "response")

  # extract status
  status <-
    vapply(
      X         = r$all_headers,
      FUN       = `[[`,
      FUN.VALUE = integer(1),
      "status"
    )

  # Remove config etc
  redirected_from <- httr::parse_url(r$request$url)
  redirected_from$params <- NULL
  redirected_from$query <- NULL
  redirected_from$fragment <- NULL
  redirected_from$username <- NULL
  redirected_from$password <- NULL
  redirected_to <- httr::parse_url(r$url)
  redirected_to$params <- NULL
  redirected_to$query <- NULL
  redirected_to$fragment <- NULL
  redirected_to$username <- NULL
  redirected_to$password <- NULL

  # check domains in location against original domain
  list(
    was_redirected = any(status >= 300 & status < 400),
    redirected_from = httr::build_url(redirected_from),
    redirected_to = httr::build_url(redirected_to)
  )
}
