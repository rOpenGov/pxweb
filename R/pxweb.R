#' S3 constructor for \code{pxweb} api object.
#'
#' @description
#' The pxwebapi object contain all information to do calls to the pxweb api and keep count
#' of the number of calls. The object is constructed
#' The object will also be cached in R temp folder to minimize calls to api.
#' All urls should be passed through the constructor to set up the pxweb api config.
#'
#' \emph{Garantuees}:
#' The base_url has been pinged
#' The sub_path has been checked
#' The config has been captured from the API
#' The url has been checked to be a pxweb api (through config)
#'
#' @param url an url to a pxweb api including language and version of the api. See examples.
#' @param x an an object to test if it is a \code{pxweb} object.
#' @param ... further arguments supplied to \code{print()}.
#'
#' @return
#' A \code{pxweb} object.
#'
#' @examples
#' \dontrun{
#' pxapi_1 <-
#'   pxweb("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24")
#' pxapi_2 <-
#'   pxweb(url = "https://api.scb.se/OV0104/v1/doris/sv")
#' }
#'
#' @export
pxweb <- function(url) {
  if (is.pxweb(url)) {
    return(url)
  }
  checkmate::assert_string(url)
  url_parsed <- parse_url_or_fail(x = url)

  obj <- list(
    url = url_parsed,
    config = NULL,
    calls = NULL,
    paths = NULL
  )

  # Add path to temp api storage and time_stamp slot
  obj$paths <- list(rda_file_path = build_pxweb_rda_file_path(obj))
  if (file.exists(obj$paths$rda_file_path)) {
    obj$calls <- load_pxweb_calls(obj)
  } else {
    obj$calls <- list(time_stamps = list())
  }


  # Add the config slot (require one call first construction of the session)
  obj <- pxweb_add_config(obj)

  # Add subpath and query path
  obj <- pxweb_add_api_subpath(obj)

  # Create object
  class(obj) <- c("pxweb", "list")
  assert_pxweb(obj)
  save_pxweb(obj)

  obj
}


#' @rdname pxweb
#' @keywords internal
#' @export
is.pxweb <- function(x) inherits(x, "pxweb")

#' @rdname pxweb
#' @export
print.pxweb <- function(x, ...) {
  cat("PXWEB API\n")
  cat("url:", httr::build_url(x$url), "\n")
  cat("config:\n")
  for (i in seq_along(x$config)) {
    cat(paste0("  ", names(x$config)[i], ": ", x$config[i], "\n"))
  }
  cat("calls:\n")
  for (i in seq_along(x$calls$time_stamps)) {
    cat(paste0("  ", x$calls$time_stamps[[i]], "\n"))
  }
  cat("paths:\n")
  cat(paste0("  ", names(x$paths)[1], ": ", x$paths[[1]], "\n"))
  cat(paste0("  ", names(x$paths)[2], ": ", x$paths[[2]]$path, "\n"))
}


#' Fix url characters
#' @param x a string to fix
#' @keywords internal
pxweb_fix_url <- function(x) {
  checkmate::assert_string(x)
  # URLencode cannot handle partially encoded strings
  x <- utils::URLencode(utils::URLdecode(x))
  x
}

#' Setup temorary directory for the pxweb
#' @param to to what part of the temp folder (apis or logs)
#' @keywords internal
pxweb_tempdir <- function(to = "apis") {
  checkmate::assert_choice(to, c("apis", "logs"))
  tmp_dir_apis <- file.path(tempdir(), "pxweb", "apis")
  tmp_dir_logs <- file.path(tempdir(), "pxweb", "logs")
  if (!dir.exists(tmp_dir_apis)) {
    dir.create(tmp_dir_apis, recursive = TRUE)
  }
  if (!dir.exists(tmp_dir_logs)) {
    dir.create(tmp_dir_logs, recursive = TRUE)
  }
  if (to == "apis") {
    return(tmp_dir_apis)
  } else {
    return(tmp_dir_logs)
  }
}
