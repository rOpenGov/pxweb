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
#' @return 
#' A \code{pxweb} object with slots:
#' FIX!
#'
#' @examples
#' pxapi_1 <- pxweb(url ="http://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24")
#' pxapi_2 <- pxweb(url ="http://api.scb.se/OV0104/v1/doris/sv")
#' 
#' @export
pxweb <- function(url){
  checkmate::assert_string(url)
  url_parsed <- parse_url_or_fail(url)
  
  obj <- list(url = url_parsed,
              config = NULL,
              calls = NULL,
              paths = NULL)

  # Add path to temp api storage and time_stamp slot
  obj$paths <- list(rda_file_path = build_pxweb_rda_file_path(obj))
  if(file.exists(obj$paths$rda_file_path)){
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
#' @export
is.pxweb <-function(x) inherits(x, "pxweb")
