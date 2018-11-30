#' Setup a structure to log all API calls
#' @details 
#' Set up internal structure to handle http PXWEB API calls. http calls are stored in a log file.
#' To access the path to this log file, the path is stored in the pxweb folder in the tempdir(). 
#' The path is given by \code{pxweb_log_paths_path()}.
#' If the file exists, the http calls should be logged (appended) to the log file where the object contains the path.
#' 
#' @param fn The file name of the log file.
#' @keywords internal
pxweb_http_log_on <- function(fn = "log_pxweb_api_http_calls.txt"){
  checkmate::assert_string(fn)
  log_file_path <- file.path(getwd(), fn)
  checkmate::assert_path_for_output(log_file_path, overwrite = TRUE)
  if(file.exists(log_file_path)){
    file.remove(log_file_path)
  }
  write(paste0("PXWEB API HTTP LOG ", Sys.time(), ":"), file = lp)
  if(file.exists(pxweb_log_paths_path())){
    file.remove(pxweb_log_paths_path())
  }
  save(log_file_path, file = pxweb_log_paths_path())
}

#' @keywords internal
#' @rdname pxweb_http_log_on
pxweb_http_log_off <- function(){
  lp <- pxweb_log_paths_path()
  if(file.exists(lp)){
    file.remove(lp)
  }
}

#' @keywords internal
#' @rdname pxweb_http_log_on
pxweb_log_paths_path <- function(){
  file.path(tempdir(), "pxweb", "log_http_calls.rda")
}

#' @keywords internal
#' @rdname pxweb_http_log_on
pxweb_log_http_response <- function(r){
  lp <- pxweb_log_paths_path()
  if(!file.exists(lp)){
    return(invisible(NULL))
  }
  
}