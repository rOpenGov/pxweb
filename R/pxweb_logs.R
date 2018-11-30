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
  internal_path <- pxweb_log_paths_path()
  checkmate::assert_path_for_output(log_file_path, overwrite = TRUE)
  if(file.exists(log_file_path)){
    file.remove(log_file_path)
  }
  write(paste0("PXWEB API HTTP LOG ", Sys.time(), ":"), file = log_file_path)
  if(file.exists(internal_path)){
    file.remove(internal_path)
  }
  save(log_file_path, file = internal_path)
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
  file.path(pxweb_tempdir(to = "logs"), "log_http_calls.rda")
}


#' @keywords internal
#' @rdname pxweb_http_log_on
pxweb_http_log_is_on <- function(){
  lpp <- pxweb_log_paths_path()
  file.exists(lpp)
}

#' @keywords internal
#' @rdname pxweb_http_log_on
pxweb_log_http_response <- function(r){
  checkmate::assert_class(r, "response")
  if(!pxweb_http_log_is_on()){
    return(invisible(NULL))
  }
  log_file_path <- NULL # Load the file path next
  load(pxweb_log_paths_path())
  
  txt <- pxweb_response_to_log_as_json(r)
  write(c(paste0("CALL AND RESPONSE FROM PXWEB API ", Sys.time(), ": "), txt), append = TRUE, file = log_file_path)
  return(invisible(NULL))
}

#' @keywords internal
#' @rdname pxweb_http_log_on
pxweb_response_to_log_as_json <- function(r){
  checkmate::assert_class(r, "response")
  txt <- unclass(r)
  txt$content <- "The content is removed to limit log file size."
  txt$request <- unclass(txt$request)
  txt$request$output <- unclass(txt$request$output)
  txt$handle <- NULL
  jsonlite::toJSON(txt, pretty = TRUE, auto_unbox = TRUE)
}
