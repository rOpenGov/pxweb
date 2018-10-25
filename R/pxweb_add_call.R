#' Add an api call to a pxweb_api_s3 object
#' 
#' @description 
#' The pxweb_add_call function add a call the the api in the call stack, then compute 
#' 
#' Promise: 
#' The stored rda api object will always have the latest calls
#' This is not thread safe so only one session at a time should call the api.
#' 
#' @param obj a \code{pxweb_api_s3} object
#' 
pxweb_add_call <- function(obj, time_stamp = Sys.time()){
  assert_pxweb_calls(obj)
  assert_pxweb_config(obj)
  checkmate::assert_class(time_stamp, "POSIXct")
  checkmate::assert_number(as.numeric(time_stamp))
  
  if(file.exists(obj$paths$rda_file_path)){
    obj$calls <- load_pxweb_calls(obj)
  }
  
  # Add call to stack
  obj$calls$time_stamps <- c(list(time_stamp), obj$calls$time_stamps)

  # This compute 
  if(length(obj$calls$time_stamps) >= obj$config$calls_per_period){
    second_between_first_and_last_call <- as.numeric(obj$calls$time_stamps[[1]], units="secs") - as.numeric(obj$calls$time_stamps[[length(obj$calls$time_stamps)]], units="secs")
    obj$calls$time_stamps[[length(obj$calls$time_stamps)]] <- NULL
    if(is.pxweb(obj)) save_pxweb(obj)
    # cat(second_between_first_and_last_call, ":", obj$config$calls_per_period, ": Sleep:", max(obj$config$calls_per_period - second_between_first_and_last_call,0))
    # print(obj$calls$time_stamps[c(1,length(obj$calls$time_stamps))])
    Sys.sleep(time=max(obj$config$calls_per_period - second_between_first_and_last_call,0))
  } else {
    if(is.pxweb(obj)) save_pxweb(obj)
  }
  obj
}
