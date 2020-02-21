#' Creates a timer that keeps track of how many calls that has been called during
#' a time period depending on the api configuration. If there can't be another call
#' the function pauses R to do the call. 
#' 
#' Use this before function that uses api calls.
#' 
#' @param api_url The url to be passed to \link{api_parameters} to get api_configs.
#' @param calls The number of calls that the functions should count per function call. Default is one.
#' 
#' @keywords internal 
#' 
api_timer <- function(api_url, calls = 1){
  
  api_timestamp_file <- paste(tempdir(), "api_time_stamp.Rdata", sep="/")
  
  if(!file.exists(api_timestamp_file)){ # File doesn't exist
    api_timer <- list(config = api_parameters(api_url)[[1]], 
                      calls = rep(Sys.time(), calls))
    save(api_timer, file=api_timestamp_file)
  } else { # File exist
    load(api_timestamp_file)
    api_timer$calls <- c(api_timer$calls, Sys.time())
    if(length(api_timer$calls) >= api_timer$config$calls_per_period){
      diff <- as.numeric(api_timer$calls[length(api_timer$calls)] - api_timer$calls[1], units="secs")
      Sys.sleep(time=max(api_timer$config$period_in_seconds - diff,0))
      api_timer$calls <- api_timer$calls[as.numeric(Sys.time() - api_timer$calls, units="secs") < api_timer$config$period_in_seconds]
    }
    save(api_timer, file=api_timestamp_file)
  }
}
