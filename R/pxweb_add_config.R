#' Add the config slot to a pxweb object
#' 
#' @details 
#' Checks if there exist a config object in the object. 
#' Otherwise it query the api to get it and add that call to the call stack.
#' 
#' @param obj an object to add config to
#' 
#' @keywords internal
pxweb_add_config <-function(obj){
  assert_pxweb_url(obj)
  assert_pxweb_rda_file_path(obj)
  
  if(inherits(obj, "pxweb")){
    return(obj)
  }

  if(file.exists(obj$paths$rda_file_path)){
    obj$config <- load_pxweb_config(obj)
    return(obj)
  }
  
  # Do a call to get the cfg and check if PXWEB API
  # Add that one call has been made
  call_time_stamp <- Sys.time()
  cfg_url <- build_pxweb_config_url(obj)
  r <- httr::GET(cfg_url)
  
  # Check that we get a config back

  if(!is_pxweb_config_response(r)){
    base_url <- build_pxweb_url(obj)
    r2 <- httr::GET(base_url)
    if(is_pxweb_response(r2)){
      stop(paste0("\nThis is an old PXWEB API not supported by pxweb R package\n(version PX-Web 2014 Dec R1 or later is needed): \n", httr::build_url(obj$url)), call. = FALSE)
    } else {
      stop(paste0("\nThis is not a PXWEB API: \n", httr::build_url(obj$url)), call. = FALSE)
    }
  } 
  
  httpdc <- http_domain_changed(r)
  if(httpdc$domain_changed){
    warning("PXWEB DOMAIN CHANGE:\n from: ", httpdc$redirected_from, "\n to: ", httpdc$redirected_to)
  }
  
  cfg <- httr::content(r, "parsed")
  obj$config <- list(calls_per_period = cfg$maxCalls,
                     period_in_seconds = cfg$timeWindow,
                     max_values_to_download = cfg$maxValues,
                     CORS = cfg$CORS)

  # Add the call to the stack
  obj <- pxweb_add_call(obj, call_time_stamp)
  obj
}


#' Check if a response is a pxweb config response
#' 
#' @param x a response object
#' @keywords internal
is_pxweb_config_response <- function(x){
  checkmate::assert_class(x, "response")
  if(httr::http_error(x)) return(FALSE)
  cfg <- suppressMessages(try(httr::content(x, "parsed"), silent = TRUE))
  (all(c("maxValues", "maxCalls", "timeWindow", "CORS") %in% names(cfg))) & !inherits(cfg, "try-error")
}



#' http_domain_changed
#'
#' @param response an httr response object, e.g. from a call to httr::GET()
#'
#' @return list with slots \code{domain_changed}, \code{redirected_from} and  \code{redirected_to}
#'
#' @references 
#' Function in large parts taken from \url{https://petermeissner.de/blog/2018/11/07/using-httr-to-detect-redirects/}.
#'
#' @keywords internal
http_domain_changed <- function(r){
    # get domain of origignal HTTP request
    orig_domain <-   httr::parse_url(r$request$url)$hostname
    
    # extract location headers
    location <- 
      unlist(
        lapply(
          X   = r$all_headers, 
          FUN = 
            function(x){
              x$headers$location
            }
        )
      )
    
    # new domains
    new_domain <- httr::parse_url(r$request$url)$hostname
    
    # check domains in location against original domain
    list(domain_changed = any(!is.na(new_domain) & new_domain != orig_domain),
         redirected_from = orig_domain,
         redirected_to = new_domain)
  }