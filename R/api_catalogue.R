#' Download and update the PXWEB API catalogue
#' 
#' @details
#' Downloads the latest api catalogue from github.
#' 
#' @export
update_pxweb_apis <- function(){
  api_list <- get_api_list(raw = TRUE)
  api_remote_list <- get_api_list_remote(raw = TRUE)
  api_list$apis <- api_remote_list$apis
  write_api_list(api_list = api_list)
  message("pxweb api catalogue updated.")
}

#' Check and compare differences between remote and local api catalogue
check_new_pxweb_apis <- function(){
  api_local_name <- names(get_api_list(raw = TRUE)$apis)
  api_remote_name <- try(names(get_api_list_remote(raw = TRUE)$apis), silent = TRUE)
  if(class(api_remote_name) == "try-error") return(NULL)
    
  remote_in_local <- api_remote_name %in% api_local_name
  if(!all(remote_in_local)){
    return(paste0("New PXWEB API(s):\n - ", 
            paste(api_remote_name[!remote_in_local], collapse = "\n - "),
            "\n\nUse update_pxweb_apis() to update api catalogue."))
  }
}


#' Get (hard coded) api catalogue github urls
#' 
#' @param type which type of github url is needed ('github_api' or 'github_raw').
#' 
get_github_api_urls <- function(type){
  if(type == "github_api") {
    warning("api_fix")
#   return("https://api.github.com/repos/rOpenGov/pxweb/contents/inst/extdata/api.json?ref=master")
    return("https://api.github.com/repos/rOpenGov/pxweb/contents/inst/extdata/api.json?ref=api_fix")

  }
  if(type == "github_raw"){
    warning("api_fix")
#   return("https://raw.githubusercontent.com/rOpenGov/pxweb/master/inst/extdata/api.json")  
    return("https://raw.githubusercontent.com/rOpenGov/pxweb/api_fix/inst/extdata/api.json")
  }
  stop("No correct type")
}


#' Get the api catalogue from json file
#' 
#' @param raw Get the raw list from the api json file. 
#' 
#' @return api_list object
#' 
get_api_list <- function(raw = FALSE){
  api_file <- system.file("extdata/api.json", package = "pxweb")
  api_raw <- jsonlite::fromJSON(api_file)
  if(raw) return(api_raw)
  api_list <- c(api_raw$apis, api_raw$local_apis)
  api_list
}

#' Get the api catalogue from github
#' 
#' @inheritParams get_api_list
#' 
#' @return api_list object
#' 
get_api_list_remote <- function(raw = FALSE){
  url_raw <- get_github_api_urls(type = "github_raw")
  request <- httr::GET(url_raw)
  httr::stop_for_status(request)
  api_raw <- 
    httr::content(request, "parsed", "application/json", simplifyVector = TRUE)
  if(raw) return(api_raw)
  api_list <- c(api_raw$apis, api_raw$local_apis)
  api_list
}



#' Write api catalogue to json file
#' 
#' @param api_list api list in raw format to write to json file.
write_api_list <- function(api_list){
  dest <- system.file("extdata/api.json", package = "pxweb")
  writeLines(jsonlite::toJSON(api_list), con = dest)
}



