#' Get the api catalogue
#' 
#' @details
#' Get the catalogue of api and store it as a list.
#' 
#' @examples
#' api_catalogue()
#' 
#' @export
api_catalogue <- function(){
  apis <- names(get_api_list())
  api_catalogue <- list()
  for(api in seq_along(apis)){
    api_catalogue[[api]] <- pxweb_api$new(apis[api])
  }
  return(api_catalogue)
}

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
#' @keywords internal 
check_new_pxweb_apis <- function(){
  api_local_name <- names(get_api_list(raw = TRUE)$apis)
  api_remote_name <- try(names(get_api_list_remote(raw = TRUE)$apis), silent = TRUE)
  if(class(api_remote_name) == "try-error") return(NULL)
    
  remote_in_local <- api_remote_name %in% api_local_name
  if(!all(remote_in_local)){
    return(paste0("New PXWEB API(s):\n - ", 
            paste(api_remote_name[!remote_in_local], collapse = "\n - "),
            "\n\nUse update_pxweb_apis() to update the api catalogue."))
  }
}


#' Get (hard coded) api catalogue github urls
#' 
#' @param type which type of github url is needed ('github_api' or 'github_raw').
#' 
#' @keywords internal 
#' 
get_github_api_urls <- function(type){
  if(type == "github_api") {
    return("https://api.github.com/repos/rOpenGov/pxweb/contents/inst/extdata/api.json?ref=master")
#    warning("api_fix")
#    return("https://api.github.com/repos/rOpenGov/pxweb/contents/inst/extdata/api.json?ref=api_fix")
  }
  if(type == "github_raw"){
    return("https://raw.githubusercontent.com/rOpenGov/pxweb/master/inst/extdata/api.json")  
#    warning("api_fix")
#    return("https://raw.githubusercontent.com/rOpenGov/pxweb/api_fix/inst/extdata/api.json")
  }
  stop("No correct type")
}


#' Get the api catalogue from json file
#' 
#' @param raw Get the raw list from the api json file. 
#' 
#' @keywords internal 
#' 
#' @return api_list object
#' 
get_api_list <- function(raw = FALSE){
  api_file <- system.file("extdata/api.json", package = "pxweb")
  api_raw <- RJSONIO::fromJSON(api_file)
  if(raw) return(api_raw)
  api_list <- c(api_raw$apis, api_raw$local_apis)
  api_list
}

#' Get the api catalogue from github
#' 
#' @inheritParams get_api_list
#' 
#' @keywords internal 
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
#' @keywords internal 
#' 
#' @param api_list api list in raw format to write to json file.
write_api_list <- function(api_list){
  dest <- system.file("extdata/api.json", package = "pxweb")
  writeLines(RJSONIO::toJSON(api_list), con = dest)
}


#' Get index of which api has the name or teh alias
#'
#' @param api_name pxweb api name or alias to lookup.
#' @param api_list pxweb api list created with \code{get_api_list()}
#' 
#' @keywords internal 
#' 
get_api_index<- function(api_name, api_list){
  list_to_check <- 
    mapply(c, 
           as.list(names(api_list)),  
           lapply(api_list, function(X) unlist(X$alias)))  
  in_list <- unlist(lapply(list_to_check, function(X) any(X %in% api_name)))
  if(sum(in_list) > 1) warning("Alias exists in multiple pxweb apis.")
  if(sum(in_list) == 0) stop("API do not exist in api catalogue.")
  which(in_list)[1]
}
