#' A Reference Class to represent an pxweb_api
#'
#' @field url An url template for the pxweb api.
#' @field versions A character vector with versions of the api.
#' @field languages A character vector with languages of the api.
#' @field calls_per_period The number of allowed calls per period.
#' @field period_in_seconds The length of the period with allowed calls.
#' @field max_values_to_download Maximum number of values to download with each call.
#' 
#' @export pxweb_api
#' 
pxweb_api <- 
  setRefClass(
    Class = "pxweb_api", 
    fields = list(url = "character",
                  versions = "character",
                  languages = "character",
                  calls_per_period = "numeric",
                  period_in_seconds = "numeric",
                  max_values_to_download = "numeric"),
    methods = list(
      get_api = function(api_name){
        'Get the api configuration from inst/extdata/api.json for the 
         api_name.'
        if(length(api_name) > 1) stop("Only one API can be chosen.", call. = FALSE)
        api.list <- get_api_list()
        api_to_use <- api.list[[api_name]] 
        if(is.null(api_to_use)) stop("API do not exist in api catalogue.")
        url <<- api_to_use$url
        versions <<- api_to_use$version
        languages <<- api_to_use$lang
        calls_per_period <<- api_to_use$calls_per_period
        period_in_seconds <<- api_to_use$period_in_seconds
        max_values_to_download <<- api_to_use$max_values_to_download
      },
      
      
      base_url = function(version = NULL, language = NULL){
        'Create a base url from the api for version and language.
         The first element of versions and languages is used as standard.'
        if(is.null(version)) {
          version <- .self$versions[1]
        } else {
          check_alt(version=version)          
        }
        if(is.null(language)) {
          language <- .self$languages[1]
        } else {
          check_alt(language=language)
        }
        
        # Create base_url
        split_url <- strsplit(.self$url, "\\]|\\[")[[1]]
        split_url[grepl(pattern = "^lang$", split_url)] <- language
        split_url[grepl(pattern = "^version$", split_url)] <- version
        paste(split_url, collapse = "")
      }, 
      
      
      initialize = function(api_name = NULL, ...){
        'Create a new pxweb_api object.'
        if(is.null(api_name)){
          check_input(...)
          .self$initFields(...)
          # Check that all urls are working
          for (v in to_check$versions){
            for (l in to_check$languages){
              http_ok <- httr::url_ok(.self$base_url())
              if(!http_ok) warning(.self$base_url(), " is not working.", call. = FALSE)
            }
          }
        } else {
          .self$get_api(api_name)
        }
      },
      
      
      check_input = function(...){
        'Check the input that it is ok.'
        to_check <- list(...)

        # Check that single_element elemens only is of length one.
        single_element <- c("url", "calls_per_period", "period_in_seconds", "max_values_to_download")
        check <- unlist(lapply(X = to_check[single_element], function(X) length(X) == 1))
        if(!all(check)) stop(paste0(paste(single_element[!check], collapse = ", "), " contain(s) more than one element."), call. = FALSE)        
        
        # Check structure of url
        has_lang <- grepl(x = to_check$url, pattern = "\\[lang\\]")
        if(!has_lang) stop("[lang] is missing in url.", call. = FALSE)
        has_version <- grepl(x = to_check$url, pattern = "\\[version\\]")
        if(!has_version) stop("[version] is missing in url.", call. = FALSE)

        # Check that all fields are correct
        fields_in_class <- names(getRefClass(as.character(class(.self)))$fields())
        all_fields <- 
          fields_in_class %in% names(to_check)
        if(!all(all_fields)) stop(paste0(paste(fields_in_class[!all_fields],collapse = ", "), " is missing."), call. = FALSE)
      }, 
                  
      check_alt = function(version = NULL, language = NULL){
        'Check if the version/language alternative exist in the object.'
        if(!is.null(version)) {
          stopifnot(version %in% .self$versions)
        }
        if(!is.null(language)) {
          stopifnot(language %in% .self$languages)
        }
      }#,
      #send_to_developers = function(){
      #  'Print out as json object, constructing api and maintainer e-mail.'
      #  # Se
      #}      
      )
  )        


