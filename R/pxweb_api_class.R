#' A Reference Class to represent an pxweb_api
#' 
#' @field api The name of the api (api domain)
#' @field description Short description of the pxweb api.
#' @field url An url template for the pxweb api.
#' @field versions A character vector with versions of the api.
#' @field languages A character vector with languages of the api.
#' @field calls_per_period The number of allowed calls per period.
#' @field period_in_seconds The length of the period with allowed calls.
#' @field max_values_to_download Maximum number of values to download with each call.
#' 
#' @examples
#'  scb_pxweb_api <- pxweb_api$new(get_api = "api.scb.se")
#'  scb_pxweb_api
#'  
#'  
#' @export pxweb_api
pxweb_api <- 
  setRefClass(
    Class = "pxweb_api", 
    fields = list(api = "character",
                  alias = "character",
                  description = "character",
                  url = "character",
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
        api_list <- get_api_list()
        api_index <- get_api_index(api_name, api_list) # api_name <- "foo.bar"
        api_to_use <- api_list[[api_index]]
        api <<- names(api_list)[api_index]
        if("alias" %in% names(api_to_use)) alias <<- api_to_use$alias
        description <<- api_to_use$description
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
      
      
      initialize = function(get_api = NULL, ...){
        'Create a new pxweb_api object.'
        if(is.null(get_api)){
          .self$initFields(...)
          .self$check_input()
        } else {
          .self$get_api(api_name = get_api)
        }
      },
      
      check_input = function(){
        'Check the consistency of the fields.'
        # Check that single_element elemens only is of length one.        
        field_names <- names(getRefClass(class(.self))$fields())
        field_exist <- logical(length(field_names))
        for (fn in seq_along(field_names)){
          field_exist[fn] <- length(.self$field(field_names[fn])) != 0
        }
        if(all(!field_exist)) return(invisible(TRUE))
        
        field_names <- field_names[!field_names %in% "alias"]
        single_elements <- c("api", "url", "calls_per_period", "period_in_seconds", "max_values_to_download")
        for(fn in field_names){
          if(length(.self$field(fn)) == 0){
            stop(fn, " is missing.", call. = FALSE)
          }
          if(length(.self$field(fn)) != 1 & fn %in% single_elements){
            stop(fn, " is not of length 1.", call. = FALSE)
          }
        }        

        # Check structure of url
        has_lang <- grepl(x = .self$url, pattern = "\\[lang\\]")
        if(!has_lang) stop("[lang] is missing in url.", call. = FALSE)
        has_version <- grepl(x = .self$url, pattern = "\\[version\\]")
        if(!has_version) stop("[version] is missing in url.", call. = FALSE)
        has_http <- grepl(x = .self$url, pattern = "^http://")
        if(!has_http) stop("url is not a http adress", call. = FALSE)
      }, 
      
      test_api = function(test_type = "fast", seed = as.integer(Sys.time())){
        'Test to connect to the api and download for each api version/language.

         :param test_type: c("fast", "sample", "full"). 

                        "fast" check that one random datapoint can be downloaded from each api

                        "sample" check that one random datapoint can be downloaded from each node

                        "full" check that all datapoints can be downloaded

         :param seed: seed to use for random choice of data points.
         '
        
        for (v in .self$versions){
          for (l in .self$languages){
            stopifnot(httr::url_success(.self$base_url(version = v, language = l)))
            if(test_type == "fast"){
              node <- get_pxweb_metadata(.self$base_url(version = v, language = l))
              choice <- sample(x = 1:nrow(node), size = 1)
              message("Check node : ", node$text[choice])
              node <- get_pxweb_metadata(choose_pxweb_database_url(.self$base_url(version = v, language = l), pre_choice = choice))
              choice <- sample(x = 1:length(node$URL), size = 1)
              while(node$type[choice] == "l"){
                message("Check node : ", node$text[choice])
                node <- get_pxweb_metadata(path = node$URL[choice])
                choice <- sample(x = 1:length(node$URL), size = 1)
              }
        
              call_meta_data <- suppressMessages(get_pxweb_dims(get_pxweb_metadata(path = node$URL[1])))
              test_dim_list <- vector("list", length(call_meta_data))
              for(i in seq_along(call_meta_data)){
                test_dim_list[[i]] <- call_meta_data[[i]]$values[1]
                names(test_dim_list)[i] <- names(call_meta_data)[i]
              }
              res <- get_pxweb_data(url = node$URL[1], dims = test_dim_list, clean = FALSE)
              if (!is.data.frame(res)) stop(paste0("Could not download data from ", 
                                                   node$URL[1]),
                                                   call = FALSE)
            } else if(test_type == "sample"){
              res <- test_pxweb_api(url = .self$base_url(), download_all = FALSE, seed = seed)
            } else if(test_type == "full"){
              res <- test_pxweb_api(url = .self$base_url(), download_all = FALSE, seed = seed)
            }
          }
        }
        message("pxweb api ok.")
        invisible(res)
      },
                  
      check_alt = function(version = NULL, language = NULL){
        'Check if the version/language alternative exist in the object.'
        if(!is.null(version)) {
          stopifnot(version %in% .self$versions)
        }
        if(!is.null(language)) {
          stopifnot(language %in% .self$languages)
        }
      },
      
      pxweb_api_to_list = function(){
        'Create a list of the current pxweb_api object'
        api_list <- list(description = .self$description,
                         url = .self$url,
                         version = .self$versions,
                         lang = .self$languages,
                         calls_per_period = .self$calls_per_period,
                         period_in_seconds = .self$period_in_seconds, 
                         max_values_to_download = .self$max_values_to_download
                         )
        return(api_list)
      },
      
      write_to_catalogue = function(){
        'Save/overwrite the current api as a locally stored api.'
        api_list_raw <- get_api_list(raw = TRUE)
        api_list_raw$local_apis[[.self$api]] <-
          .self$pxweb_api_to_list()
        write_api_list(api_list = api_list_raw)
        message("If this is a public PXWEB api, it would be great to include it in the public api catalogue.")
        message("If possible, please send the PXWEB api info to ", maintainer("pxweb"), ".")        
      },
      
      show = function(){
        'Print the pxweb api object.'
        cat("Api:", .self$api)
        if(length(.self$alias) > 0) cat(" ('", paste(.self$alias, collapse = "', '"), "')", sep="")
        cat("\n    ", .self$description, "\n")
        cat("Version(s)   :", paste(.self$versions, collapse = ", "), "\n")
        cat("Language(s)  :", paste(.self$languages, collapse = ", "), "\n")
        cat("Limit(s)     :", .self$calls_per_period ,"calls per", .self$period_in_seconds, "sec.\n")
        cat("              ", .self$max_values_to_download ," values per call.\n")
        cat("Url template :\n", .self$url, "\n")
      }
      )
  )        


