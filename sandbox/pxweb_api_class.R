
pxweb_api <- 
  setRefClass(
    Class = "pxweb_api", 
    fields = list(url = "character",
                  versions = "character",
                  langauges = "character",
                  calls_per_period = "numeric",
                  period_in_seconds = "numeric",
                  max_values_to_download = "numeric"),
    methods = list(
      get_api = function(api_name){
        'Get the api configuration from inst/extdata/api.json for the 
         api_name.'
        
        api.file <- system.file("extdata/api.json", package = "pxweb")
        api.list <- RJSONIO::fromJSON(api.file)
        api_to_use <- api.list[[api_name]] 
        if(is.null(api_to_use)) stop("API do not exist in api catalogue.")
        url <<- api_to_use$url
        versions <<- api_to_use$versions
        langauges <<- api_to_use$langauges
        calls_per_period <<- api_to_use$calls_per_period
        period_in_seconds <<- api_to_use$period_in_seconds
        max_values_to_download <<- api_to_use$max_values_to_download
      },
      base_url = function(version = NULL, language = NULL){
        'Create base url from the api for version and language.'
        # Create url
      }, 
      initialize = function(api_name = NULL, ...){
        'Create a new pxweb_api object.'
        # If api_name != NULL use get_api else create api
        # If create api do check_input
      },
      check_input = function(version = NULL, language = NULL){
        'Check the input that it is ok.'
        # Check that:
        # url is an url that works
        # url contains [language]/[version] if version/language exists
        # all versions/languages work/can bee called
        # Length of the other objects is 1
      }, 
      show = function(){
        'Method for automatically printing api-information'
      },
      set_standard = function(version = NULL, language = NULL){
        'Set the standard of which version and language to use.
         The first element in version and language is used as standard.'
        # Se
      },
      exist_in_object = function(version = NULL, language = NULL){
        'Check if the alternativ exist in the object.'
        # Se
      },
      send_to_developers = function(version = NULL, language = NULL){
        'Print out as json object and constructing file and maintainer e-mail.'
        # Se
      }      
      )
  )        



api_parameters()
typeof(hej)