#' Build a path from character elements
#' 
#' This function takes a list of strings and builds a URL to the PX-web API \emph{in reverser order}.
#' 
#' @param varname A character string or a list of strings of the variable(s) in the web API. This can be a data node or a tree node.
#' @param topnodes A string or a list of strings containing the top nodes \emph{in top-to-bottom order}
#' @param baseURL The base URL to use, depending on the web service.
#' @param ... Further arguments passed to  \code{base_url()}.

buildPath <- function(varname, topnodes = NULL, baseURL, ...) {
	
	# Error handling
	if (!is.null(topnodes)) {
	   if (identical(topnodes, ""))
	      stop("ERROR: Internal function pxweb::buildPath: `topnodes` argument set to empty string\n
			 The `topnodes` argument is required to be either NULL or a value or a vector other than [''] interpretable as a character string by paste().\n")
	}
	   
	   # Clean URL string: remove trailing slash
	base <- str_replace_all(baseURL,"/$","")
	
	# Clean topnodes string: Remove whitespace and leading/trailing slashes
	topnodes <- str_trim(topnodes)
	topnodes <- str_replace_all(topnodes,"^/|/$","")
	
	# Build a vector, in the right order, for the URL elements
	urlElements <- paste(c(base,topnodes), collapse="/")
	
	# Create full path and return it
	return(
		paste(urlElements,varname, sep="/")
	)
}



#' Get content from response
#' 
#' Get the content from a response object
#' 
#' @param response response object
#' @param type type format
#' 
getContent <- function(response, type = "csv") {
    
    if (!class(response) == "response") {
        stop("needs to be an response class object")
    }
    
    # Convert to character
    content <- httr::content(response)
    
    if (type == "csv") {
        content <- read.table(
            textConnection(content), 
            sep = ",", 
            header = T, 
            stringsAsFactors = F
        )
    } else {
        stop("Unsupported type format")
    }
    
    return(content)
}

#' Return base URL to API
#' 
#' ...
#' 
#' @param database Source database ('sweSCB' or 'statfi')
#' @param version The version of SCB API to use. (Default: \code{v1})
#' @param lang The language (two letters) to use in the fetched data. (Default: \code{sv})
#' @param ... Additional parameters. These are currently ignored.
#' @export
#' @examples
#' a <- base_url("sweSCB", "v2", "en")
#' print(a)
#' 
base_url <- function(database, version = "v1", lang = "en", ...) {

  if (database == "sweSCB") {

    url <- paste(sprintf("http://api.scb.se/OV0104/%s/doris/%s/ssd",version,lang))

  } else if (database == "statfi") {

    url <- paste(sprintf("http://pxwebapi2.stat.fi/PXWeb/api/%s/%s/StatFin",version,lang))

  }
  return(url)
}




#' Return options for database, version and language choices
#' 
#' ...
#' 
#' @param ... Additional parameters; currently ignored
#' @export
#' @examples api_parameters()
api_parameters <- function(...) {
  api.file <- system.file("extdata/api.json", package = "pxweb")
  api.list <- RJSONIO::fromJSON(api.file)
  return(api.list)
}


               