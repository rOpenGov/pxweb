#' Functions from sweSCB package that are not called anywhere (yet)
#' but might be useful later

#' Function to deparse an URL into its components.
#' 
#' This function is currently not used in the package.
#' 
#' @param place Location in hierarchy, in the form of a (data-node complete) URL.
#' @param returnDistance Whether to return only the distance (in nodes) from top node to URL. (Default: \code{FALSE})
#' @param baseURL The base URL to use, depending on the web service. 
#' @param ... Further arguments passed to \code{base_url()}.
#' 
#' @keywords internal 
#' 

deparseLevels <- function(place, returnDistance=FALSE, baseURL, ...) {
	placeLevels <- str_split(
		str_replace(place, baseURL, ""),
		"/"
	)
	
	# Remove empty elements created by str_split (caused by leadning and/or 
	# trailing slashes)
	placeLevels <- placeLevels[[1]][sapply(placeLevels, str_length) > 0]
		
	if(returnDistance) {
		levelsToTop <- length(placeLevels)		
		return(levelsToTop)
	}
	
	return(placeLevels)
}

#' Function to silently test for existence of metadata and return TRUE or FALSE based on the result of that test.
#' 
#' @keywords internal 
#' 
#' @param url API url
#' @export
checkForLevels <- function(url) {
	
	if(missing(url))
		stop("ERROR: Function pxweb::checkForLevels(): parameter `url` empty.\n
			 Please see traceback() for more information.")
	
	if(is.null(url)) {
		return(FALSE)
	}
	
	nodeData <- get_pxweb_metadata(url)
	if(!("id" %in% names(nodeData))) {
		return(FALSE)
	}
	
	# If the data passed both tests, return TRUE
	return(TRUE)
}


