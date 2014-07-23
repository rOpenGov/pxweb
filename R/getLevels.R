#' Get levels from API node
#'
#' Get levels from a node in the API. If at the lowest node, return a warning.
#' 
#' @param descriptions Whether to include node descriptions with the list of node IDs. (default: \code{FALSE})
#' @param quiet Quiet mode. Whether to stop with an error if the input node does not contain any subnodes. If set to \code{TRUE}, the function will quietly return FALSE without any errors. (default: \code{FALSE})
#' @param ... further argument to send to \code{scbGetMetadata}
#' @export

scbGetLevels <- function(
	descriptions = FALSE,
	quiet = FALSE,
	...
) {
	
	nodeData <- scbGetMetadata(quiet = TRUE, ...)
	
	if(!("id" %in% names(nodeData))) {
		if(!quiet) warning("already at lowest node, fetch data instead")
		if(quiet) return(FALSE)
	}
	
	if(!descriptions) {
		ids <- list(id=nodeData$id)
	} else {
		ids <- list(
		   id          = nodeData$id,
		   description = nodeData$text,
         URL         = nodeData$URL
		)
	}
	
	return(ids)
}

#' Function to silently test for existence of metadata and return TRUE or FALSE based on the result of that test.
#' 
#' @param url API url
#' @export
checkForLevels <- function(url) {
	
	if(missing(url))
		stop("ERROR: Function sweSCB::checkForLevels(): parameter `url` empty.\n
			 Please see traceback() for more information.")
	
	if(is.null(url)) {
		return(FALSE)
	}
	
	nodeData <- scbGetMetadata(url)
	if(!("id" %in% names(nodeData))) {
		return(FALSE)
	}
	
	# If the data passed both tests, return TRUE
	return(TRUE)
}

#' Function to deparse an URL into its components.
#' 
#' This function is currently not used in the package.
#' 
#' @param place Location in hierarchy, in the form of a (data-node complete) URL.
#' @param returnDistance Whether to return only the distance (in nodes) from top node to URL. (Default: \code{FALSE})
#' @param ... Further arguments passed to \code{baseURL()}.

deparseLevels <- function(place, returnDistance=FALSE, ...) {
	placeLevels <- str_split(
		str_replace(place, baseURL(...),""),
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
