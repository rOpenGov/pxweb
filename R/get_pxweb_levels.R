#' Get levels from API node
#'
#' Get levels from a node in the API. If at the lowest node, return a warning.
#' 
#' @param baseURL The base URL to use, depending on the web service. 
#' @param descriptions Whether to include node descriptions with the list of node IDs. (default: \code{FALSE})
#' @param quiet Quiet mode. Whether to stop with an error if the input node does not contain any subnodes. If set to \code{TRUE}, the function will quietly return FALSE without any errors. (default: \code{FALSE})
#' @param ... further argument to send to \code{get_pxweb_metadata}
#' 
#' @keywords internal 
#' 
#' @export

get_pxweb_levels <- function(
	baseURL,
	descriptions = FALSE,
	quiet = FALSE,
	...
) {
	
	nodeData <- get_pxweb_metadata(baseURL = baseURL, quiet = TRUE, ...)
	
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

