#' Get data dimensions form a bottom node in a PX-Web/PC-Axis API
#' 
#' Deparse and reorder data form the metadata object for pretty output
#' 
#' @param node Bottom node to deparse into object
#' @param verbose Verbose output mode.
#' 
#' @keywords internal 
#' 
#' @seealso \link{get_pxweb_metadata}
#' @export
#' @examples
#' \dontrun{
#' # Download meta data
#' bottom_node <- get_pxweb_metadata("http://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0114/LCIArbKv")
#' 
#' # Get dimensions (names of dimensions are printed in the terminal)
#' dims <- get_pxweb_dims(bottom_node)
#' }
get_pxweb_dims <- function(node, verbose=TRUE) {
	
	## Deparse metadata object into elements
	# Title
	title <- node$title$title
	
	vars <- node$variables$variables
	ndim <- length(vars)
	
	names(vars) <- sapply(vars, function(var,i) { var$code }, 1:ndim)
	
	if(verbose) {
	  message(paste("Title: \n", title, "\n"))
	  message(paste("Names: \n", names(vars), "\n"))
	}
	
	return(vars)
}
