#' Get data dimensions form a bottom node in SCB API
#' 
#' Deparse and reorder data form the metadata object for pretty output
#' 
#' @param node Bottom node to deparse into object
#' @param verbose Verbose output mode.
#' @seealso \link{get_pxweb_metadata}
#' @export
#' @examples
#' 
#' 
#' ## CONTINUED FROM EXAMPLES IN get_pxweb_metadata()
#' # Get metadata for a named variable
#' baseURL <- base_url("sweSCB", "v1", "sv")
#' url <- paste(c(baseURL,"AM","AM0102","AM0102A","KLStabell14LpMan"), collapse="/")
#' metadata <- get_pxweb_metadata(url)
#' 
#' # Get dimensions (names of dimensions are printed in the terminal)
#' dims <- get_pxweb_dims(metadata)
#' 
#' # Get data
#' test <- get_pxweb(metadata$URL, dims=list(
#'    Myndighet = "C02",
#'    Kon = "*",
#'    Heltiddeltid = "*",
#'    ContentsCode = "*",
#'    Tid = "*"
#' ))
#' 
get_pxweb_dims <- function(node, verbose=TRUE) {
	
	## Deparse metadata object into elements
	# Title
	title <- node$title$title
	
	vars <- node$variables$variables
	ndim <- length(vars)
	
	names <- sapply(vars, function(var,i) { var$code }, 1:ndim)
	
	if(verbose) {
	  message(paste("Title: \n", title, "\n"))
	  message(paste("Names: \n", names, "\n"))
	}
	
	return(vars)
}
