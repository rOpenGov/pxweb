#' Get data dimensions form a bottom node in SCB API
#' 
#' Deparse and reorder data form the metadata object for pretty output
#' 
#' @param node Bottom node to deparse into object
#' @param verbose Verbose output mode.
#' @seealso \link{scbGetMetadata}
#' @export
#' @examples
#' 
#' 
#' ## CONTINUED FROM EXAMPLES IN scbGetMetadata()
#' # Get metadata for a named variable
#' url <- paste(c(baseURL(),"AM","AM0102","AM0102A","KLStabell14LpMan"), collapse="/")
#' metadata <- scbGetMetadata(url)
#' 
#' # Get dimensions (names of dimensions are printed in the terminal)
#' dims <- scbGetDims(metadata)
#' 
#' # Get data
#' test <- scbGetData(metadata$URL, dims=list(
#'    Myndighet = "C02",
#'    Kon = "*",
#'    Heltiddeltid = "*",
#'    ContentsCode = "*",
#'    Tid = "*"
#' ))
#' 

scbGetDims <- function(node, verbose=TRUE) {
	
	## Deparse metadata object into elements
	# Title
	title <- node$title$title
	
	vars <- node$variables$variables
	ndim <- length(vars)
	
	names <- sapply(vars, function(var,i) { var$code }, 1:ndim)
	
	if(verbose) {
		cat("Title: \n", title, "\n")
		cat("Names: \n", names, "\n")
	}
	
	return(vars)
}
