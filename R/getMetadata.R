#' Get data
#' 
#' Get data from the API. If at the lowest node, provide the user with a friendly message about this.
#' 
#' @param path URL to fetch metadata from. If left empty, the function constructs a URL from the \code{node} and \code{topnodes} arguments
#' @param node A string with the name of the node to fetch. This is ignored if \code{path} is supplied.
#' @param topnodes A string or list of strings with the names of the parent nodes of \code{node}. This is ignored if \code{path} is supplied.
#' @param quiet Quiet mode (never return a message to the user)
#' @param ... Further arguments passed to  \code{baseURL()}.
#' @export
#' @examples
#' # Define variable name
#' topnode <- scbGetMetadata()
#' 
#' # Get metadata for the first element in the top node
#' nextnode <- scbGetMetadata(topnode$URL[1])
#' 
#' # Get metadata for a named node with named topnodes
#' a_node <- scbGetMetadata()
#' 

scbGetMetadata <- function(path = NULL, node = NULL, topnodes = NULL, quiet = TRUE, ...) {

   # path = NULL; node = NULL; topnodes = NULL; quiet = TRUE

   # Build a URL if no path is supplied
   if (is.null(path)) {
      if (is.null(node)) {
         url <- baseURL(...)
      } else {
         url <- buildPath(node, topnodes)
      }
   } else {
      # If a path is supplied, build a URL from that path and ignore the node and topnodes arguments
      url <- path
   }
      
      df <- try(
         data.frame(
         t(sapply(
            RJSONIO::fromJSON(
               paste(readLines(url, warn = F), collapse = ""),
               encoding = "utf8"
            ),
            c
         )),
         stringsAsFactors = FALSE
      ), silent=TRUE
   )
   
   if (class(df)=="try-error") {
      stop(str_join("No internet connection to ", url),
           call.=FALSE)
   }
   
   if ("id" %in% names(df)) {
      # Convert id to character
      df$id <- as.character(df$id)
      
      # Set the URL of each subnode
      df$URL <- buildPath(df$id, baseUrl = url)
      
   } else {
      
      df$URL <- url
      if (!quiet) {
         message("The data node returned is a bottom node.\nIf the name of your node object is `node`, call scbGetDims(node$URL) to get further information about the data node.")
      }
   }
   
   
   return(df)
}
