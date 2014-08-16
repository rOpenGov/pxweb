#' Get data
#' 
#' Get data from the API. If at the lowest node, provide the user with a friendly message about this.
#' 
#' @param path URL to fetch metadata from. If left empty, the function constructs a URL from the \code{node} and \code{topnodes} arguments
#' @param node A string with the name of the node to fetch. This is ignored if \code{path} is supplied.
#' @param topnodes A string or list of strings with the names of the parent nodes of \code{node}. This is ignored if \code{path} is supplied.
#' @param quiet Quiet mode (never return a message to the user)
#' @param baseURL The base URL to use, depending on the web service. Needed if path argument is not provided.
#' @param ... Further arguments passed to  \code{base_url()}.
#' @export
#' @examples
#' # Define variable name
#' api_parameters() # List options
#' baseURL <- base_url("sweSCB", "v1", "sv")
#' topnode <- get_pxweb_metadata(baseURL = baseURL)
#' 
#' # Get metadata for the first element in the top node
#' nextnode <- get_pxweb_metadata(topnode$URL[1])

get_pxweb_metadata <- function(path = NULL, node = NULL, topnodes = NULL, quiet = TRUE, baseURL = NULL, ...) {

   # Build a URL if no path is supplied
   if (is.null(path)) {

      if (is.null(baseURL)) {
        stop("Error in pxweb::get_pxweb_metadata: provide the baseURL argument!")
      }

      if (is.null(node)) {
	 url <- baseURL
      } else {
         url <- buildPath(node, topnodes, baseURL)
      }

   } else {
      # If a path is supplied, build a URL from that path and ignore the node and topnodes arguments
      url <- path
   }
      
   api_timer(url)
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
      df$URL <- buildPath(df$id, baseURL = url)
      
   } else {
      
      df$URL <- url
      if (!quiet) {
         message("The data node returned is a bottom node.\nIf the name of your node object is `node`, call get_pxweb_dims(node$URL) to get further information about the data node.")
      }
   }
      
   return(df)
}
