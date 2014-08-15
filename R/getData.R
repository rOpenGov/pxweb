#' Get data from a bottom node in PX-WEB API
#' 
#' This function fetches actual data (i.e. values)
#' 
#' @param url URL to get data from (usually sufficient to submit the base URL, supplied via the base_url() function, and the name of the variable).
#' @param dims A list of dimensional parameters to filter data by. Note that values \emph{must} be submitted for all dimensions of the data. If you don't want to filter data, submit an asterisk in quotation marks ("*") instead of values for that dimension.
#' @param clean Clean and melt the data to R format.
#' 
#' @details
#' There are five documented filter types in the PX-WEB API documentation; "Item", "All", "Top", "Agg" and "Vs". This function currently only supports the "Item" and "All" modes. 
#' To use "Item" selection, simply submit a value or vector of values with each dimensional parameter. To use "All" selection, submit a wildcard asterisk ("*") instead of a value.
#' For detailed examples see the installed example files in the \code{examples} folder of \code{path.package("pxweb")} (these are also viewable on the project's GitHub page).
#' 
#' @seealso
#' \code{\link{get_pxweb_metadata}}, \code{\link{get_pxweb_dims}}, \code{\link{get_pxweb_levels}}
#' 
#' @export
#' @examples
#' ## CONTINUED FROM EXAMPLES IN get_pxweb_metadata()
#' # Get metadata for a variable
#' baseURL <- base_url("sweSCB","v1","sv")
#' url <- paste(c(baseURL,"AM","AM0102","AM0102A","KLStabell14LpMan"), collapse="/")
#' metadata <- get_pxweb_metadata(url)
#' 
#' # Get dimensions (names of dimensions are printed in the terminal)
#' dims <- get_pxweb_dims(metadata)
#' 
#' # Get data
#' test <- get_pxweb_data(metadata$URL, dims=list(
#'    Myndighet = "C02",
#'    Kon = "*",
#'    Heltiddeltid = "*",
#'    ContentsCode = "*",
#'    Tid = "*"
#' ))
#' 

get_pxweb_data <- function(url, dims, clean = FALSE) {

   dimNames <- names(dims)
   
   queryBody <- list()
   
   # Define the query list
   for (i in 1:length(dims)) {
      if (length(dims[[dimNames[i]]]) == 1) {
         filter = ifelse(dims[[dimNames[i]]] == "*", "all", "item")
      } else {
         filter = "item"
      }
      
      queryBody[[i]] <- list(
         code = dimNames[i],
         selection = list(filter = filter,
                          values = as.list(dims[[dimNames[i]]])
         ))
   }
   
   # Get data
   api_timer(url, calls=2)
   response <- try(POST(
      url = url,
      body = toJSON(list(
         query = queryBody,
	 # NOTE: JSON might be more efficient for downloads (smaller file size)
         response = list(format = "csv")

      ))
   ), silent=TRUE)
   
   # Print error message
   if (class(response)=="try-error"){
      stop(str_join("No internet connection to ",url),
           call.=FALSE)
   }
   if(httr::http_status(response)$category != "success") {
     stop(httr::http_status(response)$message,
          call.=FALSE)
   }
   
   # Parse data into human-readable form
   # (Due to a weird encoding issue on Windows this generates a warning
   # about faulty encoding. Hence the suppressMessages() encapsulation...)
   suppressMessages(a <- content(response, as="text"))
   if(str_sub(a,1,1)==",") a <- str_sub(a,2,nchar(a)) # Correcting when the first element is , (few variables)
   b <- read.table(textConnection(a), sep=',', header=TRUE, stringsAsFactors=FALSE)
   head <- str_split(string=str_sub(a, start=1, end=str_locate(a,"\n")[[1]]),"\",\"")[[1]]
   head <- str_replace_all(string=head,pattern="\r|\n|\"","")
   rm(a)
     
   # Clean and melt data 
   if (clean) {
     b <- clean_pxweb(data2clean=b, head=head, url=url)
   }
   
   return(b)
}



