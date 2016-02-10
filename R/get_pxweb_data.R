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
#' \dontrun{
#' test_data <- 
#'   get_pxweb_data(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet", 
#'                  dims = list(ContentsCode = c('PR0101A1'), Tid = c('*')),
#'                  clean = FALSE)
#' }

get_pxweb_data <- function(url, dims, clean = FALSE) {

   dims <- reorder_and_check_dims(url, dims)
   dimNames <- names(dims)
   batches <- create_batch_list(url = url, dims = dims)
   content_node <- batches$content_node
   b_list <- list()
   
   for (batch_no in 1:length(batches$dims)){

     queryBody <- list()
     
     # print("Define the query list") 
     # batch_no <- i <- 1
     for (i in 1:length(batches$dims[[batch_no]])) {
        if (length(batches$dims[[batch_no]] [[dimNames[i]]]) == 1) {
           filter = ifelse(batches$dims[[batch_no]] [[dimNames[i]]] == "*", "all", "item")
        } else {
           filter = "item"
        }
        
        queryBody[[i]] <- list(
           code = dimNames[i],
           selection = list(filter = filter,
                            values = as.list(batches$dims[[batch_no]][[dimNames[i]]])
           ))
     }
     
     # print("Get data")
     api_timer(batches$url) 
     response <- try(httr::POST(
        url = batches$url,
        body = RJSONIO::toJSON(list(
           query = rapply(queryBody, enc2utf8, how = "replace"),
  	 # NOTE: JSON might be more efficient for downloads (smaller file size)
     # NOTE: JSON includes comments/metadata
           response = list(format = "csv")
  
        ))
     ), silent=TRUE)
     
     # print("Print error message")
     if (class(response)=="try-error"){
        stop(str_c("No internet connection to ",batches$url),
             call.=FALSE)
     }
     if(httr::http_error(response)) {
       stop(httr::http_status(response)$message,
            call.=FALSE)
     }
     
     # print("Parse data into human-readable form")
     # (Due to a weird encoding issue on Windows this generates a warning
     # about faulty encoding. Hence the suppressMessages() encapsulation...)
     suppressMessages(a <- content(response, as="text"))
     if(str_sub(a,1,1)==",") a <- str_sub(a,2,nchar(a)) # Correcting when the first element is , (few variables)
     b <- read.table(textConnection(a), sep=',', header=TRUE, stringsAsFactors=FALSE)
     head <- str_split(string=str_sub(a, start=1, end=str_locate(a,"\n")[[1]]),"\",\"")[[1]]
     head <- str_replace_all(string=head,pattern="\r|\n|\"","")
     colnames(b) <- head
     rm(a)
     
     # print(" Clean, melt and concatenate data ")
     # print(batch_no)
     if (batch_no == 2){
       time_used <- !all(batches$dim[[1]]$Tid == batches$dim[[2]]$Tid)
     }

     if (clean) {
       #message("Cleaning the data..")
       #save(b, head, batches, content_node, file = "tmp.RData")
       b <- clean_pxweb(data2clean=b, url=batches$url, dims = batches$dims[[batch_no]], content_node=content_node)
       content_node <- b[["content_node"]]
       if(batch_no == 1){
         res <- b[["data"]]
       } else {
         res <- rbind(res, b[["data"]])
       }
     } else { # If !clean
       if(batch_no == 1){
         res <- b
       } else if(time_used) {
         res <- cbind(res, b[,length(batches$dim[[1]]):ncol(b)])
       } else {
         res <- rbind(res, b)
       }
     } 
     # print("Give messages")
     if(length(batches$dims) > 2 & batch_no%%10 != 0) message(".", appendLF=FALSE)
     if(batch_no > 2 & batch_no%%10 == 0) message(" ",batch_no)
   } 

   if(length(batches$dims) > 2) message("\nDownload done.")

   return(res)
}



