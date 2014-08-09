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
#' \code{\link{scbGetMetadata}}, \code{\link{scbGetDims}}, \code{\link{scbGetLevels}}
#' 
#' @export
#' @examples
#' ## CONTINUED FROM EXAMPLES IN scbGetMetadata()
#' # Get metadata for a variable
#' baseURL <- base_url("sweSCB","v1","sv")
#' url <- paste(c(baseURL,"AM","AM0102","AM0102A","KLStabell14LpMan"), collapse="/")
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

scbGetData <- function(url, dims, clean = FALSE) {

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
   response <- try(POST(
      url = url,
      body = toJSON(list(
         query = queryBody,

	 # FIXME how about providing the format already through
	 # function arguments.  The user could then select between the
	 # supported formats (px, csv, json, xlsx, json-stat,
	 # sdmx). However this might be unnecessarily complex. For
	 # instance px format does not function properly in R, and
	 # xlsx access would not have much added value in R
	 # context. JSON might be more efficient for transfers
	 # (smaller file size?) than CSV, so we could consider that
	 # instead.
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
      b <- pxwebClean(data2clean=b, head=head, url=url)
   }
   
   return(b)
}



#' Clean raw data from PX-WEB
#' 
#' This function clean the raw data from PX-WEB to R tall format. 
#' 
#' @param data2clean Data to clean.
#' @param head Full variable names as character vector
#' @param url url to the bottom nod (to get meta data)
#' 
#' @return data frame melted and in R (numeric) format
#' 

pxwebClean <- function(data2clean, head, url) {  
  # Assertions
  stopifnot(ncol(data2clean) == length(head))
  stopifnot(class(data2clean) == "data.frame")
  stopifnot(class(head) == "character")
  stopifnot(class(url) == "character")
  
  # Convert to data table
  data2clean <- as.data.table(data2clean)
  
  # Get metadata to use in creating factors of Tid and contentCode
  contentNode <- scbGetMetadata(url)
  
  return(data2clean)

  # FIXME: Not working with generic APIs, only with sweSCB
  skip <- TRUE
  if (skip) {
 
  # Collect factor labels for tid and contentCode and convert
  # other variables to factor variables
  idvars <- character(0)
  for (content in contentNode$variables$variables) {
    if (content$code %in% c("Tid", "ContentsCode")) {
      if (content$code == "Tid") { valTextTid <- content$values }
      if (content$code == "ContentsCode") { valTextContentsCode <- content$values }
      next()
    }
    varName <- content$text
    Encoding(varName) <- "UTF-8"
    idvars <- c(idvars, varName)
  }
  notIdIndex <- !head%in%idvars

  # Remove , in variable titles (not compatible with data.table melt)
  idvars <- str_replace_all(idvars, pattern=",", ":")
  
  # Rename columns
  newhead <- head
  newhead[notIdIndex] <- 
    unlist(
      lapply(str_split(head[notIdIndex],pattern=" "),
             FUN=function(x) {
               if(length(x)>1) {
                paste(paste(x[1:(length(x)-1)],collapse=" "),"$",x[length(x)],sep="")
                      } else {x}})
    )
  newhead[!notIdIndex] <- idvars
  setnames(data2clean,old=names(data2clean), new=newhead)
  
  # Melt the data to long format 
  meltData <- data2clean[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), by = eval(idvars)]
  meltData <- as.data.frame(meltData)
  # Convert to factors
  for (idvar in idvars){
    meltData[,idvar] <- as.factor(meltData[,idvar])
  }

  # Add variables tid, tabellinneh\u00e5ll and v\u00e4rde
  epicSplit <- str_locate(meltData$variable,pattern="\\$")[,1]
  meltData[, "tid"] <- factor(str_sub(meltData$variable,start=epicSplit+1))
  meltData[, "tabellinneh\u00e5ll"] <- factor(str_sub(meltData$variable,end=epicSplit-1))
  meltData[, "v\u00e4rde"] <- suppressWarnings(as.numeric(str_replace_all(meltData$value,"\\s","")))
   
  # Remove variables wiyhout any use
  meltData$value <- NULL
  meltData$variable <- NULL
   
  return(meltData)
  }

}
