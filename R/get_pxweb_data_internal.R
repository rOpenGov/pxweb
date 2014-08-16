#' Clean raw data from PX-WEB
#' 
#' This function clean the raw data from PX-WEB to R tall format. 
#' 
#' @param data2clean Data to clean.
#' @param head Full variable names as character vector
#' @param url url to the bottom nod (to get meta data)
#' @param content_node node with content downloaded with \link{get_pxweb_metadata}. 
#' If NULL, meta data is downloaded with \link{get_pxweb_metadata}.
#' 
#' @return data frame melted and in R (numeric) format
#' 
#' 
#' 

clean_pxweb <- function(data2clean, head, url, content_node=NULL) {  

  # Assertions
  stopifnot(ncol(data2clean) == length(head))
  stopifnot(class(data2clean) == "data.frame")
  stopifnot(class(head) == "character")
  stopifnot(class(url) == "character")
  
  # Convert to data table
  data2clean <- as.data.table(data2clean)
  
  # Get metadata to use in creating factors of Tid and contentCode
  if(is.null(content_node)){
    contentNode <- get_pxweb_metadata(url)
  } else {
    contentNode <- content_node
  }
  
  # FIXME: cleaning not working with generic APIs, only with sweSCB
  if (!length(grep("scb", url)) == 1) {
    return(data2clean)
  } else {
 
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
   
  return(list(data=meltData, content_node = contentNode))
  }

}
