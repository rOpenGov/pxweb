require(httr)
require(RJSONIO)
require(stringr)
require(data.table)
require(ggplot2)

##### EXAMPLE: HIERARCHY MINING #####
kMaxLevels = 4

kTakeSample = FALSE
kSamplesize = 10



# RUN --------------------------------------------------------------------------
## INIT: Define the data containers
hierarchy <- as.data.table(get_pxweb_levels(descriptions=TRUE))
setnames(
   hierarchy,
   names(hierarchy),
   paste(names(hierarchy), "_lv1", sep = "")
)
emptyRows <- list()
tableAtLevel <- list()
timeSeries <- data.table(obs=integer(), level=integer(), id=character(), time=numeric())
count <- 0

## Warn if kTakeSample is FALSE
if (!kTakeSample) {
	cat("\n\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
	cat("WARNING: Fetching data ALL NODES in the SCB API.\n")
	cat("This might take a VERY LONG TIME!\n")
	cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n\n")
	
}

## Run loop
for (j in 2:kMaxLevels) {
	
	# Take a sample of Hierarchy to make it manageable
	if (kTakeSample) {
	hierarchy <- hierarchy[
		sample(
			nrow(hierarchy),
			min(nrow(hierarchy),kSamplesize*(j-1)))
		]
	}
	
	# Define the sublevel at which we will be looking
	idLevel <- j
	
	cat("*******************************************\n")
	cat("Getting data for level", idLevel,"\n")
	cat("*******************************************\n\n")
	
	# Clear input data
	subLevelData <- data.table(
      topId       = character(),
      bottomId    = character(),
      description = character(),
      URL         = character()
	)
	
	topLevelName <- paste0("id_lv", idLevel-1)
	topURLName <- paste0("URL_lv", idLevel-1)
	bottomLevelName <- paste0("id_lv", idLevel)
	bottomURLName <- paste0("URL_lv", idLevel)
	descName <- paste0("description_lv", idLevel)
	
	for (i in 1:nrow(hierarchy)) {
		count <- count + 1
		nodeURL <- hierarchy[i, topURLName, with=FALSE][[1]]
      nodeId <- hierarchy[i, topLevelName, with=FALSE][[1]]
		if (nodeId != "") {
			tid <- system.time({
				
				## QUICK FIX FOR BUGS IN THE SCB API:
				if (!nodeId %in% c("BE0001T04BAr")) {
					
					# Trim levels (some levels are returned with trailing whitespace)
					queryLevels <- get_pxweb_levels(path = nodeURL, descriptions=TRUE, quiet=FALSE)
				} else {
					queryLevels <- NULL
				}
				
				if (!is.null(queryLevels$id)) {
					queryData <- data.table(
						topId       = nodeId,
						bottomId    = as.character(queryLevels$id),
						description = as.character(queryLevels$description),
                  URL         = queryLevels$URL
					)
				} else {
					queryData <- data.table(
						topId       = nodeId,
						bottomId    = "",
						description = "",
                  URL         = ""
					)
				}
				
				cat("Level",j-1, "value: ",nodeId,"\n")
				cat("Level",j  , "values:",queryData$bottomId,"\n")
				
				subLevelData <- rbind(subLevelData,queryData)
			})
		} else {
			# Make sure the timestamp from the previous query doesn't cause us
			# to post the next query too soon
			tid <- c(0,0,0)
		}
		
		# Create time series object for analysis purposes
		timeSeries <- rbind(timeSeries,
							list(obs  = count,
								 level = j,
								 id    = nodeId,
								 time  = tid[[3]]))
		cat("Time taken for query:", tid[[3]],"\n\n")
		
		# Ensure we don't overdo our query limit (1/sec) by adding
		# 0.1-1.2 seconds sleep time, depending on past query processing time 
		if(tid[[3]] != 0) { Sys.sleep(1.2-min(tid[[3]],1.1)) }
	}
	
	setnames(
	   subLevelData,
	   names(subLevelData),
	   c(topLevelName, bottomLevelName, descName, bottomURLName)
	)
	
	hierarchy <- merge(hierarchy,subLevelData,
	                   by = paste0(topLevelName),
	                   all.x = TRUE)
	
# 	emptyRows[[j]] <- hierarchy[get(bottomLevelName) == ""]
	tableAtLevel[[j]] <- hierarchy
	
	if(all(hierarchy[,bottomLevelName,with=FALSE] == "") | nrow(hierarchy) == 0) {
		stop("Reached bottom node in all data tree paths. Stopping.")
	}
	
# 	hierarchy <- hierarchy[get(bottomLevelName) != ""]
}

# Order columns
hierarchy <- hierarchy[,list(
	id_lv1,
	description_lv1,
   URL_lv1,
	id_lv2,
	description_lv2,
	URL_lv2,
	id_lv3,
	description_lv3,
	URL_lv3,
	id_lv4,
	description_lv4,
	URL_lv4
)]

hierarchy_bk <- copy(hierarchy)
save(hierarchy,file="hierarchy.RData")

View(hierarchy)