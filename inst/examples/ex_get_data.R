# Build url
varname <- "BefProgFoddaMedel10"

# Get metadata
metadata <- scbGetMetadata(varname)

# Get dimensions (names of dimensions are printed in the terminal)
dims <- scbGetDims(metadata)

# Get data
test <- scbGetData(metadata$URL, dims=list(
	Fodelseland = "010",
	Alder="*",
	ContentsCode = "*",
	Tid="*"
))

# Examine data
View(test)

# Plot the data
ggplot(testClean,aes(x=ålder,y=Födda.2010,fill=födelseland)) + geom_histogram()
