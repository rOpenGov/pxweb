# Build url
varname <- "BefProgFoddaMedel10"

# Get metadata
metadata <- get_pxweb_metadata(varname)

# Get dimensions (names of dimensions are printed in the terminal)
dims <- get_pxweb_dims(metadata)

# Get data
test <- get_pxweb(metadata$URL, dims=list(
	Fodelseland = "010",
	Alder="*",
	ContentsCode = "*",
	Tid="*"
))

# Examine data
View(test)

# Plot the data
ggplot(testClean,aes(x=ålder,y=Födda.2010,fill=födelseland)) + geom_histogram()
