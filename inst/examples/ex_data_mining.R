## EXAMPLE: USING sweSCB TO MINE DATA FROM THE SCB API

## 1. Get metadata from the base node
a <- get_pxweb_metadata()

# View the data. We will recycle the values in the "id" column to delve deeper into the API node structure.
View(a)

# Go down one level into the node tree.
b <- get_pxweb_metadata(a$id[1])

# Do this for consecutive steps...
cc <- get_pxweb_metadata(b$id[5])

# ...until we reach a node with no nodes below it, only data
d <- get_pxweb_metadata(cc$id[3])

datanode <- get_pxweb_metadata(d$id[1])

# We receive a message that this is a bottom node. We can now use d$URL to get dimensions of the data:
dims  <- get_pxweb_dims(datanode)

# View dimension names
for(i in 1:length(dims)) {
   print(dims[[i]]$code)
}

# View dimension value space
for(i in 1:length(dims)) {
   print(dims[[i]]$code)
   print(dims[[i]]$values)
}

# Submit a query containing one or several values for each of the named dimensions
sdata <- get_pxweb(datanode$URL, list(ContentsCode = "AM0110D1", Utbildningsgrupp = "*", Kon = "*", Tid = "2012"))

# Create a simple bar plot with the resulting data
ggplot(sdata,aes(x=utbildningsgrupp.SUN.2000,y=Grundlön.2012,fill=kön)) + geom_bar(position=position_dodge()) + coord_flip()
