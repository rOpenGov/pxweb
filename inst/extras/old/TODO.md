## Advanced access (not tested yet - TODO)

### Exploring the top node of the API data tree
Data in the SCB API is structured in a data tree. To explore the top node of the data tree, use `scbGetMetadata()`. Provided with no parameters, the function fetches metadata from the top node in the API:
```r
topNode <- scbGetMetadata()
View(topNode)
```

### Traversing the node tree
The node tree can be ascended by adding the id of the next subnode to the URL of the base URL. This id is stored in the "id" column of the topNode object created above. The example below goes down the node tree one level at the third position in the "id" column:

```r
nextNode <- scbGetMetadata(topNode$id[3])
View(nextNode)
```
This can be repeated until we reach a node that references data instead of subnodes. Once you reach the bottom node, `scbGetMetadata` warns the user that the bottom node has been reached.

Note that `scbGetMetadata` actually takes the name of a node as input parameter, so if you know the name of a subnode in the node tree or a bottom (data) node, you can supply this directly to the function instead of explorin the data tree as we did above.

### Getting data dimensions
Next, we want to find the dimensions of the data at a particular bottom node, e.g. the node "KPIFastM" which is the bottom node in the following tree branch:

PR -> PR0101 -> PR0301B -> HMPIM07

The following code fetches dimensions for the variable `HMPIM07`:

```r
bottomNode <- scbGetMetadata("HMPIM07")
dims <- scbGetDims(bottomNode)
```

The first call, `scbGetMetadata("HMPIM07")`, returns a friendly message informing the user that this is a bottom node in the data tree. This call also provides us with a resulting object, `bottomNode`, which contains the URL to the node. We will recycle this when getting 'real' data from the database.

The second call, `scbGetDims(bottomNode)` also prints out a friendly message stating that the dimensions for the data at this node are "ContentsCode" and "Tid". We can now either pass on a wildcard ("*") to these dimensions, or a value, or a range of values on vector form.

To see what values are allowed for each dimension, have a look at the `dims` object using `print(dims)`. You will see that this is actually a list, and the values for each of the dimensions can be found by looking closer at each of the elements in the list by calling `dims[[n]]` where `n` is the number of the dimension you want to look at.

### Getting the data
The information we got above, i.e. the URL to the data node and the dimensions required for the call, can now be used to construct a call to the API to fetch the actual data:
```r
sdata <- scbGetData(bottomNode$URL, list(SPIN2007 = "*", ContentsCode = "PR0301I4", Tid = c("2010M02","2011M03")))
```

The data can now be inspected, e.g. by doing `View(sdata)`.


### Cleaning up the results
Unfortunately, the beta version of the SCB web API often returns faulty formatted data, which can cause a lot of pain. In version 0.3 of pxweb this can be handled directly by setting the parameter `clean = TRUE` in the `scbGetData()` function. So to get cleaned and molten data for the same call as in the previous example, just add `clean = TRUE`:
```r
sdata <- scbGetData(bottomNode$URL, 
  list(SPIN2007 = "*", ContentsCode = "PR0301I4", Tid = c("2010M02","2011M03")),
  clean=TRUE)
```
The data should now be ready for use. 

## Further examples
Further examples of advanced package usage are included in the "examples" folder installed with this package. To locate this folder on your system, run `system.file(package = "pxweb")` from the R terminal.

