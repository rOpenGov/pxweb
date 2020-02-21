# Troubleshooting pxweb R package

## Error: Too large query 

### Date added: 
2019-02-15

### Versions
This problem exist in pxweb version 0.8+.

### Description
In some situation (some PXWEB APIS) it is possible to get an `Error: Too large query` error message (can look a little bit different for different pxweb versions) if a large query is done. The reason for this problem is that the pxweb R package cannot split the query into chunks/batches with a size smaller than the maximum values limit for the API.

All PXWEB APIs table objects have dimension and/or content variables. Content variables are only one variable and indicate what contents to download (such as the number of inhabitants and the percentage change in inhabitants). It is not always so that content variables exist in a PXWEB table. To split up a large query, only dimension variables can be split into smaller chunks (they are the rows in the PX-DATA object). So the pxweb R package tries to identify what variables that are dimension variables by checking if the variable can be eliminated or if the variable is the time variable. So if a variable is not time and cannot be eliminated it is not possible to be sure that the variable is not a content variable, and then the variable cannot be used to split the query.

This will hopefully be solved by the PXWEB API provider supplying information if the variable is a content variable or not in future releases.

### Workaround
The only way to solve this currently is to manually specify variables that the pxweb package can use to split up the query by setting the variables elimination to `TRUE` that are not content variables. Below is a working example of a workaround.

```
url <- "http://px.hagstofa.is/pxis/api/v1/is/Efnahagur/utanrikisverslun/1_voruvidskipti/03_inntollskra/UTA03801.px"
pxweb_query_list <- list("Tollskrárnúmer" = c("*"),
                         "Land" = c("AF", "AL"),           
                         "Mánuður" = c("2016M01"),
                         "Eining" = c("*"))
pxq <- pxweb_query(pxweb_query_list)
px_data <- pxweb_get(url, pxq)
Error: Too large query

# Variable(s) Tollskrárnúmer, Eining cannot be split to batches (eliminate is FALSE).


# We download the metadata object and set eliminate to TRUE (since Eining)
pxmd <- pxweb_get(url)
pxmd$variables[[4]]$elimination <- TRUE
# We explicitly supply the meta data object to use for downloading
px_data <- pxweb_advanced_get(px, pxq, pxmdo = pxmd)

Downloading large query (in 4 batches):
  |======================================================================================| 100%

px_data

PXWEB DATA
With 5 variables and 35792 observations.
```



