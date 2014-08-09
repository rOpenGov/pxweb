library(pxweb)

# List available database options
api_parameters() 

# Standard query that asks for options
baseURL <- base_url("statfi", "v1", "fi")
#d <- findData(baseURL)

# Custom query: 
dims <- list()
data.url <- "http://pxwebapi2.stat.fi/PXWeb/api/v1/fi/StatFin/asu/asas/010_asas_tau_101.px"
dims[["Alue"]] <- c("*"))
dims[["Asuntokunnan koko"]] <- c("*")
dims[["Talotyyppi"]] <- c("S")
dims[["Vuosi"]] <- c("*")
myDataSetName <- scbGetData(url = data.url, dims = dims, clean = TRUE)

