library(pxweb)

# List available database options
api_parameters() 

# Custom query: 
dims <- list()
url <- "http://pxwebapi2.stat.fi/PXWeb/api/v1/fi/StatFin/asu/asas/010_asas_tau_101.px"
dims[["Alue"]] <- c("*")
dims[["Asuntokunnan koko"]] <- c("*")
dims[["Talotyyppi"]] <- c("S")
dims[["Vuosi"]] <- c("*")
clean <- TRUE

#d <- get_pxweb_data(url = url, dims = dims, clean = clean)

library(data.table)
library(plyr)
library(stringr)
library(RJSONIO)
library(httr)
fs <- list.files("../../R/", full.names = T)
for (f in fs) {source(f)}



d <- get_pxweb_data(url = url, dims = dims, clean = clean)


#Error in eval(expr, envir, enclos) : object 'Vuosi' not found
#In addition: Warning message:
#In newhead[!notIdIndex] <- idvars :
#  number of items to replace is not a multiple of replacement length
