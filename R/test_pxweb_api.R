#' Test a full or a part of an pxweb api.
#' 
#' @description
#' The function can be used to test a whole pxweb api by using the api base url.
#' By using a branch in a tree the api is tested below this branch.
#' 
#' @param url The base url to the pxweb api (or a branch of the metadata tree)
#' @param download_all Should all data be downloaded (TRUE) or sample (FALSE)
#' @param seed Seed to use if download_all is FALSE and a sample is downloaded
#' 
#' @return 
#' Function returns a data.frame with information on each node
#' Two variables are added: 
#' \code{checked} : The node has been checked
#' \code{error} : Whether there were errors encountered with the call
#' \code{download_error} : Whether there were errors encountered during download
#' 
#' @examples
#' \dontrun{
#' test_data <- test_pxweb_api(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/TK")
#' }
#' @export
#' @importFrom plyr rbind.fill
#'  
test_pxweb_api <- function(url, download_all=FALSE, seed=as.integer(Sys.time())){

  nodes <- test_pxweb_api_get_nodes(url=url)
  message("\n")
  nodes_list <- test_pxweb_api_get_node_metadata(nodes=nodes)
  message("\n")
  res <- test_pxweb_api_get_data(nodes=nodes, seed=seed,
                                 nodesList=nodes_list, 
                                 download_all=download_all)
  res_data <- rbind.fill(nodes[nodes$type == "l", ], res$data)
  return(list(data=res_data, calls=res$calls))
}