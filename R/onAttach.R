.onAttach <- function(lib, pkg) {
  packageStartupMessage(paste0("pxweb ", utils::packageVersion("pxweb"), ": R Interface to PXWEB APIs.\nhttps://github.com/ropengov/pxweb\n"))
}
