.onAttach <- function(lib, pkg)
{

  packageStartupMessage(paste0("pxweb ", utils::packageVersion("pxweb"), ": R tools for the PX-WEB API.\nhttps://github.com/ropengov/pxweb\n"))

}
