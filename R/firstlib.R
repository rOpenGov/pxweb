.onAttach <- function(lib, pkg)
{

  # This may help with encodings in Mac/Linux
  # Sys.setlocale(locale = "UTF-8")
  # Sys.setlocale(locale = "WINDOWS-1252")

  packageStartupMessage("pxweb: R tools for PX-WEB API.\nCopyright (C) 2014 Mans Magnusson, Leo Lahti, Love Hansson\nhttps://github.com/ropengov/pxweb\n")
  packageStartupMessage(check_new_pxweb_apis())
}
