.onAttach <- function(lib, pkg)
{

  # This may help with encodings in Mac/Linux
  # Sys.setlocale(locale = "UTF-8")
  # Sys.setlocale(locale = "WINDOWS-1252")

  packageStartupMessage("pxweb - R tools for PX-WEB API.\nCopyright (C) 2014 Leo Lahti, Mans Magnusson, Love Hansson\n\nhttps://ropengov.github.com/pxweb \n\n Hard sciences are successful because they deal with soft problems; \n soft sciences are struggling because they deal with hard problems.\n-                        Von Foerster\n")

}
