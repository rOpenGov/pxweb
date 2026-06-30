# To reproduce the JSS manuscript PDF:
source("case-study.R")
tools::texi2pdf("article.tex", clean = FALSE)
