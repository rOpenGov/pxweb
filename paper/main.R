# To reproduce the JSS manuscript PDF:
source("case-study.R")
source("workflow-figure.R")
tools::texi2pdf("article.tex", clean = FALSE)
