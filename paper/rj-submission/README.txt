Replication for Opening Up Official Statistics in R with pxweb

Install pxweb 0.19.2 or later and knitr (see _Rpackages.txt). Run this command from this folder:

    Rscript --vanilla paperRJ.R

The entry point uses the helper scripts in scripts/. It runs the manuscript examples and regenerates figs/workflow-model.pdf and figs/case-study-population.pdf. The workflow figure uses Graphviz if available and has a base R fallback. The cached Statistics Sweden TAB638 inputs in data/ include the search results, metadata, a small API response, and the full case-study dataset, comments, and citation.

The default run uses these snapshots without contacting the API. To retrieve the current data and replace the snapshots, set PXWEB_PAPER_REFRESH_DATA=true before running the script. Results and access dates can change on refresh.

To build the manuscript, make the official RJournal.sty available on your LaTeX search path and run this command from this folder:

    Rscript --vanilla -e 'tools::texi2pdf("RJwrapper.tex", clean = TRUE)'

From the staged folder in the repository, this command uses the style in the parent directory on macOS or Linux:

    TEXINPUTS=..: Rscript --vanilla -e 'tools::texi2pdf("RJwrapper.tex", clean = TRUE)'

The R Journal supplies RJournal.sty in its LaTeX template:
https://github.com/rjournal/share/raw/refs/heads/master/RJtemplate.zip
