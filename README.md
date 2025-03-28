
<!-- README.md is generated from README.Rmd. Please edit that file.
Build with

# load_all()
rmarkdown::render("README.Rmd")

which builds the .html that can be viewed locally (but isn't pushed to GitHub;
GitHub uses README.md to make the page you see on GitHub). See pacea if want to
save figures.
-->

# hdiAnalysis

<!-- badges: start -->

[![R-CMD-check](https://github.com/andrew-edwards/hdiAnalysis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/andrew-edwards/hdiAnalysis/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/andrew-edwards/hdiAnalysis/branch/main/graph/badge.svg)](https://app.codecov.io/gh/andrew-edwards/hdiAnalysis?branch=main)
[![DOI](https://zenodo.org/badge/781686083.svg)](https://doi.org/10.5281/zenodo.14969069)
![Visitors](https://api.visitorbadge.io/api/visitors?path=https%3A%2F%2Fgithub.com%2Fandrew-edwards%2FhdiAnalysis&label=VISITORS&countColor=%23263759&style=flat&labelStyle=lower)
<!-- badges: end -->

An R package for calculating and plotting highest density intervals

This package accompanies the manuscript:

*Using highest density intervals can reduce perceived uncertainty in
stock assessments*

by Andrew M. Edwards and Marie Auger-Méthé, submitted to *Fisheries
Research*.

In the manuscript (available upon request) we show that using highest
density intervals (HDIs) to characterise uncertainty can be preferable
to using the usual equal-tailed intervals. The HDIs are shorter (less
uncertainty) and can also lead to unanticipated conservation
implications by changing perceptions of stock status.

This package provides functionality to calculate, understand, and plot
highest density intervals. The manuscript should be read first to
understand the potential of the package. Three rendered vignettes are
designed to help users investigate highest density intervals for their
own applications. The vignettes include examples of our simple plotting
functions to help understand and check calculated HDIs, plus code to
streamline the calculation of HDIs for time series of (Markov chain
Monte Carlo) MCMC values and to produce simple summary tables of
results. All results in the manuscript are reproducible in the
vignettes.

The three vignettes are rendered and viewable on the GitHub site, and
are:

-   [results.html](http://htmlpreview.github.io/?https://github.com/andrew-edwards/hdiAnalysis/blob/main/vignettes/results.html)
    – Designed as a template for users to analyse their own data, by
    reproducing the main Pacific Hake results in the manuscript as an
    example.
-   [results-extra.html](http://htmlpreview.github.io/?https://github.com/andrew-edwards/hdiAnalysis/blob/main/vignettes/results-extra.html)
    – Includes further results, calculations, explanations, and figure
    options.
-   [results-herring-petrale-cod.html](http://htmlpreview.github.io/?https://github.com/andrew-edwards/hdiAnalysis/blob/main/vignettes/results-herring-petrale-cod.html)
    – Calculations for three further stocks, Pacific Herring, Petrale
    Sole, and Pacific Cod.

To run and adapt the code yourself, simply download the raw R Markdown
files from
[results.Rmd](https://github.com/andrew-edwards/hdiAnalysis/blob/main/vignettes/results.Rmd),
[results-extra.Rmd](https://github.com/andrew-edwards/hdiAnalysis/blob/main/vignettes/results-extra.Rmd),
or
[results-herring-petrale-cod.Rmd](https://github.com/andrew-edwards/hdiAnalysis/blob/main/vignettes/results-herring-petrale-cod.Rmd).
Run the file locally, and then adapt it for your own data.

## Installation

To install the latest version just:

    install.packages("remotes")    # If you do not already have the "remotes" package

    remotes::install_github("andrew-edwards/hdiAnalysis")

If you get an error like

    Error in utils::download.file(....)

then the connection may be timing out (this happens to us on the DFO
network). Try

    options(timeout = 1200)

and then try and install again. If you get a different error then post
an Issue or contact
<a href="mailto:andrew.edwards@dfo-mpo.gc.ca">Andy</a> for help.

## Citation

If you use `hdiAnalysis` in your work then please cite the manuscript,
and also the package as:

Edwards, A.M. and Auger-Méthé, M. (2025). hdiAnalysis: An R package for
calculating and plotting highest density intervals. R package version
1.0.0, <https://github.com/andrew-edwards/HDIanalysis>.

This will give us motivation for updating and maintaining it in the
future. `citation("hdiAnalysis")` to get a version for LaTeX and R
Markdown bibliographies. We will add a DOI if the manuscript is
accepted.
<!-- Although the DOI badge at the top of this page ends in ...805,
the ...804 DOI is preferable because that will not change with any future
updates to the GitHub tag. So ...804 seems simpler and more consistent to use -->
<!-- (thanks!). -->

Also let us know of any applications. This will help us devote effort
into maintaining `hdiAnalysis`.

The `report` folder contains early exploratory investigations and
analyses for other examples not included in the manuscript, and are not
intended for packages users (all the data are not pushed to GitHub so
the code likely can’t be run anyway).
