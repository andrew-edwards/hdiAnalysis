
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
![Visitors](https://api.visitorbadge.io/api/visitors?path=https%3A%2F%2Fgithub.com%2Fandrew-edwards%2FhdiAnalysis&label=VISITORS&countColor=%23263759&style=flat&labelStyle=lower)
<!-- badges: end -->

An R package for calculating and plotting highest density intervals

This package accompanies the manuscript:

*Using highest density intervals can reduce uncertainty in stock
assessments by billions of fish*

by Andrew M. Edwards and Marie Auger-Méthé, being submitted to
*Fisheries Research*.

In the manuscript (available upon request) we show that using highest
density intervals (HDIs) to characterise uncertainty is preferable to
using the usual equal-tailed intervals. The HDIs are shorter (less
uncertainty) and also lead to unanticipated conservation implications in
our example application.

This package provides functionality to calculate, understand, and plot
highest density intervals. The manuscript should be read first to
understand the potential of the package. Two rendered vignettes are
designed to help users investigate highest density intervals for their
own applications. The vignettes include examples of our simple plotting
functions to help understand and check calculated HDIs, plus code to
streamline the calculation of HDIs for time series of (Markov chain
Monte Carlo) MCMC values and to produce simple summary tables of
results.

The two vignettes are rendered and viewable on the GitHub site, and are:

-   [results.html](http://htmlpreview.github.io/?https://github.com/andrew-edwards/hdiAnalysis/blob/main/vignettes/results.html)
    – Designed as a template for users to analyse their own data, by
    reproducing the results in the manuscript as an example.
-   [results-extra.html](http://htmlpreview.github.io/?https://github.com/andrew-edwards/hdiAnalysis/blob/main/vignettes/results-extra.html)
    – Includes further results, calculations, explanations, and figure
    options.

To run and adapt the code yourself, simply download the raw R Markdown
files from
[results.Rmd](https://github.com/andrew-edwards/hdiAnalysis/blob/main/vignettes/results.Rmd)
or
[results-extra.Rmd](https://github.com/andrew-edwards/hdiAnalysis/blob/main/vignettes/results-extra.Rmd).
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

The `report` folder contains early exploratory investigations and
analyses for other examples not included in the manuscript, and are not
intended for packages users (all the data are not pushed to GitHub so
the code likely can’t be run anyway).
