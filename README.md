
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

This package accompanies the paper:

*Using highest density intervals can reduce perceived uncertainty in
stock assessments*

by Andrew M. Edwards and Marie Auger-Méthé, *Fisheries Research*
285:107326. <https://doi.org/10.1016/j.fishres.2025.107326>

In the paper we show that using highest density intervals (HDIs) to
characterise uncertainty can be preferable to using the usual
equal-tailed intervals. The HDIs are shorter (reduce the perceived
uncertainty) and can lead to potential management implications by
changing perceptions of stock status.

This package provides functionality to calculate, understand, and plot
highest density intervals. The paper is a Short Communication (\<4.5
pages) and should be read first to understand the ideas and the
applications of the package (further technical details are in the
[Supplementary
Material](https://ars.els-cdn.com/content/image/1-s2.0-S0165783625000633-mmc1.pdf)).

Our package builds upon and adds functionality (e.g. our plotting
functions) to the popular HDInterval package by Meredith and Kruschke
(2022): HDInterval: Highest (Posterior) Density Intervals.
<https://cran.r-project.org/package=HDInterval>, for which the source
code is viewable at <https://github.com/mikemeredith/HDInterval>.

## Vignettes

Three vignettes for hdiAnalysis are designed to help users investigate
highest density intervals for their own applications. The vignettes
include examples of our simple plotting functions to help understand and
check calculated HDIs, plus code to streamline the calculation of HDIs
for time series of Markov chain Monte Carlo (MCMC) values and to produce
simple summary tables of results. All results in the manuscript are
reproducible in the vignettes.

The three vignettes are rendered and viewable on GitHub at:

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

## Citation

If you use `hdiAnalysis` in your work then please cite the paper, and
also the package as:

Edwards, A.M. and Auger-Méthé, M. (2025). hdiAnalysis: An R package for
calculating and plotting highest density intervals. R package version
1.0.0, <https://github.com/andrew-edwards/HDIanalysis>.

This will give us motivation for updating and maintaining it in the
future. `citation("hdiAnalysis")` to get a version for LaTeX and R
Markdown bibliographies.
<!-- Although the DOI badge at the top of this page ends in ...805,
the ...804 DOI is preferable because that will not change with any future
updates to the GitHub tag. So ...804 seems simpler and more consistent to use -->
<!-- (thanks!). -->

Also let us know of any applications. This will help us devote effort
into maintaining `hdiAnalysis`. Please report any bugs or suggestions as
[an Issue](https://github.com/andrew-edwards/hdiAnalysis/issues).

## Extra code

The
[talks/](https://github.com/andrew-edwards/hdiAnalysis/tree/dev-andy/talks)
folder contains `.Rmd` code, and rendered `.pdf` files, for
presentations.

The
[report/](https://github.com/andrew-edwards/hdiAnalysis/tree/dev-andy/report)
folder contains early exploratory investigations and analyses for other
examples not included in the manuscript, and are not intended for
package users (all the data are not pushed to GitHub so the code likely
can’t be run anyway).

## Installation

To install the latest version just:

    install.packages("remotes")    # If you do not already have the "remotes" package

    remotes::install_github("andrew-edwards/hdiAnalysis")

If you get an error like

    Error in utils::download.file(....)

then the connection may be timing out (this can especially happen on a
work network). Try

    options(timeout = 1200)

and then try and install again. If the R-CMD-check badge above has a
green “passing” component then the package has been independently tested
and installed via a GitHub Action, and so should install fine to your
local machine. If you encounter problems then either post an Issue or
contact <a href="mailto:andrew.edwards@dfo-mpo.gc.ca">Andy</a> for help.
