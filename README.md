
<!-- README.md is generated from README.Rmd. Please edit that file -->

# labelR - Pairwise Data Labeling on Mechanical Turk

<!-- badges: start -->

<!-- badges: end -->

labelR is a package that assists users in conducting data labeling with
pairwise comparisons on Amazon’s Mechanical Turk (MTurk). This package
replicates and extends the functionality of the sentimentIt package (now
deprecated) to interface directly with MTurk through R, rather than
through the sentimentIt servers.

## Installation

<!--
You can install the released version of labelR from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("labelR")
```
-->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RydenButler/labelR")
```

## Tutorial

This tutorial provides a complete walkthrough of how to use labelR to
conduct pairwise data labeling on MTurk. It begins by demonstrating how
to create a worker qualification and corresponding qualification test
that will permit only authorized workers to complete your assignments
(MTurk documentation will refer to these as “human intelligence tasks”
or “HITs”). Next, it demos how to format a HIT template and pass the
formatting to MTurk to use with your assignments. Finally, it shows how
to send assignment to MTurk for labeling, retrieve the labeled results,
and check worker quality.

``` r
#library(labelR)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
#summary(cars)
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
