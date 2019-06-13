---
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, eval = TRUE)
library("ggplot2")
library("cowplot")
```

[![Build Status](https://travis-ci.org/pat-s/oddsratio.svg?branch=master)](https://travis-ci.org/pat-s/oddsratio)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version-ago/oddsratio)](https://cran.r-project.org/package=oddsratio)
[![cran checks](https://cranchecks.info/badges/worst/oddsratio)](https://cran.r-project.org/web/checks/check_results_oddsratio.html)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/oddsratio)](https://cran.rstudio.com/web/packages/oddsratio/index.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Dependencies](https://tinyverse.netlify.com/badge/oddsratio)](https://cran.r-project.org/package=oddsratio)

Functions for calculation and plotting of odds ratios of Generalized Additive (Mixed)
Models and Generalized Linear (Mixed) Models with a binomial
response variable (i.e. logistic regression models).  

## Installation

Install from CRAN:

```R
install.packages("oddsratio")
```

Get the development version from Github:

```R
remotes::install_github("pat-s/oddsratio")
```

## Usage

See the [Getting Started](https://pat-s.github.io/oddsratio/articles/oddsratio.html) vignette.
