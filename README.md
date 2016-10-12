
[![Build Status](https://travis-ci.org/pat-s/oddsratio.svg?branch=master)](https://travis-ci.org/pat-s/oddsratio) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/oddsratio)](http://cran.r-project.org/package=oddsratio) [![Downloads](http://cranlogs.r-pkg.org/badges/oddsratio?color=brightgreen)](http://www.r-pkg.org/pkg/oddsratio)

oddsratio
=========

Convenience functions for odds ratio calculation of Generalized Additive (Mixed) Models and Generalized Linear (Mixed) Models with a binomial response variable (i.e. logistic regression models).

Examples
--------

### GLM

Odds ratio calculation of predictors `gre` & `gpa` of a fitted model `fit.glm` with increment steps of 380 and 5, respectively.
For factor variables (here: `rank` with 4 levels), automatically all odds ratios corresponding to the base level (here: `rank1`) are returned.
Data source: <http://www.ats.ucla.edu/stat/r/dae/logit.htm>

``` r
library(oddsratio)
dat <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
dat$rank <- factor(dat$rank)
fit.glm <- glm(admit ~ gre + gpa + rank, data = dat, family = "binomial")

calc.oddsratio.glm(data = dat, model = fit.glm, incr = list(gre = 380, gpa = 5))
```

    ##   predictor oddsratio          increment
    ## 1       gre     2.364                380
    ## 2       gpa    55.712                  5
    ## 3     rank2     0.509 Indicator variable
    ## 4     rank3     0.262 Indicator variable
    ## 5     rank4     0.212 Indicator variable

### GAM

For GAMs, the calculation of odds ratio is different. Due to its non-linear definition, odds ratios do only apply to specific value changes and are not constant throughout the whole value range of the predictor as for GLMs. Hence, odds ratios of GAMs can only be computed for one predictor at a time by holding all other predictors at a fixed value while changing the value of the specific predictor. Data source: `?mgcv::predict.gam()`

Here, the usage of `calc.oddsratio.gam()` is shown by calculating odds ratios of pred `x2` for a 20% steps across the whole value range of the predictor.

``` r
suppressPackageStartupMessages(library(mgcv))
set.seed(1234)
n <- 200
sig <- 2
dat <- gamSim(1, n = n,scale = sig, verbose = FALSE)
dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50)))
fit.gam <- mgcv::gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + offset(x3) + x4, data = dat)

calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", 
                   percentage = 20, slice = TRUE)
```

    ##   predictor value1 value2 perc1 perc2    oddsratio
    ## 1        x2  0.001  0.200     0    20 2.510768e+03
    ## 2        x2  0.200  0.400    20    40 2.870699e-02
    ## 3        x2  0.400  0.599    40    60 5.761210e-01
    ## 4        x2  0.599  0.799    60    80 6.032289e-02
    ## 5        x2  0.799  0.998    80   100 4.063187e-01

If you want to compute a single odds ratio for specific values, simply set param `slice = FALSE`:

``` r
calc.oddsratio.gam(data = dat, model = fit.gam, 
                   pred = "x2", values = c(0.099, 0.198))
```

    ##   predictor value1 value2 oddsratio
    ## 1        x2  0.099  0.198  23.32353

Installation
------------

Install from CRAN:

``` r
install.packages("oddsratio")
```

Get the development version from Github:

``` r
devtools::install_github("pat-s/oddsratio", build_vignettes = TRUE)
```

Examples
--------

To see full examples, please see the examples in the respective help pages `?calc.oddsratio.gam` and `?calc.oddsratio.gam` or take a look at the package vignette.

To Do
-----

-   Add calculation of odds ratio confidence intervals
-   Implement plotting function of calculated odds ratio in GLM/GAM function?
-   Add references for odds ratio calculation?
