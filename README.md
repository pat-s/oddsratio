
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
calc.oddsratio.glm(data = dat, model = fit.glm, incr = list(gre = 380, gpa = 5))
```

``` r
Variable:   'gre'
Increment:  '380'
Odds ratio: 2.364

Variable:   'gpa'
Increment:  '5'
Odds ratio: 55.712

Variable:   'rank2'
Increment:  'Non numeric predictor. Refer to base factor level!'
Odds ratio: 0.509

Variable:   'rank3'
Increment:  'Non numeric predictor. Refer to base factor level!'
Odds ratio: 0.262

Variable:   'rank4'
Increment:  'Non numeric predictor. Refer to base factor level!'
Odds ratio: 0.212
```

### GAM

For GAMs, the calculation of odds ratio is different. Due to its non-linear definition, odds ratios do only apply to specific value changes and are not constant throughout the whole value range of the predictor as for GLMs. Hence, odds ratios of GAMs can only be computed for one predictor at a time by holding all other predictors at a fixed value while changing the value of the specific predictor. Data source: `?mgcv::predict.gam()`

Here, the usage of `calc.oddsratio.gam()` is shown by calculating odds ratio of pred `x2` for a value change from 0.099 to 0.198.

``` r
calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", values = c(0.099, 0.198))
```

``` r
Predictor: 'x2'

Odds ratio from '0.099' to '0.198': 109.2059
```

If you want to compute multiple odds ratios, the argument `slice = TRUE` provides the opportunity to split the value range in percentage steps (`percentage`) for which odds ratios are calculated. For example, applying a value of *20* will split the value range of the chosen predictor in five parts and computes odds ratios between those (0% - 20%, 20% - 40%, etc.).

``` r
calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", percentage = 20, slice = TRUE)
```

``` r
Predictor: 'x2'
Steps:     5 (20%)

Odds ratio from 0.002(0%) to 0.201(20%): 5259.149
Odds ratio from 0.201(20%) to 0.399(40%): 0.008115773
Odds ratio from 0.399(40%) to 0.598(60%): 1.841249
Odds ratio from 0.598(60%) to 0.797(80%): 0.1072271
Odds ratio from 0.797(80%) to 0.996(100%): 0.0680226
```

Installation
------------

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
