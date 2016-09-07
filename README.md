# Purpose

Convenience functions for odds ratio calculation of Generalized Additive Models (GAM) and Generalized Linear Models (GLM) with a binomial response variable (i.e. logistic regression models).  
Since both model types behave differently in their way to calculate odds increments for changes of their predictors (GLM = linear, GAM = non-linear), both functions `calc.oddsratio.glm()` and `calc.odds.ratio.gam()` behave differently. Subsequently, both functions are explained seperately. 

## GLM

With GLMs, the estimated coefficients of each predictor provide the linear change in log odds for a specific unit increase. The coefficient is multiplied with the unit change and converted into odds ratio using `exp()`. This unit increase can be specified in the function argument `incr` using a list with the name of the predictor and its corresponding change (e.g. `list(gre = 380)`). Multiple values for all continuous predictors can be given.  
For indicator variables, the odds ratio is calculated and reported for all levels in relation to the base factor level which was used for model training. 

### Arguments

- **model:** The fitted GAM model

- **data**: The training data of the fitted GAM model

- **incr:**: List. Increment values of each predictor

- **quietly:** Logical. Default = `FALSE`. Whether to output information to the console.

## GAM

When dealing with binomial GAMs, odds increases related to certain predictor increment steps are not linear (as they are for GLMs for example). 
Hence, odds increments corresponding to specific increases in each variable have to be calculated for every predictor change combination while holding other predictors constant at the same time.  
To do so, log odds of a specific predictor combination are calculated using the generic `predict()` function with the `type == link` argument. The log odds estimates are substracted by each other and converted into odds applying `exp()` on it. Since callin `exp()` on a substraction (log odds (1) - log odds (2)) turns the substraction into a division, the result is an odds ratio. 

This function provides this ability. It has two basic applications:  
1. Calculate odds ratio between two manually given values  
2. Calculate odds ratios of certain increment steps using the full range of the selected predictor (e.g. for every 10% increase in predictor X)

### Arguments

- **model:** The fitted GAM model

- **data**: The training data of the fitted GAM model

- **pred:** Character. The predictor of which to estimate odds ratio(s)

- **values:** Numeric vector of length two. Predictor values to estimate odds ratio from. Function is coded to use the first given number as the "lower" one, i.e. calculating odds ratio "from *value1* to *value2*". Only used if `slice = FALSE`. 

- **slice:** Logical. Default `FALSE`. Whether to calculate odds ratio for fixed increment steps over the whole predictor distribution. See `steps` for setting the increment values.

- **percentage:** Numeric. Percentage number to split the predictor distribution into. A value of *10* would split the predictor distribution by 10% intervals. A value of *20* would split it into 20% intervals. Only needed if `slice` == `TRUE`.

- **quietly:** Logical. Default = `FALSE`. Whether to output information to the console.

# Examples

To see the functions in action, please knit the .Rmd file located in `R/`. 

# ToDo

- Check usage with different functions (`gam::gam`, `lme4::glmer`, `mgcv::gamm`) and more?
- Write vignette for both functions or include both in one?
- Add calculation of odds ratio confidence intervals
- Set everything up for R package init
