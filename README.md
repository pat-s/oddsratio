# Purpose

When dealing with binomial GAMs, odds increases related to certain predictor increment steps are not linear (as they are for GLMs for example). 
Hence, odds increments corresponding to specific increases in each variable have to be calculated for every predictor change combination while holding other predictors constant at the same time.  

This function provides this ability. It has two basic applications:  
1. Calculate odds ratio between two manually given values  
2. Calculate odds ratios of certain increment steps using the full range of the selected predictor (e.g. for every 10% increase in predictor X)

# Arguments

- **model:** The fitted GAM model

- **data**: The training data of the fitted GAM model

- **predictors:** Character vector. Predictor names the GAM was trained with (without the response!)

- **pred:** Character. The predictor of which to estimate odds ratio(s)

- **values:** Numeric vector of length two. Predictor values to estimate odds ratio from. Function is coded to use the first given number as the "lower" one, i.e. calculating odds ratio "from *value1* to *value2*". 

- **slice:** Logical. Default `FALSE`. Whether to calculate odds ratio for fixed increment steps over the whole predictor distribution. See `steps` for setting the increment values.

- **steps:** Numeric. Number of tiles to split the predictor distribution into. A value of *10* would split the predictor distribution by 10% steps. A value of *5* would split it into 20% steps. Only needed if `slice` == `TRUE`.
