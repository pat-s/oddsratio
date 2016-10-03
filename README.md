<<<<<<< HEAD
# oddsratio
=======
# Purpose

Convenience functions for odds ratio calculation of Generalized Additive Models (GAM) and Generalized Linear Models (GLM) with a binomial response variable (i.e. logistic regression models).  
Since both model types behave differently in the way to calculate odds increments for changes of their predictors due to their basic type (GLM = linear, GAM = non-linear), the usage of both functions `calc.oddsratio.glm()` and `calc.odds.ratio.gam()` is slightly different. Subsequently, both functions are explained seperately. 

# Install package

To install the latest version of the package, simply execute `devtools::install_github("pat-s/oddsratio")`. 

## GLM

With GLMs, the estimated coefficients of each predictor provide the linear change in log odds for a specific unit increase. The coefficient is multiplied with the unit change and converted into odds ratio using `exp()`. This unit increase can be specified in the function argument `incr` using a list with the name of the predictor and its corresponding change (e.g. `list(gre = 380)`). Multiple values for all continuous predictors can be given.  
For indicator variables, the odds ratio is calculated and reported for all levels in relation to the base factor level which was used for model training. 

### Arguments

- **model:** The fitted GAM model
>>>>>>> master

[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/oddsratio)](http://cran.r-project.org/package=oddsratio)


Convenience functions for odds ratio calculation of Generalized Additive (Mixed)
Models and Generalized Linear (Mixed) Models with a binomial
response variable (i.e. logistic regression models).  

# Purpose

### GLM

Right now, odds ratio calculation of GLMs is done usually by exponentiating the model 
coefficents (using `coef`) of each (continuous) predictor with a respective increment 
step (here: `incr`). Here, four model coefficients are assumed with their
respective increment steps. 

```R
incr <- c(1, 5, 2, 20)
exp(coef(model) * incr)
```

Resulting in a plain numerical output (odds ratio) without reference to its respective 
predictor. 
Mistakes can be made quite easily by specifying the increment steps in a wrong 
order (referencing to the order of coefficients). 
Cases in which the coefficients are non-numerical are not covered in this 
example. 
However, the package also takes care of these cases. 
Function `calc.odds.ratio.glm()` provides a nicely formatted output and 
helps avoiding false references of predictors and increment steps by 
explicitly specifying the relations in its argument `incr`. 

```R
calc.oddsratio.glm(data = dat, model = fit.glm, incr = list(gre = 380, gpa = 5))
```

### GAM

For GAMs, the calculation of odds ratio is different. 
Due to its non-linear definition, odds ratios do only apply to specific
value pairs and are not constant throughout the whole value range of the 
predictor as for GLMs. 
Hence, odds ratios of GAMs can only be computed for one predictor at a time fixing 
all other predictors to a fixed value while only changing the desired value 
combination of the specific predictor. 

To simplify this behaviour, the `oddsratio` package does this work for you:
Function `calc.odds.ratio.gam` takes the predictor name (`pred`) and the 
two values (`values`) as arguments and saves you time and coding lines. 

```R
calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", values = c(0.099, 0.198))
```

If you want to compute multiple odds ratios, the argument `slice = TRUE` 
provides the opportunity to split the value range in percentage steps (`percentage`)
for which odds ratios are calculated. For example, applying a value of *20* 
will split the value range of the chosen predictor in five parts and computes 
odds ratios between those (0% - 20%, 20% - 40%, etc.). 

```R
calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", percentage = 20, slice = TRUE)
```

# Install package

To install the latest version of the package, simply execute
```R
devtools::install_github("pat-s/oddsratio")
```

# Examples

To see the functions in action, please see the examples in the respective help pages `?calc.oddsratio.gam` and `?calc.oddsratio.gam`.  

# To Do

- Write vignette (necessary?) for both functions or include both in one?
- Add calculation of odds ratio confidence intervals
- Implement plotting function of calculated odds ratio in GLM/GAM function?
- Add references for odds ratio calculation
