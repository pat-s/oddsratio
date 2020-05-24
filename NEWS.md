# oddsratio 2.0.1

## Package infrastructure

- Move to GitHub Actions
- Add lintr
- Add codemeta
- Add precommit framework
- Use stock pkgdown theme

## R package

- Optimize wording and documentation
- Update broken links
- Remove dependencies: 
  - tibble
  - stringr
  - gtable
  - gam
  - cowplot
  - scales
- Account for partial matching of predictor variables (#34)


# oddsratio 2.0.0

* return a `tibble` instead of a `data.frame`
* clean up code base
* don't use `cowplot` ggplot theme by default
* optimize wording in vignette

# oddsratio 1.0.3

* update functions to work with ggplot2 v3.0.0

# oddsratio 1.0.2

## Minor
  * Add CITATION file

# oddsratio 1.0.0

## Major
  * rename functions (snake_case)

# oddsratio 0.3.1

* update functions to work with ggplot2 v2.2.0
* add data and enable lazy loading in examples

# oddsratio 0.3.0

#### New functions
* `plot_smooth.gam()`: Lets you plot smoothing functions of GAM(M)s using `ggplot2`.
* `add.oddsratio.into.plot()`: Add odds ratios into plot of GAM(M) smoothing function.

#### Function updates
* `calc.oddsratio.glm`, `calc.oddsratio.gam`: Add odds ratio confident interval calculation 
* For GLM models CI level can be specified manually.
* Print 'CI' warning if model is of type `glmmPQL`

# oddsratio 0.2.0

* Remove param `quietly`
* return data.frame in any case
* update DESCRIPTION

# oddsratio 0.1.0

* Initial release attempt to CRAN
