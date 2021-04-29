
<!-- README.md is generated from README.Rmd. Please edit that file -->

# csmp

<!-- badges: start -->

<!-- badges: end -->

The goal of csmp is to assess the climate sensitivity of model
performance, as described in Marshall et al. (2021). Please see the
manuscript for more details about use and applications.

## Installation

You can install csmp from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools") # if devtools is not already installed
devtools::install_github("adrienne-marshall/CSMP", ref = 'main') # ref needed for older versions of devtools()
#> Downloading GitHub repo adrienne-marshall/CSMP@main
#> 
#>      checking for file ‘/private/var/folders/gc/pm7q4j_n2vq1xsghlzqgzc2c0000gn/T/RtmpXTGe9t/remotes2ca21144a435/adrienne-marshall-CSMP-8d7e008/DESCRIPTION’ ...  ✓  checking for file ‘/private/var/folders/gc/pm7q4j_n2vq1xsghlzqgzc2c0000gn/T/RtmpXTGe9t/remotes2ca21144a435/adrienne-marshall-CSMP-8d7e008/DESCRIPTION’ (594ms)
#>   ─  preparing ‘csmp’:
#> ✓  checking DESCRIPTION meta-information
#>   ─  checking for LF line-endings in source and make files and shell scripts
#>   ─  checking for empty or unneeded directories
#>   ─  building ‘csmp_0.0.0.9000.tar.gz’
#>      
#> 
#> Installing package into '/private/var/folders/gc/pm7q4j_n2vq1xsghlzqgzc2c0000gn/T/RtmpjQsh24/temp_libpath25c539730654'
#> (as 'lib' is unspecified)
```

## Examples

We can evaluate the model performance, as represented by an objective
function (e.g., RMSE), in different climatic contexts. Each value in the
objective function vector represents model performance in the
corresponding air temperature and precipitation vectors. In the
following example, air temperature, precipitation, and the objective
function are all random vectors representing hypothetical annual mean
air temperature, annual precipitation, and the model performance in a
given year.

``` r
library(csmp)
set.seed(10)
airtemp <- rnorm(20, mean = 0, sd = 1)
precip <- rnorm(20, mean = 500, sd = 100)
objective <- runif(20, min = 0, max = 1)

ans <- csmp(airtemp, precip, objective, return_plot = TRUE)
ans$stats
#>     rsquared p_airtemp  p_precip  csmp_warm   csmp_cool   csmp_dry    csmp_wet
#> 1 -0.0885348 0.8428947 0.5554549 0.01531651 -0.01905679 0.05575412 -0.04555934
ans$contour_plot 
```

<img src="man/figures/README-example-1.png" width="100%" />

The output contour plot can be modified with standard ggplot commands:

``` r
library(ggplot2)
ans$contour_plot + 
  labs(fill = "Objective\nfunction",
       x = "Air temperature (°C)",
       y = "Precipitation (mm)")
```

<img src="man/figures/README-modify plot-1.png" width="100%" />

The `new_airtemp` and `new_precip` parameters represent the conditions
under which to compare the change in model performance. For example, for
evaluation of model performance in a 2 °C warming scenario,
`new_airtemp` should be 2 °C warmer than historical average air
temperature (which can be specified with the `ref_airtemp` parameter or
inferred based on the supplied vector of air temperature observations).
`new_airtemp` and `new_precip` can each be vectors of length one
(representing unidirectional change in climate) or two (to evaluate,
e.g., potential changes due to wetting or drying).

In this example, we modify the new values in air temperature and
precipitation. Keep in mind the model will be most reliable if
predictions are within the range of observed variability.

``` r
# ans <- csmp(airtemp, precip, objective,
#             return_plot = TRUE,
#             new_airtemp = c(-4, 4),
#             new_precip = c(100, 800))
# ans$stats
```

# References

Marshall, A.M., T.E. Link, G.N. Flerchinger, D.J. Nicolsky, M.S. Lucash
(2021). Ecohydrologic modeling in deciduous boreal forest: Model
evaluation for application in non-stationary climates. *_Submitted to
Hydrological Processes_*.
