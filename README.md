
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RegDDM

<!-- badges: start -->
<!-- badges: end -->

Build Regression models over Drift Diffusion Model parameters using
MCMC!

## Installation

You can install latest version of RegDDM using Github. The package will
later be available on CRAN.

``` r
remotes::install_github("biorabbit/RegDDM")
```

## Example

First, load the package and the example dataset.

``` r
library(RegDDM)
data(regddm_tutorial)
```

`data1` is the subject-level dataset:

``` r
head(regddm_tutorial$data1)
#> # A tibble: 6 × 4
#>      id      y      c1 c2   
#>   <int>  <dbl>   <dbl> <chr>
#> 1     1  1.97   0.0846 a    
#> 2     2  2.64   1.82   <NA> 
#> 3     3  5.18   1.23   b    
#> 4     4 -1.16  NA      c    
#> 5     5  0.985  1.77   a    
#> 6     6  2.05   1.37   b
```

`data2` is the subject-level dataset:

``` r
head(regddm_tutorial$data2)
#> # A tibble: 6 × 5
#>      id     x1 x2       rt response
#>   <int>  <dbl> <chr> <dbl>    <dbl>
#> 1     1  0.404 a     0.753        1
#> 2     1 -0.871 b     0.731        1
#> 3     1  1.57  c     0.897        1
#> 4     1  1.51  a     0.940        1
#> 5     1 -0.812 b     0.652        1
#> 6     1  1.17  c     0.601        0
```

Specify the model using a list. In this example, the drift rate is
influenced by `x1` and the subject’s outcome `y` is predicted by
baseline drift rate when `x1` is 0 (`v_0`), the influence of `x1` on
drift rate `v_x1` and covariate `c1`:

``` r
model = list(
  v ~ x1,
  y ~ v_0 + v_x1 + c1
)
```

Use the main function to automatically generate the stan model and
summary the results. This could take ~20 minutes to run. The rows
starting with ‘beta\_’ are the posterior distributions of regression
parameters:

``` r
fit = regddm(
  regddm_tutorial$data1,
  regddm_tutorial$data2,
  model
)
#> # A tibble: 5 × 6
#>   variable    mean    sd `2.5%` `97.5%`  Rhat
#>   <chr>      <dbl> <dbl>  <dbl>   <dbl> <dbl>
#> 1 beta_0     1.58  0.963 -0.316   3.47  0.999
#> 2 beta_v_0  -0.867 0.529 -1.87    0.174 0.999
#> 3 beta_v_x1  0.919 0.196  0.525   1.30  0.999
#> 4 beta_c1    0.914 0.388  0.124   1.63  1.00 
#> 5 sigma      1.13  0.176  0.847   1.51  1.00
```

In conclusion, the outcome is positively correlated with `v_x1` and
`c1`. The higher the influence of `x1` and the higher the first 
covariate, the higher the outcome `y`.

# Using your own data!

If you want to fit the model on your own data, you need to specify
`data1`, `data2` and `model`.

`data1` is subject-level data table. It should contain the following: \*
`id`: unique indexing column for each subject. \* other subject-level
variables that we want to include in the regression. Variables used as
covariates could contain missing values.

`data2` is trial-level data table. It should contain the following: \*
`id`: the subject of each trial using the same index in `data1`. \*
`rt`: response time of the trial in seconds. \* \`response\`\`: response
the trial. must be either 0 or 1. \* trial-level variables. These are
the variables that differ by trial, such as difficulty of the task or
different numbers on the screen. We assume that subjects’ behavior
changes according to these variables. These variables cannot contain
missing values.

`model` is the proposed dependency between these parameters. Default is
an empty list. It must be a list of 0 - 5 formulas. The outcome of these
formulas can be either: \* one of the four DDM parameters `a`, `t`, `z`,
`v`, modeling the relationship between DDM parameters and trial-level
variables. \* one formula for GLM regression, modeling the relationship
between estimated DDM parameters and other subject-level variables.

`family` is the family of distribution of GLM. It can be either
`"gaussian"`, `"bernoulli"` or `"poisson"`. Default is `"gaussian"`.

`init` is how we initialize the MCMC algorithm. The `"default"`
initialization should work in most conditions

`prior` determines whether to use the default prior for DDM parameters
or not. Default is `TRUE`

`stan_filename` is the file loaction for the automatically generated
stan model. If an empty string ’’ is provided, a temporary file will be
created and deleted after the model is fit. Default is
`"stan_model.stan"`

`gen_model` determines whether to generate the model or not. Default is
`TRUE`.

`fit_model` determines whether to fit the model or not. Default is
`TRUE`.

`...`: additional parameters used by `rstan`, including
`warmup`,`iter`,`chains`,`cores` etc.

# Citation

to be added
