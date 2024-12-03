
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
#>   id          y         c1   c2
#> 1  1  1.9690519 0.08461457    a
#> 2  2  2.6410850 1.82427245 <NA>
#> 3  3  5.1843542 1.23414213    b
#> 4  4 -1.1623707         NA    c
#> 5  5  0.9845534 1.77316247    a
#> 6  6  2.0520609 1.37139039    b
```

`data2` is the subject-level dataset:

``` r
head(regddm_tutorial$data2)
#>   id         x1 x2        rt response
#> 1  1  0.4038328  a 0.7533853        1
#> 2  1 -0.8707744  b 0.7314780        1
#> 3  1  1.5737835  c 0.8965344        1
#> 4  1  1.5112327  a 0.9395178        1
#> 5  1 -0.8122571  b 0.6522295        1
#> 6  1  1.1721147  c 0.6013884        0
```

Specify the model using a list. In this example, the drift rate is
influenced by `x1`. The subject’s outcome `y` is predicted by baseline
drift rate `v_0` (drift rate when `x1` is 0), the influence of `x1` on
drift rate `v_x1` and covariate `c1`:

``` r
model = list(
  v ~ x1,
  y ~ v_0 + v_x1 + c1
)
```

Use the main function of `RegDDM` to automatically generate the `RStan`
model and summary the results. This could take ~20 minutes to run. The
rows starting with ‘beta\_’ are the posterior distributions of
regression parameters:

``` r
fit = regddm(
  regddm_tutorial$data1,
  regddm_tutorial$data2,
  model
)

print(fit)
#> RegDDM Model Summary
#> Number of subjects: 30
#> Number of trials: 3000
#> Model:
#>   v ~ x1
#>   y ~ v_0 + v_x1 + c1
#> Family: gaussian
#> Sampling: 4 chains, 500 warmups and 1000 iterations were used. Longest elipsed time is 639 s.
#> 
#> Regression coefficients:
#>    variable   mean    sd    2.5% 97.5% n_eff  Rhat
#> 1    beta_0  1.551 0.992 -0.3261 3.551  1413 0.998
#> 2  beta_v_0 -0.851 0.551 -1.9479 0.253  1669 0.999
#> 3 beta_v_x1  0.917 0.202  0.5067 1.309  3290 0.998
#> 4   beta_c1  0.918 0.389  0.0827 1.661  1791 0.998
#> 5     sigma  1.131 0.180  0.8394 1.538  2068 1.000
#> Maximum R-hat: 1.005
```

In this example, the outcome is positively correlated with `v_x1` and
`c1`, but not `v_0`. The higher the influence of `x1` on drift rate and
the higher the covariate, the higher the outcome `y`.

# Using your own data!

If you want to fit the model on your own data, you need to specify
`data1`, `data2` and `model`.

`data1` is subject-level data table. It should contain the following: \*
`id`: unique indexing column for each subject. \* other subject-level
variables that we want to include in the regression. Missing value is
supported

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
