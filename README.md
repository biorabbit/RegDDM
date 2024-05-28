
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RegDDM

<!-- badges: start -->
<!-- badges: end -->

Build Regression models over Drift Diffusion Model parameters using
MCMC!

## Installation

You can install latest version of RegDDM using Github:

``` r
remotes::install_github("DominiqueMakowski/easyRT")
remotes::install_github("biorabbit/RegDDM")
```

## Example

First, load the demo data which contains 10 subjects and 100 trials for
each subject. This yeilds 2 separate tables. `data1` contains
subject-level variables and `data2` contains trial-level variables.

``` r
library(RegDDM)
```

Alternatively, you can generate some other fake data using the
`generate_fake_data()` function, which gives you the flexibility to test
the models.

``` r
fake_data = generate_fake_data()
data1 = fake_data[["data1"]]
data2 = fake_data[["data2"]]
```

Specify the model using a list. In this demo, we assume the drift rate
`v` is linearly determined by two trial-level variables `x1` and `x2`.
The outcome `y` is influenced by how sensitivity the subject is to `x1`
and `x2`, as well as two covariates `c1` and `c2`:

``` r
model = list(
  a = c(),
  z = c(),
  t = c(),
  v = c("x1", "x2"),
  y = c("v_x1", "v_x2", "c1", "c2")
)
```

Use the main function to automatically generate the stan model and
summary the results. This could take ~10 minutes to run. The rows
starting with ‘beta\_’ are the posterior distributions of regression
parameters:

``` r
fit = regddm(
  data1,
  data2,
  model = model,
  warmup = 300,
  iter = 500,
  chains = 4,
  cores = 4,
  thin = 1,
)

round(rstan::summary(fit)$summary, 3)[1:6, c(1,3,4,8,9,10)]
```

``` r
#>             mean    sd   2.5%  97.5%   n_eff  Rhat
#> beta_0    -0.073 0.794 -2.219  1.178 157.223 1.015
#> beta_v_x1  0.980 0.538  0.174  2.321 178.711 1.016
#> beta_v_x2 -0.354 0.842 -2.687  1.143 170.865 1.011
#> beta_c1   -1.083 0.122 -1.329 -0.843 362.126 1.003
#> beta_c2   -0.046 0.112 -0.272  0.180 599.815 0.999
#> sigma_y    0.228 0.154  0.065  0.613 194.202 1.008
```

# Using your own data!

If you want to fit the model on your own data, you need to specify
`data1`, `data2` and `model`.

`data1` is subject-level data table. It should contain the following: \*
`id`: consecutive positive integers starting from 1. If you index the
subjects differently, consider putting your old index in a covariate
column. \* `y`: outcome. Currently RegDDM only support linear model. \*
other covariates that we want to adjust for.

`data2` is trial-level data table. It should contain the following: \*
`id`: the subject of each trial. must be one of `id` in `data1`. \*
`rt`: response time of the trial. must be positive real number \*
\`response\`\`: response the trial. must be either 0 or 1. Typically, 1
stands for acceptance. \* trial-level variables. These are the variables
that differs by trial, such as difficulty of the task or different
numbers on the screen. We assume that subjects’ behavior changes
according to these variables.

`model` is the proposed dependency between these parameters. It must be
a list containing 5 items `a`, `t`, `z`, `v`, `y`. Each item is another
list containing the dependency. \* `a`, `t`, `z`, `v`: can only contain
names of trial-level variables in `data2`. \* `y`: can only contain
names of covariates in `data1` or parameters specified before in the
format of `a/t/z/v`\_`trial_level_variables`
