
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RegDDM

<!-- badges: start -->
<!-- badges: end -->

Build Regression models over Drift Diffusion Model parameters using
MCMC!

## Installation

You can install latest version of RegDDM using Github:

``` r
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
  v ~ x1 + x2,
  y ~ v_x1 + v_x2 + c1 + c2
)
```

Use the main function to automatically generate the stan model and
summary the results. This could take ~10 minutes to run. Some warnings
may pop out depending on model convergence, which can be helped by
increasing iterations. The rows starting with ‘beta\_’ are the posterior
distributions of regression parameters:

``` r
fit = regddm(
  data1,
  data2,
  model = model,
  family = "gaussian",
  scale = FALSE,
  warmup = 500,
  iter = 700
)


round(rstan::summary(fit)$summary, 3)[1:6, c(1,3,4,8,9,10)]
#>             mean    sd   2.5%  97.5%   n_eff  Rhat
#> beta_0    -0.019 0.839 -1.607  0.880  45.169 1.074
#> beta_v_x1  1.052 1.017 -0.051  2.957  40.073 1.086
#> beta_v_x2 -0.127 0.461 -1.005  0.891  97.702 1.020
#> beta_c1   -1.026 0.173 -1.306 -0.716 323.395 1.005
#> beta_c2    0.132 0.099 -0.073  0.332 364.489 1.002
#> sigma_y    0.210 0.247  0.039  0.647  58.069 1.055
```

Comparing with a liner model using the true sensitivity:

``` r
summary(lm(y ~ v_x1 + v_x2 + c1 + c2, data = fake_data[["data1_true"]]))
#> 
#> Call:
#> lm(formula = y ~ v_x1 + v_x2 + c1 + c2, data = fake_data[["data1_true"]])
#> 
#> Residuals:
#>        1        2        3        4        5        6        7        8 
#> -0.02469  0.04263 -0.01908  0.09315 -0.01233 -0.10934  0.12474 -0.03710 
#>        9       10 
#> -0.02976 -0.02824 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.11060    0.10552   1.048  0.34259    
#> v_x1         1.06778    0.16878   6.326  0.00145 ** 
#> v_x2         0.03067    0.14810   0.207  0.84412    
#> c1          -0.97797    0.03704 -26.402 1.46e-06 ***
#> c2           0.05609    0.03691   1.520  0.18904    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.09186 on 5 degrees of freedom
#> Multiple R-squared:  0.9939, Adjusted R-squared:  0.989 
#> F-statistic: 203.5 on 4 and 5 DF,  p-value: 1.015e-05
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
a list containing several formulas, such as `v ~ x1 * x2` \* `a`, `t`,
`z`, `v`: can only contain names of trial-level variables in `data2`. \*
`y`: can only contain names of covariates in `data1` or parameters
specified before in the format of `{a/t/z/v}_trial_level_variables`For
example `v_x1` for main effect and `v_x1_x2` for interaction term.

`family` is the family of distribution of outcome `y`, similar to ones
in generalized linear model. It can take either `gaussian`, `bernoulli`
or `poisson`. Only canonical link function is used in RegDDM.

`ddm_link` specifies the relationship between DDM parameters of each
trial and trial-level parameters. By default,

``` r
  ddm_link = list(
    a = "exp",
    t = "exp",
    z = "inv_logit",
    v = ""
  )
```

`init` how to initialize the MCMC algorithm `scale`
