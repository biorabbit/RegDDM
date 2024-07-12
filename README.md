
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RegDDM

<!-- badges: start -->
<!-- badges: end -->

Build Regression models over Drift Diffusion Model parameters using
MCMC!

## Installation

You can install latest version of RegDDM using Github:

``` r
remote::install_github("biorabbit/RegDDM")
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
summary the results. This could take ~10 minutes to run. The rows
starting with ‘beta\_’ are the posterior distributions of regression
parameters:

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
#> Warning in check_data(data1, data2): variabley are not scaled, which may influence model convergence and validity of priors.
#> Warning in mean.default(data1, na.rm = TRUE): 参数不是数值也不是逻辑值：回覆NA
#> Warning in sqrt(var(data1, na.rm = TRUE) * n_obs/(n_obs - 1)): 产生了NaNs
#> Warning in mean.default(data1, na.rm = TRUE): 参数不是数值也不是逻辑值：回覆NA
#> Warning in sqrt(var(data1, na.rm = TRUE) * n_obs/(n_obs - 1)): 产生了NaNs
#> Warning: There were 105 divergent transitions after warmup. See
#> https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
#> to find out why this is a problem and how to eliminate them.
#> Warning: There were 244 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
#> https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
#> Warning: There were 2 chains where the estimated Bayesian Fraction of Missing Information was low. See
#> https://mc-stan.org/misc/warnings.html#bfmi-low
#> Warning: Examine the pairs() plot to diagnose sampling problems
#> Warning: The largest R-hat is 1.28, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess

round(rstan::summary(fit)$summary, 3)[1:6, c(1,3,4,8,9,10)]
#>              mean     sd    2.5%  97.5%   n_eff  Rhat
#> beta_0     -4.088 12.665 -41.660 11.741  15.429 1.258
#> beta_v_x1   1.176  4.331  -5.293  8.173  58.682 1.071
#> beta_v_x2 -11.086 30.951 -88.337 23.608  14.268 1.277
#> beta_c1    -1.089  0.256  -1.583 -0.536 297.927 1.009
#> beta_c2    -0.025  0.227  -0.477  0.393 325.549 1.008
#> sigma_y     0.258  0.192   0.038  0.708  43.071 1.077
```

Comparing with a liner model using the true sensitivity:

``` r
summary(lm(y ~ v_x1 + v_x2 + c1 + c2, data = fake_data[["data1_true"]]))
#> 
#> Call:
#> lm(formula = y ~ v_x1 + v_x2 + c1 + c2, data = fake_data[["data1_true"]])
#> 
#> Residuals:
#>         1         2         3         4         5         6         7         8 
#> -0.027036 -0.016014 -0.034147 -0.045841  0.175341 -0.016902 -0.009869  0.064586 
#>         9        10 
#> -0.064369 -0.025750 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  0.15982    0.17400   0.919  0.40050    
#> v_x1         1.01694    0.12963   7.845  0.00054 ***
#> v_x2        -0.08156    0.17197  -0.474  0.65529    
#> c1          -0.93483    0.05729 -16.317 1.58e-05 ***
#> c2           0.02271    0.04611   0.493  0.64319    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 0.09419 on 5 degrees of freedom
#> Multiple R-squared:  0.9927, Adjusted R-squared:  0.9869 
#> F-statistic: 170.2 on 4 and 5 DF,  p-value: 1.581e-05
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
