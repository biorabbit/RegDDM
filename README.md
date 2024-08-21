
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
#> 
#> 载入程辑包：'RegDDM'
#> The following object is masked from 'package:base':
#> 
#>     summary
data(regddm_data)
```

`data1` is the subject-level dataset with 49 rows and 4 columns:

``` r
head(regddm_data$data1)
#>     id      y age education
#> 1 4200 103.84  25        13
#> 2 4203 114.96  23        14
#> 3 4206 120.48  26        16
#> 4 4221 116.96  25        16
#> 5 4231 113.20  26        14
#> 6 4233 121.36  30        16
```

`data2` is the subject-level dataset with 6032 rows and 4 columns:

``` r
head(regddm_data$data2)
#>     id memload response    rt
#> 1 4200       3        1 0.985
#> 2 4200       6        1 0.829
#> 3 4200       1        1 0.901
#> 4 4200       6        1 1.237
#> 5 4200       6        1 1.163
#> 6 4200       6        1 0.799
```

Specify the model using a list. In this example, we want to study how
the memory load of the trial (`memload` variable) influences the drift
rate v for each subject differently, and how such influence correlates
with the IQ of the subject (`y` variable). Thus, the following model is
built:

``` r
model = list(
  v ~ memload,
  y ~ v_0 + v_memload + age + education
)
```

Use the main function to automatically generate the stan model and
summary the results. This could take ~20 minutes to run. The rows
starting with ‘beta\_’ are the posterior distributions of regression
parameters:

``` r
fit = regddm(
  data1,
  data2,
  model,
  warmup = 300,
  iter = 500
)
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
#> # A tibble: 6 × 6
#>   variable       se_mean     sd   `75%` `97.5%` n_eff
#>   <chr>            <dbl>  <dbl>   <dbl>   <dbl> <dbl>
#> 1 beta_0          0.441  12.8   110.    126.     847.
#> 2 beta_v_0        0.0959  2.59   -3.87   -0.143  729.
#> 3 beta_v_memload  0.315   7.99  -12.9    -0.504  646.
#> 4 beta_age        0.0110  0.383   0.613   1.07  1205.
#> 5 beta_education  0.0200  0.596   1.03    1.80   886.
#> 6 sigma_y         0.0313  0.870   7.43    8.75   774.
```

Based on the result, increasing memory load by one standard deviation
will decrease the drift rate by around 0.431. The more ”negative” such
influence is, the lower the IQ of the subject. In other words, subjects
with lower IQ are more susceptible to increased memory load.

# Using your own data!

If you want to fit the model on your own data, you need to specify
`data1`, `data2` and `model`.

`data1` is subject-level data table. It should contain the following: \*
`id`: unique indexing column for each subject. \* `y`: primary outcome.
\* other covariates that we want to adjust for. These covariates could
contain some missing values. However,if there are too many missing
values, the model may fail to converge.

`data2` is trial-level data table. It should contain the following: \*
`id`: the subject of each trial using the same index in `data1`. All
subjects must have a trial and all trials must have a subject. \* `rt`:
response time of the trial in seconds. \* \`response\`\`: response the
trial. must be either 0 or 1. \* trial-level variables. These are the
variables that differ by trial, such as difficulty of the task or
different numbers on the screen. We assume that subjects’ behavior
changes according to these variables. These variables cannot contain
missing values.

`model` is the proposed dependency between these parameters. It must be
a list containing several formulas, such as `v ~ x1 * x2` \* `a`, `t`,
`z`, `v`: can only contain names of trial-level variables in `data2`. \*
`y`: can only contain names of covariates in `data1` or parameters
specified before in the format of `{a/t/z/v}_trial_level_variables`For
example `v_x1` for main effect and `v_x1_x2` for interaction term.

`family` is the family of distribution of outcome `y`, similar to ones
in generalized linear model. It can take either `gaussian`, `bernoulli`
or `poisson`. Canonical link function is used in RegDDM.

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

`init` how to initialize the MCMC algorithm. The `"default"`
initialization should work in most conditions

`scale` whether to scale the trial-level variables in `data2` or not. If
these variables are not scaled, the default prior distribution and
constraints will be disabled.

`stan_filename` the file loaction for the automatically generated stan
model. If an empty string ’’ is provided, a temporary file will be
created and deleted after the model is fit.

# Citation

to be added
