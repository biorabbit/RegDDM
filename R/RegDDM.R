#' Bayesian hierachical generalized linear regression using Drift-Diffusion Model
#' @description
#' Use
#'
#' @usage regddm(data1, data2, model, family = 'gaussian', ddm_link = "default", init = "default", scale = TRUE, ...)
#'
#' @param data1 subject-level dataframe. It must contain two columns: `id` and `y`.
#' @param data2 trial-level dataframe. It must contain three columns: `id`, `response` and `rt`.
#' @param model a list containing 1-5 formulas, specifying the dependence structure between variables.
#' @param family family of distribution of `y`. Can be `gaussian`, `bernoulli` or `poisson`.
#' @param ddm_link either `default`, `ident` or a named list of strings specifying the structure
#' @param init either `default` or other values supported by rstan (see rstan documentation)
#' @param scale a losigtic value, specifying weather or not to scale trial-level variables in `data2`
#' @param stan_filename a string specifying the automatically generated stan file name.
#'        if an empty string `''` is provided, a temporary file will be created and deleted after the model is fit.
#' @param fit_model a logistic value indicating weather or not to fit the model.
#'        If not, RegDDM will only generate the code and return the data input for stan.
#' @param ... other parameters sent to rstan package.
#'
#' @details
#' Additional details...
#'
#' @export
#'
#' @references reference
#'
#' @examples
#' # example code
#' fake_data = generate_fake_data()
#' model = list(v ~ x1 + x2, y ~ v_x1 + v_x2)
#' fit = regddm(
#'   fake_data[["data1"]],
#'   fake_data[["data2"]],
#'   model,
#'   warmup = 200,
#'   iter = 300
#' )
#' summary(fit)$summary
regddm = function(
    data1,
    data2,
    model,
    family = "gaussian",
    ddm_link = "default",
    init = "default",
    scale = TRUE,
    stan_filename = "stan_model.stan",
    fit_model = TRUE,
    warmup = 700,
    iter = 1000,
    chains = 4,
    cores = 4,
    ...
){


  # trial-level variables and subject-level covariates
  xvar = colnames(data2)
  xvar = xvar[!xvar %in% c("id", "response", "rt")]
  cvar = colnames(data1)
  cvar = cvar[!cvar %in% c("id", "y")]

  if(scale)(
    # scale trial-level variables to implement prior distribution and constraints
    for(x in xvar){
      data2[[x]] = scale(data2[[x]])[,1]
    }

    # subject-level covariates does not necessarily need to be scaled.
    # for(c in cvar){
    #   data1[[c]] = scale(data1[[c]])[,1]
    # }
  )

  # check for errors in the data
  check_data(data1, data2)

  # parse the model into better formats.
  model = parse_model(model)

  # check for errors in the model
  check_model(xvar, cvar, model)

  # format the data into a list required by rstan
  stan_data = format_data(data1, data2)

  # set the link function of ddm parameters
  if(is.character(ddm_link) && ddm_link == "default"){
    ddm_link = list(
      a = "exp",
      t = "exp",
      z = "inv_logit",
      v = ""
    )
  }
  else if(is.character(ddm_link) && ddm_link == "ident"){
    ddm_link = list(
      a = "",
      t = "",
      z = "",
      v = ""
    )
  }

  # see if the automatically generated file should be deleted
  delete_flag = FALSE
  if(stan_filename == ""){
    stan_filename = stringr::str_c(sample.int(999999, size = 1),"_stan_model_tmp.stan")
    delete_flag = TRUE
  }

  # automatically generate the stan model
  generate_model(
    xvar,
    cvar,
    data1,
    data2,
    model,
    ddm_link,
    family,
    stan_filename
  )
  rstan::stanc(stan_filename)

  # find the initialization for MCMC sampling
  if(is.character(init) && init == "default"){
    #init_list = list(t_0 = log(rep(0.03, N)), a_0 = log(rep(1, N)), v_0 = rep(1, N), z_0 = rep(0, N))
    init_list = generate_default_initialization(data1, data2, model, ddm_link)
    init = replicate(chains, init_list, simplify = FALSE)
  }

  # fit the stan model, if not, return the data input for stan
  if(!fit_model){
    return(stan_data)
  }

  stan_model <- stan_filename

  if(delete_flag){
    file.remove(stan_filename)
  }

  fit <- rstan::stan(
    file = stan_model,
    data = stan_data,
    init = init,
    warmup = warmup,
    iter = iter,
    chains = chains,
    cores = cores,
    ...
  )

  # present the results in a better format
  res = rstan::summary(fit)

  output = list(
    beta = NA, # glm regression parameters
    sigma_y = NA, # standard deviation of error term in glm.
    subject_ddm = NA, # ddm parameters of each subject
    group_ddm = NA, # group mean and SD of DDM parameters
    missing_value = NA, # estimated missing covariates
    group_covariates = NA, # group mean and sd of covariates
    max_r_hat = NA,
    model = model,
    stan_fit = fit # additional information can be found from the original stan model.
  )

  # print the useful information
  print(
    list(
      GLM_coefficients = NA
    )
  )

  return(output)
}
