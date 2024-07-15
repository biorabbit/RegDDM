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
#' @param scale a losigtic value, specifying weather or not to scale all variables except `y` prior to modeling
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
    warmup = 700,
    iter = 1000,
    chains = 4,
    cores = 4,
    ...
){


  # trial-level variables and subject-level covariates
  xvar = colnames(data2)
  xvar = xvar[!xvar %in% c("id","response","rt")]
  cvar = colnames(data1)
  cvar = cvar[!cvar %in% c("id", "y")]

  if(scale)(
    # scale variables
    for(x in xvar){
      data2[[x]] = scale(data2[[x]])[,1]
    }
    #for(c in cvar){
    #  data1[[c]] = scale(data1[[c]])[,1]
    #}
  )

  # check for errors in the data
  check_data(data1, data2)

  # check for errors in the model
  model = parse_model(model)
  check_model(xvar, cvar, model)

  # format the data into a list required by rstan
  stan_data = format_data(data1, data2)

  # automatically generate the stan model
  N = stan_data[["N"]]
  #rt_limits = dplyr::group_by(data2, id)
  #rt_limits = dplyr::summarise(rt_limits, rt = min(rt))
  #rt_limits = dplyr::pull(rt_limits, rt)
  #generate_model(xvar, cvar, data1, model, N, rt_limits,family)

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
  generate_model(xvar, cvar, data1, model,ddm_link,family)

  # find the initialization for MCMC sampling
  if(is.character(init) && init == "default"){
    init_list = list(t_0 = log(rep(0.03, N)), a_0 = log(rep(1, N)), v_0 = rep(1, N), z_0 = rep(0, N))
    init = replicate(chains, init_list, simplify = FALSE)
  }

  # fit the stan model
  stan_model <- "stan_model.stan"
  fit <- rstan::stan(
    file = stan_model,
    data = stan_data,
    init=init,
    warmup = warmup,
    iter = iter,
    chains = chains,
    cores = cores,
    ...
  )

  # tmp = round(summary(fit)$summary, 3)

  return(fit)

}
