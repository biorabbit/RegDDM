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
#' @param prior a losigtic value, specifying whether or not to use default prior for DDM parameters where default is TRUE. Improper use of prior distribution may influence model validity.
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
#' ## Not run:
#' # example analysis of Cognitive Reserve Study dataset.
#' data(regddm_data)
#' data1 = regddm_data$data1
#' data2 = regddm_data$data2
#' model = list(v ~ memload, y ~ v_0 + v_memload + age + education)
#' fit = regddm(
#'   regddm_data$data1,
#'   regddm_data$data2,
#'   model,
#'   warmup = 500,
#'   iter = 1000
#' )
#' ## End(Not run)
regddm = function(
    data1,
    data2,
    model,
    family = "gaussian",
    # ddm_link = "default",
    init = "default",
    prior = TRUE,
    stan_filename = "stan_model.stan",
    gen_model = TRUE,
    fit_model = TRUE,
    warmup = 500,
    iter = 1000,
    chains = 4,
    cores = 4,
    ...
){


  # trial-level variables and subject-level covariates
  #xvar = colnames(data2)
  #xvar = xvar[!xvar %in% c("id", "response", "rt")]
  #cvar = colnames(data1)
  #cvar = cvar[!cvar %in% c("id", "y")]

  # check for errors in the data
  check_data(data1, data2)

  # parse the model into better formats.
  model = parse_model(model, data1, data2)

  # check for errors in the model
  #check_model(xvar, cvar, model)

  # format the data into a list required by rstan
  stan_data = format_data(data1, data2)

  # set the link function of ddm parameters
  # if(is.character(ddm_link) && ddm_link == "default"){
  #   ddm_link = list(
  #     a = "exp",
  #     t = "exp",
  #     z = "inv_logit",
  #     v = ""
  #   )
  # }
  # else if(is.character(ddm_link) && ddm_link == "ident"){
  #   ddm_link = list(
  #     a = "",
  #     t = "",
  #     z = "",
  #     v = ""
  #   )
  # }

  # see if the automatically generated file should be deleted
  delete_flag = FALSE
  if(stan_filename == ""){
    stan_filename = stringr::str_c(sample.int(999999, size = 1),"_stan_model_tmp.stan")
    delete_flag = TRUE
  }

  # automatically generate the stan model
  if(gen_model){
  generate_model(
    stan_data,
    data1,
    model,
    prior,
    # ddm_link,
    family,
    stan_filename
  )
  }
  rstan::stanc(stan_filename)

  # find the initialization for MCMC sampling
  if(is.character(init) && init == "default"){
    # init_list = list(t_0 = log(rep(0.03, N)), a_0 = log(rep(1, N)), v_0 = rep(1, N), z_0 = rep(0, N))
    # init_list = generate_default_initialization(data1, data2, model, ddm_link)
    init_list = generate_default_initialization(data1, data2, model)
    init = replicate(chains, init_list, simplify = FALSE)
  }

  # fit the stan model, if not, return the data input for stan
  if(!fit_model){
    return(stan_data)
  }

  stan_model <- stan_filename

  fit <- rstan::stan(
    file = stan_model,
    data = stan_data$data,
    init = init,
    warmup = warmup,
    iter = iter,
    chains = chains,
    cores = cores,
    ...
  )

  if(delete_flag){
    file.remove(stan_filename)
  }

  # present the results in a better format
  output = summary_results(fit, model, data1)
  #output = fit

  # print the useful information
  print(
    output$glm_coefficiets[,c(1,2,4,5,9,11)]
  )

  return(output)
}
