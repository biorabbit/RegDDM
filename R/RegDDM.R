#' Bayesian hierachical generalized linear regression using Drift-Diffusion Model
#' @description
#' `regddm` makes it easy to fit a single Bayesian hierarchical drift-diffusion
#' model (DDM) that estimates the DDM parameters of each subject and uses the
#' estimated parameters as variables in a generalized linear regression.
#'
#' @param data1 Subject-level dataframe with column such as age and gender. It must contain an `id` column unique for each subject.
#' @param data2 Trial-level dataframe. It must contain three columns: `id`, `response` and `rt`. It can also contain additional trial-level variables such as experiment condition.
#' @param model A list containing 0-5 formulas, specifying the dependence structure between variables.
#' @param family Family of distribution of `y`. Can be `gaussian`, `bernoulli` or `poisson`.
#' @param init Either `default` or other values supported by rstan (see rstan documentation)
#' @param prior Z losigtic value, specifying whether or not to use default prior for DDM parameters. By default, `prior` = TRUE.
#' @param stan_filename Z string specifying the automatically generated stan file name.
#'        if an empty string `''` is provided, a temporary file will be created and deleted after the model is fit.
#' @param gen_model Z logistic value indicating weather or not to generate the model.
#'        If not, RegDDM will not generate the code but use the existing stan model instead.
#' @param fit_model Z logistic value indicating weather or not to fit the model.
#'        If not, RegDDM will only generate the code and return the data input for stan.
#' @param ... Other parameters sent to rstan package.
#'
#'
#' @export
#'
#' @references To be added
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
    model = list(),
    family = "gaussian",
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
  # convert all non-numeric columns into factors
  data1 = dplyr::mutate(data1, dplyr::across(dplyr::where(~ !is.numeric(.)), as.factor))
  data2 = dplyr::mutate(data2, dplyr::across(dplyr::where(~ !is.numeric(.)), as.factor))

  # remove variables that is not included in the model for simplicity
  used_vars = c()
  for(f in model){
    used_vars = c(used_vars, as.character(attr(terms(f), "variables"))[-1])
  }
  for(tmp_col in colnames(data2)){
    if(tmp_col %in% c("id","rt","response")){
      next
    }
    if(!tmp_col %in% used_vars){
      data2 = dplyr::select(data2, -all_of(tmp_col))
    }
  }
  for(tmp_col in colnames(data1)){
    if(tmp_col == "id"){
      next
    }
    if(!tmp_col %in% used_vars){
      data1 = dplyr::select(data1, -all_of(tmp_col))
    }
  }

  # check for errors in the data
  check_data(data1, data2)

  # parse the model into better formats
  model = parse_model(model, data1, data2)

  # format the data into a list required by rstan
  stan_data = format_data(data1, data2)

  # check for errors in the model
  check_model(stan_data$x_names, stan_data$c_names, model)

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
      family,
      stan_filename
    )
  }
  rstan::stanc(stan_filename)

  # fit the stan model, if not, return the data input for stan
  if(!fit_model){
    return(stan_data)
  }

  # find the initialization for MCMC sampling
  if(is.character(init) && init == "default"){
    init_list = generate_default_initialization(data1, data2, model)
    init = replicate(chains, init_list, simplify = FALSE)
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

  # remove the temporary file if necessary
  if(delete_flag){
    file.remove(stan_filename)
  }

  # present the results in a better format
  output = summary_results(fit, model, data1)

  # print the useful information
  print(
    output$glm_coefficiets[,c(1,2,4,5,9,11)]
  )

  return(output)
}
