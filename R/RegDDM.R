#' Main function of RegDDM
#'
#'
#'
#'
#' @export
regddm = function(
    data1,
    data2,
    model = default_model,
    init = "default",
    scale = FALSE,
    chains = 4,
    cores = 4,
    thin = 1,
    ...
){

  if(scale)(
    # code to scale variables
    1
  )

  # check for errors in the data
  check_data(data1, data2)

  # check for errors in the model
  xvar = colnames(data2)
  xvar = xvar[!xvar %in% c("id","response","rt")]
  cvar = colnames(data1)
  cvar = cvar[!cvar %in% c("id", "y")]
  check_model(xvar, cvar, model)

  # format the data into a list required by rstan
  stan_data = format_data(data1, data2)

  # automatically generate the stan model
  N = stan_data[["N"]]
  rt_limits = dplyr::group_by(data2, id)
  rt_limits = dplyr::summarise(rt_limits, rt = min(rt))
  rt_limits = dplyr::pull(rt_limits, rt)
  generate_model(xvar, cvar, model, N, rt_limits)

  # find the initialization for MCMC sampling
  if(init == "default"){
    init_list = list(t_0 = rep(0.01, N), a_0 = rep(1, N), v_0 = rep(1, N), z_0 = rep(0.5, N))
    init = replicate(chains, init_list, simplify = FALSE)
  }

  # fit the stan model
  stan_model <- "stan_model.stan"
  fit <- rstan::stan(
    file = stan_model,
    data = stan_data,
    init=init,
    chains = chains,
    cores = cores,
    thin = thin,
    ...
  )

  # tmp = round(summary(fit)$summary, 3)

  return(fit)

}
