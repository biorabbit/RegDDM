#' Somehow stan does not support : in variable names thus we have to replace it with _.
#' @keywords internal
#' @noRd
replace_colon = function(var){
  return(stringr::str_replace_all(var, ":", "_"))
}




#' This function generates a model specified by the user
#' @keywords internal
#' @noRd
#'
generate_model = function(
    x_names,
    c_names,
    data1,
    model,
    #N,
    ddm_link,
    # rt_limits, # this is ugly but we need to specify the upper limit of reaction time for each subject (no longer needed)
    family
){
  # this function writes to a file containing the stan model
  # currently only support gaussian, bernoulli and poisson distribution
  if(!family %in% c("gaussian", "bernoulli", "poisson")){
    stop(stringr::str_interp("unsupported distribution family: ${family}\n"))
  }

  # summary the information for missing data (for data1 only)
  missing_info = list()
  for(c_name in c_names){
    dat = dplyr::arrange(data1,id)[[c_name]]
    dat_unique = unique(dat)
    n_mis = sum(is.na(dat))
    n_obs = length(dat) - sum(is.na(dat))
    missing_info[[c_name]] = list(
      n_mis = n_mis,
      n_obs = n_obs,
      iid_mis = which(is.na(dat)),
      iid_obs = which(!is.na(dat)),
      mean_hat = mean(data1, na.rm = TRUE),
      sd_hat = sqrt(var(data1, na.rm = TRUE)*n_obs/(n_obs-1)),
      is_binary = identical(sort(dat_unique), c(0,1))
    )
  }

  # internal function to append stan code to the model file
  add_script = function(str){
    str = stringr::str_interp(str)
    write(str, file = "stan_model.stan", append = TRUE)
  }

  # creating a file and start writing
  write("// RegDDM generated stan model", file = "stan_model.stan", append = FALSE)

  ########
  # data #
  ########
  add_script("")
  add_script("data {")
  add_script("  int<lower = 1> N; // total number of subjects")
  add_script("  // number of missing data for each covariate")
  for(c_name in c_names){
    if(missing_info[[c_name]][['n_mis']] == 0){
      next
    }
    add_script("  int<lower = 1> N_mis_${c_name};")
    add_script("  int<lower = 1> N_obs_${c_name};")
  }
  add_script("  int<lower = 1> n; // total number of trials")

  # subject level covariates
  add_script("")
  add_script("  // subject level covariates")
  for(c_name in c_names){
    if(missing_info[[c_name]][['is_binary']] == 1){
      data_type = "int<lower = 0, upper = 1>"
    }
    else{
      data_type = "real"
    }
    if(missing_info[[c_name]][['n_mis']] == 0){
      add_script("  ${data_type} ${c_name}[N];")
      next
    }
    add_script("  ${data_type} ${c_name}_obs[N_obs_${c_name}];")
    add_script("  int<lower = 0> ii_obs_${c_name}[N_obs_${c_name}];")
    if(missing_info[[c_name]][['n_mis']] == 1){
      add_script("  int<lower = 0> ii_mis_${c_name};")
    }
    else{
      add_script("  int<lower = 0> ii_mis_${c_name}[N_mis_${c_name}];")
    }
  }

  # for trial level variables
  add_script("")
  add_script("  // input for each trial")
  for(x_name in x_names){
    add_script("  vector[n] ${x_name};")
  }

  # for subject-level output y
  add_script("")
  add_script("  // output y")
  if(family == "gaussian"){
    add_script("  real y[N];")
  }
  else{
    add_script("  int y[N];")
  }


  # for response, response time and subject id of each trial
  add_script("  ")
  add_script("  // response, response time and subject id of each trial")
  add_script("  int response[n];")
  add_script("  real<lower = 0> rt[n]; ")
  add_script("  int id[n];")


  add_script("}")


  ##############
  # prarmeters #
  ##############
  add_script("")
  add_script("parameters {")

  # for regression parameters
  add_script("  // regression parameters of output y")
  add_script("  real beta_0;")
  for(predictor in attr(terms(model$y), "term.labels")){
    predictor = replace_colon(predictor)
    add_script("  real beta_${predictor};")
  }
  if(family == "gaussian"){
    add_script("  real<lower = 0> sigma_y;")
  }

  # group mean and sd of missing covariates
  add_script("")
  add_script("  // group mean and sd of missing covariates")
  for(c_name in c_names){
    if(missing_info[[c_name]][['n_mis']] == 0){
      next
    }
    if(missing_info[[c_name]][['is_binary']] == 1){
      add_script("  real p_${c_name};")
    }
    else{
      add_script("  real u_${c_name};")
      add_script("  real<lower = 0> sig_${c_name};")
    }

  }


  # for ddm group-level parameters
  add_script("  ")
  add_script("  // ddm group parameters")
  # add_script("  real<lower = 0> u_a_0;")
  # add_script("  real<lower = 0> sig_a_0;")
  # add_script("  real u_v_0;")
  # add_script("  real<lower = 0> sig_v_0;")
  # add_script("  real<lower = 0, upper = 1> u_z_0;")
  # add_script("  real<lower = 0> sig_z_0;")
  # add_script("  real<lower = 0> u_t_0;")
  # add_script("  real<lower = 0> sig_t_0;")
  for(param in c("a", "t", "z", "v")){
    tmp_term = attr(terms(model[[param]]), "term.labels")
    tmp_term = c("0", tmp_term) # this is for intercept
    for(predictor in tmp_term){
      predictor = replace_colon(predictor)
      add_script("  real u_${param}_${predictor};")
      add_script("  real<lower = 0> sig_${param}_${predictor};")
      add_script("  real ${param}_${predictor}[N];")
    }
  }


  # for subject-level DDM parameters (a_0, z_0, t_0, v_0)
  # add_script("")
  # add_script("  // ddm parameters")
  # add_script("  real a_0[N];")
  # add_script("  real z_0[N];")
  # add_script("  real v_0[N];")
  # add_script("  real t_0[N];")
  # ugly code to define subject-specific non-decision time t limits (no longer needed)
  #for(i in 1:N){
  #  add_script("  real<lower = 0, upper = ${rt_limits[i]}> t_0_${i};")
  #}
  # trial-level variables' influence on DDM parameters
  # for(param in c("a", "t", "z", "v")){
  #   for(predictor in model[param][[1]]){
  #     add_script("  real ${param}_${predictor}[N];")
  #   }
  # }

  # missing data of covariates
  add_script("")
  add_script("  // missing covariate data")
  for(c_name in c_names){
    if(missing_info[[c_name]][['n_mis']] == 0){
      next
    }
    if(missing_info[[c_name]][['is_binary']] == 1){
      data_type = "int<lower = 0, upper = 1>"
    }
    else{
      data_type = "real"
    }
    if(missing_info[[c_name]][['n_mis']] == 1){
      add_script("  ${data_type} ${c_name}_mis;")
    }
    else{
      add_script("  ${data_type} ${c_name}_mis[N_mis_${c_name}];")
    }
  }

  add_script("}")

  #########################
  # transformed parameters#
  #########################
  add_script("")
  add_script("transformed parameters {")

  # ugly code to define subject-specific non-decision time t limits (no longer needed)
  #add_script("  vector[N] t_0;")
  #for(i in 1:N){
  #  add_script("  t_0[${i}] = t_0_${i};")
  #}

  # using the ddm_link to link the linear predictor with the ddm parameters
  add_script("  // transformation of baseline ddm parameter to apply prior distribution")
  add_script("  real a_0_transformed[N];")
  add_script("  real z_0_transformed[N];")
  add_script("  real v_0_transformed[N];")
  add_script("  real t_0_transformed[N];")
  add_script("  for(i in 1:N){")
    add_script("    a_0_transformed[i] = ${ddm_link[['a']]}(a_0[i]);")
    add_script("    z_0_transformed[i] = ${ddm_link[['z']]}(z_0[i]);")
    add_script("    v_0_transformed[i] = ${ddm_link[['v']]}(v_0[i]);")
    add_script("    t_0_transformed[i] = ${ddm_link[['t']]}(t_0[i]);")
  add_script("  }")

  # combining the missing and observed covariates
  add_script("  ")
  add_script("  // combining missing and observed covariate data")
  for(c_name in c_names){
    if(missing_info[[c_name]][['n_mis']] == 0){
      next
    }
    if(missing_info[[c_name]][['is_binary']] == 1){
      data_type = "int<lower = 0, upper = 1>"
    }
    else{
      data_type = "real"
    }
    add_script("  ${data_type} ${c_name}[N];")
    add_script("  ${c_name}[ii_obs_${c_name}] = ${c_name}_obs;")
    add_script("  ${c_name}[ii_mis_${c_name}] = ${c_name}_mis;")
  }

  add_script("}")

  #########
  # model #
  #########
  add_script("")
  add_script("model {")

  # priors.
  # priors for ddm group parameters, adopted from HDDM
  add_script("  // priors for ddm group parameters")
  add_script("  u_a_0 ~ gamma(1.125, 0.75);")
  add_script("  sig_a_0 ~ normal(0, 0.1);")
  add_script("  u_v_0 ~ normal(2, 3);")
  add_script("  sig_v_0  ~ normal(0, 2);")
  add_script("  u_z_0 ~ normal(0.5, 0.5);")
  add_script("  sig_z_0 ~ normal(0, 0.05);")
  add_script("  u_t_0 ~ gamma(0.08, 0.2);")
  add_script("  sig_t_0 ~ normal(0, 1);")

  # modeling missing covariates
  add_script("  ")
  add_script("  // modeling missing covariates")
  for(c_name in c_names){
    if(missing_info[[c_name]][['n_mis']] == 0){
      next
    }
    add_script("  for(i in 1:N){")
    if(missing_info[[c_name]][['is_binary']] == 1){
      add_script("    ${c_name}[i] ~ bernoulli(p_${c_name});")
    }
    else{
      add_script("    ${c_name}[i] ~ normal(u_${c_name}, sig_${c_name});")
    }
    add_script("  }")
  }


  # the distribution of ddm parameter for each subject at baseline experiment condition
  add_script("  ")
  add_script("  // priors for each subject")
  add_script("  for(i in 1:N){")
  add_script("    a_0_transformed[i] ~ normal(u_a_0, sig_a_0);")
  add_script("    z_0_transformed[i] ~ normal(u_z_0, sig_z_0);")
  add_script("    t_0_transformed[i] ~ normal(u_t_0, sig_t_0);")
  add_script("    v_0_transformed[i] ~ normal(u_v_0, sig_v_0);")

  # and for their sensitivity to changes in the conditions
  for(param in c("a", "t", "z", "v")){
    for(predictor in attr(terms(model[[param]]), "term.labels")){
      predictor = replace_colon(predictor)
      add_script("    ${param}_${predictor}[i] ~ normal(u_${param}_${predictor}, sig_${param}_${predictor});")
    }
  }
  add_script("  }")

  # ddm part of the model
  add_script("  ")
  add_script("  // ddm parameter for each trial")
  add_script("  real a[n];")
  add_script("  real z[n];")
  add_script("  real t[n];")
  add_script("  real v[n];")

  add_script("  ")
  add_script("  // model RT for each trial using WFPT distribution")
  add_script("  for(j in 1:n){")
  add_script("    int i;")
  add_script("    i = id[j];")
  for(param in c("a", "z", "t", "v")){
    tmp_term = attr(terms(model[[param]]), "term.labels")
    str = stringr::str_interp("    ${param}[j] = ${ddm_link[[param]]}(${param}_0[i]")
    for(predictor in tmp_term){
      tmp_str = stringr::str_c(
        stringr::str_interp("${param}_"),predictor,"[i]*",stringr::str_replace_all(predictor, ":", "[j]*")
      )
      str = stringr::str_c(str, stringr::str_interp(" + ${tmp_str}[j]"))
    }
    str = stringr::str_c(str, ");")
    str = replace_colon(str)
    add_script(str)
  }
  add_script("    ")
  add_script("    if(response[j] == 1){")
  add_script("      target += wiener_lpdf(rt[j] | a[j], t[j], z[j], v[j]);")
  add_script("    }")
  add_script("    else {")
  add_script("      target += wiener_lpdf(rt[j] | a[j], t[j], 1 - z[j], -v[j]);")
  add_script("    }")
  add_script("  }")

  # regression part of the model
  add_script("  ")
  add_script("  // generalized linear regression part of the model")
  add_script("  real eta[N]; // linear predictor")
  add_script("  for(i in 1:N){")
  # # previous linear model, no longer needed
  # str = "    y[i] ~ normal(beta_0"
  # for(predictor in model$y){
  #   str = stringr::str_c(str, stringr::str_interp(" + beta_${predictor}*${predictor}[i]"))
  # }
  # str = stringr::str_c(str, ", sigma_y);")
  # add_script(str)
  tmp_term = attr(terms(model[["y"]]), "term.labels")
  str = "    eta[i] = beta_0"
  for(predictor in tmp_term){
    tmp_str = stringr::str_c(
      "beta_",predictor,"*",stringr::str_replace_all(predictor, ":", "[i]*")
    )
    str = stringr::str_c(str, stringr::str_interp(" + ${tmp_str}[i]"))
  }
  str = stringr::str_c(str, ";")
  str = replace_colon(str)
  add_script(str)
  if(family == "gaussian"){
    add_script("    y[i] ~ normal(eta[i], sigma_y);")
  }
  else if(family == "bernoulli"){
    add_script("    y[i] ~ bernoulli(inv_logit(eta[i]));")
  }
  else if(family == "poisson"){
    add_script("    y[i] ~ poisson(exp(eta[i]));")
  }
  add_script("  }")

  add_script("}")

  rstan::stanc("stan_model.stan")
}
