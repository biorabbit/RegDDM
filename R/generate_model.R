#' This function generates a model specified by the user
#'
#'
#'
#'
#' @export
generate_model = function(
    x_names,
    c_names,
    model,
    N,
    rt_limits # this is ugly but we need to specify the upper limit of reaction time for each subject
){
  # this function writes to a file containing the stan model
  add_script = function(str){
    str = stringr::str_interp(str)
    write(str, file = "stan_model.stan", append = TRUE)
  }

  # creating a file and start writing
  write("// generated stan model for hddm_regression", file = "stan_model.stan", append = FALSE)


  # start building the model
  add_script("")
  add_script("data {")
  add_script("  int N; // total number of subjects")
  add_script("  int n; // total number of trials")

  ########
  # data #
  ########

  # for subject level covariates
  add_script("")
  add_script("  // subject level covariates")
  for(c_name in c_names){
    add_script("  vector[N] ${c_name};")
  }

  # for trial level inputs
  add_script("")
  add_script("  // input for each trial")
  for(x_name in x_names){
    add_script("  vector[n] ${x_name};")
  }

  # for output y
  add_script("")
  add_script("  // output y")
  add_script("  vector[N] y;")


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
  add_script("  ")
  add_script("  // for output y")
  add_script("  real beta_0;")
  for(predictor in model$y){
    add_script("  real beta_${predictor};")
  }
  add_script("  real<lower = 0> sigma_y;")

  # for ddm group-level parameters
  add_script("  // ddm group parameters")
  add_script("  real<lower = 0> u_a_0;")
  add_script("  real<lower = 0> sig_a_0;")
  add_script("  real u_v_0;")
  add_script("  real<lower = 0> sig_v_0;")
  add_script("  real<lower = 0, upper = 1> u_z_0;")
  add_script("  real<lower = 0> sig_z_0;")
  add_script("  real<lower = 0> u_t_0;")
  add_script("  real<lower = 0> sig_t_0;")
  for(param in c("a", "t", "z", "v")){
    for(predictor in model[param][[1]]){
      add_script("  real u_${param}_${predictor};")
      add_script("  real<lower = 0> sig_${param}_${predictor};")
    }
  }


  # for subject-level DDM parameters (a_0, z_0, t_0, v_0)
  add_script("")
  add_script("  // ddm parameters")
  add_script("  real<lower = 0> a_0[N];")
  add_script("  real<lower = 0, upper = 1> z_0[N];")
  add_script("  real v_0[N];")
  # ugly code to define subject-specific non-decision time t limits
  for(i in 1:N){
    add_script("  real<lower = 0, upper = ${rt_limits[i]}> t_0_${i};")
  }
  # trial-level variables' influence on DDM parameters
  for(param in c("a", "t", "z", "v")){
    for(predictor in model[param][[1]]){
      add_script("  real ${param}_${predictor}[N];")
    }
  }

  add_script("}")

  #########################
  # transformed parameters#
  #########################
  # ugly code to define subject-specific non-decision time t limits
  add_script("")
  add_script("transformed parameters {")
  add_script("  vector[N] t_0;")
  for(i in 1:N){
    add_script("  t_0[${i}] = t_0_${i};")
  }
  add_script("}")

  #########
  # model #
  #########
  add_script("")
  add_script("model {")

  # priors.
  # priors for ddm group parameters
  add_script("  // priors for ddm group parameters")
  add_script("  u_a_0 ~ gamma(1.125, 0.75);")
  add_script("  sig_a_0 ~ normal(0, 0.1);")
  add_script("  u_v_0 ~ normal(2, 3);")
  add_script("  sig_v_0  ~ normal(0, 2);")
  add_script("  u_z_0 ~ normal(0.5, 0.5);")
  add_script("  sig_z_0 ~ normal(0, 0.05);")
  add_script("  u_t_0 ~ gamma(0.08, 0.2);")
  add_script("  sig_t_0 ~ normal(0, 1);")



  add_script("  // priors for each subject")
  add_script("  for(i in 1:N){")
  add_script("    a_0[i] ~ normal(u_a_0, sig_a_0);")
  add_script("    z_0[i] ~ normal(u_z_0, sig_z_0);")
  add_script("    t_0[i] ~ normal(u_t_0, sig_t_0);")
  add_script("    v_0[i] ~ normal(u_v_0, sig_v_0);")
  for(param in c("a", "t", "z", "v")){
    for(predictor in model[param][[1]]){
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
  add_script("  // for each trial")
  add_script("  for(j in 1:n){")
  add_script("    int i;")
  add_script("    i = id[j];")
  for(param in c("a", "z", "t", "v")){
    str = stringr::str_interp("    ${param}[j] = ${param}_0[i]")
    for(predictor in model[param][[1]]){
      str = stringr::str_c(str, stringr::str_interp(" + ${param}_${predictor}[i]*${predictor}[j]"))
    }
    str = stringr::str_c(str, ";")
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
  add_script("  // regression part of the model")
  add_script("  for(i in 1:N){")
  str = "    y[i] ~ normal(beta_0"
  for(predictor in model$y){
    str = stringr::str_c(str, stringr::str_interp(" + beta_${predictor}*${predictor}[i]"))
  }
  str = stringr::str_c(str, ", sigma_y);")
  add_script(str)
  add_script("  }")

  add_script("}")

  rstan::stanc("stan_model.stan")
}
