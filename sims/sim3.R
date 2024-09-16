.libPaths(c("/home/jinzekai/R_libs/"))

# This function is used to generate the second simulation result in the paper
# Under different number of trials and subjects per subject, compare the
# accuracy of posterior estimates of GLM regression parameters
# Each simulates an experiment with 0, one or two trial-level variables.
N = 30
n_each = 30
out_file = "test2.csv"
n_xvar = 1
simulate_experiment = function(
    N, # number of subjects
    n_each, # number of trials for each subject
    n_xvar, # number of trial-level variables to include
    out_file # which .csv file to append the simulate results.
){
  fake_data = RegDDM::generate_fake_data(
    N = N,
    beta_0 = 0,
    beta_c1 = 0,
    beta_c2 = 0,
    beta_v_0 = 0,
    beta_v_x1 = 0,
    beta_v_x2 = 0,
    sigma_y = 1,
    sigma_v = 0,
    n_xvar = n_xvar,
    n_each = n_each,
    y_family = "gaussian"
  )
  if(n_xvar == 0){
    model = list(
      y ~ v_0
    )
  }
  if(n_xvar == 1){
    model = list(
      v ~ x1,
      y ~ v_0 + v_x1
    )
  }
  if(n_xvar == 2){
    model = list(
      v ~ x1 + x2,
      y ~ v_0 + v_x1 + v_x2
    )
  }
  time_taken = system.time({
    fit = RegDDM::regddm(
      fake_data[["data1"]],
      fake_data[["data2"]],
      model,
      stan_filename = "",
      warmup = 500,
      iter = 1000
    )
  })["elapsed"]

  # compare the regression parameters estimated using RegDDM and two-step approaches
  res = fit$glm_coefficiets


  summary_stat = stringr::str_c(
    N,
    n_each,
    n_xvar,
    time_taken,
    max(as.data.frame(rstan::summary(fit$stan_fit)$summary)$Rhat),

    mean((fit$subject_ddm_param$mean$a_0 - fake_data$data1_true$a_0)^2),
    mean((fit$subject_ddm_param$mean$t_0 - fake_data$data1_true$t_0)^2),
    mean((fit$subject_ddm_param$mean$z_0 - fake_data$data1_true$z_0)^2),
    mean((fit$subject_ddm_param$mean$v_0 - fake_data$data1_true$v_0)^2),
    ifelse(n_xvar >=1, mean((fit$subject_ddm_param$mean$v_x1 - fake_data$data1_true$v_x1)^2), ""),
    ifelse(n_xvar ==2, mean((fit$subject_ddm_param$mean$v_x2 - fake_data$data1_true$v_x2)^2), ""),

    res$mean[2],
    res$sd[2],
    res$`2.5%`[2],
    res$`97.5%`[2],

    ifelse(n_xvar >= 1, res$mean[3], ""),
    ifelse(n_xvar >= 1, res$sd[3], ""),
    ifelse(n_xvar >= 1, res$`2.5%`[3], ""),
    ifelse(n_xvar >= 1, res$`97.5%`[3], ""),

    ifelse(n_xvar == 2, res$mean[4], ""),
    ifelse(n_xvar == 2, res$sd[4], ""),
    ifelse(n_xvar == 2, res$`2.5%`[4], ""),
    ifelse(n_xvar == 2, res$`97.5%`[4], ""),

    sep = ","
  )

  write(summary_stat,out_file,append = TRUE)

}

args <- commandArgs(trailingOnly = TRUE)

for(i in 1:as.numeric(args[4])){
  simulate_experiment(
    as.numeric(args[1]),
    as.numeric(args[2]),
    0,
    args[3]
  )
  simulate_experiment(
    as.numeric(args[1]),
    as.numeric(args[2]),
    1,
    args[3]
  )
  simulate_experiment(
    as.numeric(args[1]),
    as.numeric(args[2]),
    2,
    args[3]
  )
}

"
N,n_each,n_xvar,time_taken,max_rhat,mse_a_0,mse_t_0,mse_z_0,mse_v_0,mse_v_x1,mse_v_x2,mean_v_0,sd_v_0,cril_v_0,crih_v_0,mean_v_x1,sd_v_x1,cril_v_x1,crih_v_x1,mean_v_x2,sd_v_x2,cril_v_x2,crih_v_x2
"
