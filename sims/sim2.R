.libPaths(c("/home/jinzekai/R_libs/"))

# This function is used to generate the first simulation result in the paper
# Under different number of trials per subject, compare the false discover rate
# between RegDDM and 2-step approaches, as well as MSE for ddm parameter estimates.
# Each simulates an experiment with two trial-level variables.
# One of them is related to y, the other one is not.
# N = 30
# n_each = 30
# out_file = "test2.csv"
simulate_experiment = function(
    N, # number of subjects
    n_each, # number of trials for each subject
    out_file # which .csv file to append the simulate results.
){
  fake_data = RegDDM::generate_fake_data(
    N = N,
    beta_0 = 0,
    beta_c1 = 0,
    beta_c2 = 0,
    beta_v_0 = 0,
    beta_v_x1 = 1,
    beta_v_x2 = 0,
    sigma_y = 1,
    sigma_v = 0,
    n_xvar = 2,
    n_each = n_each,
    y_family = "gaussian"
  )

  model = list(
    v ~ x1 + x2,
    y ~ v_x1 + v_x2
  )

  fit = RegDDM::regddm(
    fake_data[["data1"]],
    fake_data[["data2"]],
    model,
    stan_filename = "",
    fit_model = TRUE,
    warmup = 500,
    iter = 1000
  )

  model_2step = list(
    v~ x1 + x2,
    y ~ 1
  )

  fit_2step = RegDDM::regddm(
    fake_data[["data1"]],
    fake_data[["data2"]],
    model_2step,
    stan_filename = "",
    warmup = 500,
    iter = 1000
  )


  # compare the regression parameters estimated using RegDDM and two-step approaches
  res = fit$glm_coefficiets

  # fit linear model using posterior of first model
  lm_data = dplyr::mutate(
    fit$subject_ddm_param$mean,
    y = fake_data[["data1"]][["y"]]
  )

  res_2step = broom::tidy(
    lm(y ~ v_x1 + v_x2, lm_data),conf.int = TRUE, conf.level = 0.95
  )


  # fit linear model using posterior of second model
  lm_data2 = dplyr::mutate(
    fit_2step$subject_ddm_param$mean,
    y = fake_data[["data1"]][["y"]]
  )

  res_2step2 = broom::tidy(
    lm(y ~ v_x1 + v_x2, lm_data2),conf.int = TRUE, conf.level = 0.95
  )

  summary_stat = stringr::str_c(
    N,
    n_each,
    max(as.data.frame(rstan::summary(fit$stan_fit)$summary)$Rhat),
    max(as.data.frame(rstan::summary(fit_2step$stan_fit)$summary)$Rhat),

    mean((fit$subject_ddm_param$mean$v_x1 - fake_data$data1_true$v_x1)^2),
    mean((fit_2step$subject_ddm_param$mean$v_x1 - fake_data$data1_true$v_x1)^2),
    res$mean[2],
    res$`2.5%`[2],
    res$`97.5%`[2],
    res_2step$estimate[2],
    res_2step$conf.low[2],
    res_2step$conf.high[2],
    res_2step2$estimate[2],
    res_2step2$conf.low[2],
    res_2step2$conf.high[2],

    mean((fit$subject_ddm_param$mean$v_x2 - fake_data$data1_true$v_x2)^2),
    mean((fit_2step$subject_ddm_param$mean$v_x2 - fake_data$data1_true$v_x2)^2),
    res$mean[3],
    res$`2.5%`[3],
    res$`97.5%`[3],
    res_2step$estimate[3],
    res_2step$conf.low[3],
    res_2step$conf.high[3],
    res_2step2$estimate[3],
    res_2step2$conf.low[3],
    res_2step2$conf.high[3],
    sep = ","
  )

  write(summary_stat,out_file,append = TRUE)

}

args <- commandArgs(trailingOnly = TRUE)

for(i in 1:as.numeric(args[4])){
  simulate_experiment(
    as.numeric(args[1]),
    as.numeric(args[2]),
    args[3]
  )
}


"
N, n_each, max_rhat_1, max_rhat_2, mse_v_x1_1, mse_v_x1_2, mean_beta_v_x1, cril_beta_v_x1, crih_beta_v_x1, est_beta_v_x1_m1, cil_beta_v_x1_m1, cih_beta_v_x1_m1, est_beta_v_x1_m2, cil_beta_v_x1_m2, cih_beta_v_x1_m2, mse_v_x2_1, mse_v_x2_2, mean_beta_v_x2, cril_beta_v_x2, crih_beta_v_x2, est_beta_v_x2_m1, cil_beta_v_x2_m1, cih_beta_v_x2_m1, est_beta_v_x2_m2, cil_beta_v_x2_m2, cih_beta_v_x2_m2,
"
