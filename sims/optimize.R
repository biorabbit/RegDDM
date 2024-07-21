.libPaths(c(
  "/ifs/home/msph/LeeLab/zj2357/R_libs",
  "/ifs/scratch/msph/software/R/library422",
  "/home/jinzekai/R_libs/"
))

generate_fake_data <- function(
    N = 30,
    beta_0 = 0,
    beta_v_x1 = 1,
    beta_v_x2 = 1,
    sigma_y,
    n_xvar = 2,
    n_each = 100
){
  x1 = runif(n_each*N, -1, 1)
  x2 = runif(n_each*N, -1, 1)


  data1_true =
    dplyr::tibble(
      id = 1:N,
      t_0 = runif(N, 0.1, 0.4),
      a_0 = runif(N, 1, 3),
      z_0 = runif(N, 0.2, 0.9),
      v_0 = rnorm(N, 1, 0.5),
      v_x1 = rnorm(N)*ifelse(n_xvar >= 1, 1, 0), # first trial-level variable
      v_x2 = rnorm(N)*ifelse(n_xvar == 2, 1, 0), # second trial-level variable
      y = rep(beta_0, N) + beta_v_x1*v_x1 + beta_v_x2*v_x2 + rnorm(N, 0, sigma_y),
    )

  data2_true = dplyr::bind_cols(x1, x2)
  colnames(data2_true) = c("x1", "x2")

  # this function calculates drift rate given trial-level variables
  # by default, we assume the drift rate to be fully determined
  calc_v = function(v_0, v_x1, v_x2, x1, x2, sigma_v = 0){
    return(v_0 + v_x1*x1 + v_x2*x2 + rnorm(1, 0, sigma_v))
  }

  # this function simulates DDM using easyRT
  r_ddm = function(a,z,t,v){
    tmp = rtdists::rdiffusion(n = 1, a = a, v = v, z = z, t0 = t, st0 = 0.001)
    return(tmp)
  }

  # generate fake response and reaction time for each trial.
  # these are the trial-level true parameters with output
  data2_true =
    dplyr::mutate(
      data2_true,
      id = rep(1:N, each = n_each),
      t = rep(data1_true$t_0, each = n_each),
      a = rep(data1_true$a_0, each = n_each),
      z = rep(data1_true$z_0, each = n_each),
      v_0 = rep(data1_true$v_0, each = n_each),
      v_x1 = rep(data1_true$v_x1, each = n_each),
      v_x2 = rep(data1_true$v_x2, each = n_each),
      v = purrr::pmap(list(v_0, v_x1, v_x2, x1, x2), calc_v),
      ddm = purrr::pmap(list(a,z,t,v), r_ddm)
    )

  data2_true =
    tidyr::unnest(data2_true, ddm)

  data2_true =
    dplyr::mutate(
      data2_true,
      response = ifelse(response == "upper", 1, 0)
    )

  data1 = dplyr::select(data1_true, id, y)
  data2 = dplyr::select(data2_true, id, x1, x2, rt, response)

  return(list(
    data1 = data1,
    data2 = data2,
    data1_true = data1_true,
    data2_true = data2_true
  ))
}



N=10
n_each = 50
n_xvar = 2
outcome_link = 2
outcome_model = 2
out_file = "NA"
mode = 4
# significance level a = 0.1
simulate_experiment = function(
    N, # number of subjects
    n_each, # number of trials for each subject
    n_xvar, # v is determined by nothing (0), x1 (1) or x1+x2 (2)
    outcome_link, # either outcome y is influenced by nothing (0), x1 (1) or x1+x2 (2)
    outcome_model, # either outcome y is modeled using nothing (0), v_x1 (1) or v_x1+v_x2 (2)
    out_file, # which .csv file to append the simulate results.
    mode # 1: normal 2: +0.01, 3: -0.01, 4:good init
){
  print(N)
  beta_0_true = 0
  beta_v_x1_true = ifelse(outcome_link >=1, 1, 0)
  beta_v_x2_true = ifelse(outcome_link ==2, 1, 0)
  sigma_y = 0.5


  fake_data = generate_fake_data(
    N,
    beta_0_true,
    beta_v_x1_true,
    beta_v_x2_true,
    sigma_y,
    n_xvar,
    n_each
  )

  v_formula = NA
  if(n_xvar == 0){
    v_formula = v ~ 1
  }
  else if (n_xvar == 1){
    v_formula = v ~ x1
  }
  else if (n_xvar == 2){
    v_formula = v ~ x1 + x2
  }

  y_formula = NA
  if(outcome_model == 0){
    y_formula = y ~ 1
  }
  else if (outcome_model == 1){
    y_formula = y ~ v_x1
  }
  else if (outcome_model == 2){
    y_formula = y ~ v_x1 + v_x2
  }
  model = list(
    v_formula, y_formula
  )

  logit = function(x){
    return(log(x/(1-x)))
  }

  init_list = list(
    t_0 = log(fake_data[["data1_true"]]$t_0),
    a_0 = log(fake_data[["data1_true"]]$a_0),
    v_0 = fake_data[["data1_true"]]$v_0,
    z_0 = logit(fake_data[["data1_true"]]$z_0),
    v_x1 = fake_data[["data1_true"]]$v_x1,
    v_x2 = fake_data[["data1_true"]]$v_x2,
    beta_0 = 0,
    beta_v_x1 = 1,
    beta_v_x2 = 1,
    u_a_0 = 2,
    sig_a_0 = 2/sqrt(12),
    u_t_0 = 0.25,
    sig_t_0 = 0.3/sqrt(12),
    u_z_0 = 0.55,
    sig_z_0 = 0.7/sqrt(12),
    u_v_0 = 1,
    sig_v_0 = 0.5,
    u_v_x1 = 0,
    sig_v_x1 = 1,
    u_v_x2 = 0,
    sig_v_x2 = 1,
    sigma_y = 0.5
  )
  init_true = replicate(4, init_list, simplify = FALSE)

  ddm_link1 = list(
    a = "exp",
    t = "0.01+exp",
    z = "inv_logit",
    v = ""
  )

  ddm_link2 = list(
    a = "exp",
    t = "0.01+exp",
    z = "inv_logit",
    v = ""
  )

  init = NA
  ddm_link = NA

  if(mode == 1){
    init = "default"
    ddm_link = "default"
  }
  else if(mode == 2){
    init = "default"
    ddm_link = ddm_link1
  }
  else if(mode == 3){
    init = "default"
    ddm_link = ddm_link2
  }
  else if(mode == 4){
    init = init_true
    ddm_link = "default"
  }
  else if (mode == 5){
    init_list = list(t_0 = log(rep(0.1, N)), a_0 = log(rep(1, N)), v_0 = rep(1, N), z_0 = rep(0, N), u_t_0 = 0.2, sig_t_0 = 0.4)
    init = replicate(4, init_list, simplify = FALSE)
    ddm_link = "default"
  }


  time_taken = system.time({
  fit = RegDDM::regddm(
    fake_data[["data1"]],
    fake_data[["data2"]],
    model,
    family = "gaussian",
    init = init,
    ddm_link = ddm_link,
    scale = FALSE,
    warmup = 500,
    iter = 1000
  )})

  res = rstan::summary(fit)$summary
  res = as.data.frame(res)

  figure_dir = "figures1/"
  for(predictor in rownames(res)){
    if(stringr::str_detect(predictor,"transformed")){
      next
    }
    fig = rstan::traceplot(fit, pars = predictor, inc_warmup = TRUE)
    ggplot2::ggsave(paste0(figure_dir,predictor,".png"),fig)
  }

  return(0)

  dat_2step = dplyr::select(fake_data[["data1"]],y)

  if(n_xvar >= 1){
    dat_2step = dplyr::bind_cols(dat_2step, dplyr::tibble(
      v_x1 =res[stringr::str_detect(rownames(res),"^v_x1\\[\\d+\\]$"),1]
    ))
  }
  if(n_xvar == 2){
    dat_2step = dplyr::bind_cols(dat_2step, dplyr::tibble(
      v_x2 =res[stringr::str_detect(rownames(res),"^v_x2\\[\\d+\\]$"),1]
    ))
  }

  res_2step = broom::tidy(
    lm(y_formula, dat_2step),conf.int = TRUE, conf.level = 0.95
  )




  mse_a_0 = mean(
    (fake_data[["data1_true"]][["a_0"]]-
       res[stringr::str_detect(rownames(res),"^a_0_transformed\\[\\d+\\]$"),1])^2
  )
  mse_t_0 = mean(
    (fake_data[["data1_true"]][["t_0"]]-
       res[stringr::str_detect(rownames(res),"^t_0_transformed\\[\\d+\\]$"),1])^2
  )
  mse_z_0 = mean(
    (fake_data[["data1_true"]][["z_0"]]-
       res[stringr::str_detect(rownames(res),"^z_0_transformed\\[\\d+\\]$"),1])^2
  )
  mse_v_0 = mean(
    (fake_data[["data1_true"]][["v_0"]]-
       res[stringr::str_detect(rownames(res),"^v_0_transformed\\[\\d+\\]$"),1])^2
  )
  mse_v_x1 = mean(
    (fake_data[["data1_true"]][["v_x1"]]-
       res[stringr::str_detect(rownames(res),"^v_x1\\[\\d+\\]$"),1])^2
  )
  mse_v_x2 = mean(
    (fake_data[["data1_true"]][["v_x2"]]-
       res[stringr::str_detect(rownames(res),"^v_x2\\[\\d+\\]$"),1])^2
  )


  summary_stat = stringr::str_c(
    N,
    n_each,
    n_xvar,
    outcome_link,
    outcome_model,
    mode,
    time_taken["elapsed"],
    max(res[["Rhat"]]),

    # to analyze the difference between true ddm parameter and RegDDM estimates
    mse_a_0,
    mse_t_0,
    mse_z_0,
    mse_v_0,
    mse_v_x1,
    mse_v_x2,
    # to analyze the posterior of regression coefficients, the true value and the estimation from two-step approach
    beta_0_true,
    res_2step[1,2],
    res_2step[1,3],
    res_2step[1,6],
    res_2step[1,7],
    ifelse(res_2step[1,5]<0.1, 1, 0),
    res[1,1],
    res[1,3],
    res[1,4],
    res[1,8],

    beta_v_x1_true,
    res_2step[2,2],
    res_2step[2,3],
    res_2step[2,6],
    res_2step[2,7],
    ifelse(res_2step[2,5]<0.1, 1, 0),
    ifelse(outcome_model >=1, res[2,1], NA),
    ifelse(outcome_model >=1, res[2,3], NA),
    ifelse(outcome_model >=1, res[2,4], NA),
    ifelse(outcome_model >=1, res[2,8], NA),

    beta_v_x2_true,
    res_2step[3,2],
    res_2step[3,3],
    res_2step[3,6],
    res_2step[3,7],
    ifelse(res_2step[3,5]<0.1, 1, 0),
    ifelse(outcome_model ==2, res[3,1], NA),
    ifelse(outcome_model ==2, res[3,3], NA),
    ifelse(outcome_model ==2, res[3,4], NA),
    ifelse(outcome_model ==2, res[3,8], NA),

    sigma_y,

    sep = ","
  )

  write(summary_stat,out_file,append = TRUE)

}

args <- commandArgs(trailingOnly = TRUE)

for(i in 1:as.numeric(args[7])){
  simulate_experiment(
    as.numeric(args[1]),
    as.numeric(args[2]),
    as.numeric(args[3]),
    as.numeric(args[4]),
    as.numeric(args[5]),
    args[6],
    args[8]
  )
}

#fit = simulate_experiment(
#  20,
#  200,
#  2,
#  2,
#  2,
#  "E:/tmp.csv",
#  4
#)
#
#
#
#ttt = rstan::summary(fit)$summary %>% as.data.frame()
#figure_dir = "sims/figures/"
#for(predictor in rownames(ttt)){
#  if(str_detect(predictor,"transformed")){
#    next
#  }
#  fig = traceplot(fit, pars = predictor, inc_warmup = TRUE)
#  ggsave(paste0(figure_dir,predictor,".png"),fig)
#}

"
N,n_each,n_xvar,outcome_link,outcome_model,mode,time_taken,max_r_hat,mse_a_0,mse_t_0,mse_z_0,mse_v_0,mse_v_x1,mse_v_x2,beta_0_true,beta_0_2step_mean,beta_0_2step_se,beta_0_2step_cil,beta_0_2step_cih,beta_0_2step_sig,beta_0_mean,beta_0_sd,beta_0_cil,beta_0_cih,beta_v_x1_true,beta_v_x1_2step_mean,beta_v_x1_2step_se,beta_v_x1_2step_cil,beta_v_x1_2step_cih,beta_v_x1_2step_sig,beta_v_x1_mean,beta_v_x1_sd,beta_v_x1_cil,beta_v_x1_cih,beta_v_x2_true,beta_v_x2_2step_mean,beta_v_x2_2step_se,beta_v_x2_2step_cil,beta_v_x2_2step_cih,beta_v_x2_2step_sig,beta_v_x2_mean,beta_v_x2_sd,beta_v_x2_cil,beta_v_x2_cih,sigma_y
"
