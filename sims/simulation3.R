.libPaths(c(
  "/ifs/home/msph/LeeLab/zj2357/R_libs",
  "/ifs/scratch/msph/software/R/library422",
  "/home/jinzekai/R_libs/"
))

generate_fake_data <- function(
    N = 30,
    beta_0 = 0,
    beta_v_0 = 1,
    sigma_y = 1,
    n_each = 100,
    cont_level = 0
){

  data1_true =
    dplyr::tibble(
      id = 1:N,
      t_0 = runif(N, 0.1, 0.4),
      a_0 = runif(N, 1, 3),
      z_0 = runif(N, 0.2, 0.9),
      v_0 = rnorm(N, 1, 0.5),
      y = rep(beta_0, N) + beta_v_0*v_0 + rnorm(N, 0, sigma_y),
    )


  # this function simulates DDM using easyRT
  r_ddm = function(a,z,t,v){
    tmp = rtdists::rdiffusion(n = 1, a = a, v = v, z = z, t0 = t, st0 = 0.001)
    return(tmp)
  }



  # generate fake response and reaction time for each trial.
  # these are the trial-level true parameters with output
  data2_true = dplyr::tibble(
    id = rep(1:N, each = n_each),
    t = rep(data1_true$t_0, each = n_each),
    a = rep(data1_true$a_0, each = n_each),
    z = rep(data1_true$z_0, each = n_each),
    v = rep(data1_true$v_0, each = n_each) + rnorm(N*n_each, 0, cont_level),
  )

  data2_true =
    dplyr::mutate(
      data2_true,
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
  data2 = dplyr::select(data2_true, id, rt, response)

  return(list(
    data1 = data1,
    data2 = data2,
    data1_true = data1_true,
    data2_true = data2_true
  ))
}


#N = 10 # number of subjects
#n_each = 20 # number of trials for each subject


# significance level a = 0.1
simulate_experiment = function(
    N, # number of subjects
    n_each, # number of trials for each subject
    cont_level, # v is determined by nothing (0), x1 (1) or x1+x2 (2)
    out_file # which .csv file to append the simulate results.
){
  error_flag = 0
  beta_0_true = 0
  beta_v_0_true = 1
  sigma_y = 1


  fake_data = generate_fake_data(
    N,
    beta_0_true,
    beta_v_0_true,
    sigma_y,
    n_each,
    cont_level
  )


  model = list(
    y ~ v_0
  )

  init = "default"
  ddm_link = "default"

  time_taken = system.time({
    tryCatch({
      fit = RegDDM::regddm(
        fake_data[["data1"]],
        fake_data[["data2"]],
        model,
        family = "gaussian",
        init = init,
        ddm_link = ddm_link,
        scale = FALSE,
        warmup = 700,
        iter = 2000
      )
    },error = function(e) {
      cat("Error on condition:", N, n_each, cont_level, conditionMessage(e), "\n")
      error_flag = 1
      return(0)
    })
  })

  if(error_flag == 1){
    return(0)
  }

  res = rstan::summary(fit)$summary
  res = as.data.frame(res)

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

  if(abs(max(res[["Rhat"]])-1)>0.1){
    figure_dir = stringr::str_interp("N${N}_n${n_each}_cont_level${cont_level}_${sample.int(999999, size = 1)}/")
    dir.create(figure_dir)
    for(predictor in rownames(res)){
      if(stringr::str_detect(predictor,"transformed")){
        next
      }
      fig = rstan::traceplot(fit, pars = predictor, inc_warmup = TRUE)
      ggplot2::ggsave(paste0(figure_dir,predictor,".png"),fig)
    }
  }



  summary_stat = stringr::str_c(
    N,
    n_each,
    cont_level,
    time_taken["elapsed"],
    max(res[["Rhat"]]),

    # to analyze the difference between true ddm parameter and RegDDM estimates
    mse_a_0,
    mse_t_0,
    mse_z_0,
    mse_v_0,

    # to analyze the posterior of regression coefficients, the true value and the estimation from two-step approach
    beta_0_true,
    res[1,1],
    res[1,3],
    res[1,4],
    res[1,8],

    beta_v_0_true,
    res[2,1],
    res[2,3],
    res[2,4],
    res[2,8],

    sigma_y,

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
    0.1,
    args[3]
  )
  simulate_experiment(
    as.numeric(args[1]),
    as.numeric(args[2]),
    0.3,
    args[3]
  )
  simulate_experiment(
    as.numeric(args[1]),
    as.numeric(args[2]),
    0.5,
    args[3]
  )
  simulate_experiment(
    as.numeric(args[1]),
    as.numeric(args[2]),
    1,
    args[3]
  )
}

"
N,n_each,cont_level,time_taken,max_r_hat,mse_a_0,mse_t_0,mse_z_0,mse_v_0,beta_0_true,beta_0_mean,beta_0_sd,beta_0_cil,beta_0_cih,beta_v_0_true,beta_v_0_mean,beta_v_0_sd,beta_v_0_cil,beta_v_0_cih,sigma_y
"
