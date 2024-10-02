.libPaths(c("/home/jinzekai/R_libs/"))
library(dplyr)
library(tidyr)
library(RegDDM)

r_ddm = function(a,z,t,v){
  # somehow, if st0 = 0, there will be some weird outcomes
  tmp = rtdists::rdiffusion(n = 1, a = a, v = v, z = z, t0 = t, st0 = 0.001)
  return(tmp)
}

mse = function(x1, x2){
  return(mean((x1-x2)^2))
}

run = function(N,n,sigmav,out_file){
  # predictor
  x = rnorm(N, 0, 0.5)

  # subject-level ddm parameters
  t_0 = runif(N, 0.2, 0.5)
  a_0 = runif(N, 1, 3)
  z_0 = runif(N, 0.4, 0.6)
  v_0 = x + rnorm(N, 1.5, sigmav)

  # prepare data for RegDDM
  data1 = tibble(
    id = 1:N,
    x = x
  )

  # sometimes this function goes wrong and produce extremely small reaction times
  # then simulate another experiment
  min_rt = 0
  while(min_rt < 0.15){
    data2 = tibble(
      id = rep(1:N, each  = n),
      t = rep(t_0, each = n),
      a = rep(a_0, each = n),
      z = rep(z_0, each = n),
      v = rep(v_0, each = n),
      ddm = purrr::pmap(list(a,z,t,v), r_ddm)
    ) %>%
      unnest(cols = c(ddm)) %>%
      mutate(response = ifelse(response == "upper", 1, 0)) %>%
      select(id, rt, response)
    min_rt = min(data2$rt)
  }

  # fit the two RegDDM models
  model1 = list(
    v_0 ~ x
  )

  fit1 = regddm(data1,data2,model1,stan_filename = "")

  model2 = list()

  fit2 = regddm(data1,data2,model2,,stan_filename = "")


  summary_stat = stringr::str_c(
    N,
    n[1],
    sigmav,
    fit1$max_r_hat,
    fit2$max_r_hat,
    mse(v_0,fit1$subject_ddm_param$mean$v_0),
    mse(v_0,fit2$subject_ddm_param$mean$v_0),
    (lm(v_0 ~ x) %>% broom::tidy(conf.int = TRUE))$estimate[2],
    (lm(v_0 ~ x) %>% broom::tidy(conf.int = TRUE))$std.error[2],
    (lm(v_0 ~ x) %>% broom::tidy(conf.int = TRUE))$conf.low[2],
    (lm(v_0 ~ x) %>% broom::tidy(conf.int = TRUE))$conf.high[2],

    (lm(fit1$subject_ddm_param$mean$v_0 ~ x) %>% broom::tidy(conf.int = TRUE))$estimate[2],
    (lm(fit1$subject_ddm_param$mean$v_0 ~ x) %>% broom::tidy(conf.int = TRUE))$std.error[2],
    (lm(fit1$subject_ddm_param$mean$v_0 ~ x) %>% broom::tidy(conf.int = TRUE))$conf.low[2],
    (lm(fit1$subject_ddm_param$mean$v_0 ~ x) %>% broom::tidy(conf.int = TRUE))$conf.high[2],

    (lm(fit2$subject_ddm_param$mean$v_0 ~ x) %>% broom::tidy(conf.int = TRUE))$estimate[2],
    (lm(fit2$subject_ddm_param$mean$v_0 ~ x) %>% broom::tidy(conf.int = TRUE))$std.error[2],
    (lm(fit2$subject_ddm_param$mean$v_0 ~ x) %>% broom::tidy(conf.int = TRUE))$conf.low[2],
    (lm(fit2$subject_ddm_param$mean$v_0 ~ x) %>% broom::tidy(conf.int = TRUE))$conf.high[2],

    fit1$glm_coefficiets$mean[2],
    fit1$glm_coefficiets$sd[2],
    fit1$glm_coefficiets$`2.5%`[2],
    fit1$glm_coefficiets$`97.5%`[2],

    sep = ","
  )

  write(summary_stat,out_file,append = TRUE)

}

args <- commandArgs(trailingOnly = TRUE)

for(i in 1:as.numeric(args[5])){
  run(
    as.numeric(args[1]),
    as.numeric(args[2]),
    as.numeric(args[3]),
    args[4]
  )
}


txt = "
nohup Rscript sim4_1.R 30 25 0.5 sim4.csv 100 &
nohup Rscript sim4_1.R 30 50 0.5 sim4.csv 100 &
nohup Rscript sim4_1.R 30 100 0.5 sim4.csv 50 &
nohup Rscript sim4_1.R 30 100 0.5 sim4.csv 50 &
nohup Rscript sim4_1.R 30 200 0.5 sim4.csv 25 &
nohup Rscript sim4_1.R 30 200 0.5 sim4.csv 25 &
nohup Rscript sim4_1.R 30 200 0.5 sim4.csv 25 &
nohup Rscript sim4_1.R 30 200 0.5 sim4.csv 25 &

N,n,sigmav,rhat1,rhat2,mse1,mse2,lm1_est,lm1_mean,lm1_cil,lm1_cih,lm2_est,lm2_mean,lm2_cil,lm2_cih,lm3_est,lm3_mean,lm3_cil,lm3_cih,regddm_est,regddm_sd,regddm_cil,regddm_cih
"
