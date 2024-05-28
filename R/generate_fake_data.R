#' This function generates a fake dataset using easyRT
#'
#'
#'
#'
#' @export
generate_fake_data <- function(
    N = 10,
    beta_0 = 0.2,
    beta_c1 = -1,
    beta_c2 = 0,
    beta_v_x1 = 1,
    beta_v_x2 = 0,
    sigma_y = 0.1,
    rep_trial = 4
){

  # These are the subject-level true parameters.
  data1_true = tibble::tibble(
    id = 1:N,
    c1 = rnorm(N),
    c2 = rnorm(N),
    t_0 = runif(N, 0.1, 0.4),
    a_0 = rnorm(N, 1.6, 0.4),
    z_0 = runif(N, 0.2, 0.9),
    v_0 = rnorm(N, 1.1, 0.3),
    v_x1 = runif(N, 0.5, 1.5),
    v_x2 = runif(N, -0.8, 0.2),
    y = rep(beta_0, N) + beta_c1*c1 + beta_c2*c2 + beta_v_x1*v_x1 + beta_v_x2*v_x2 + rnorm(N, 0, sigma_y)
  )

  # generate trial-level variables with a 5x5 grid.
  x1 = scale(c(1,2,3,4,5))
  x2 = scale(c(1,2,3,4,5))
  data2_true = tibble::tibble(expand.grid(x1, x2))
  data2_true <- rep(list(data2_true), times = rep_trial*N)
  data2_true <- dplyr::bind_rows(data2_true)
  colnames(data2_true) = c("x1", "x2")

  # this function calculates drift rate given trial-level variables
  # by default, we assume the drift rate to be fully determined
  calc_v = function(v_0, v_x1, v_x2, x1, x2, sigma_v = 0){
    return(v_0 + v_x1*x1 + v_x2*x2 + rnorm(1, 0, sigma_v))
  }

  # this function simulates DDM using easyRT
  r_ddm = function(a,z,t,v){
    params = list(
      drift = v,
      bs = a,
      bias = z,
      ndt = t
    )
    tmp = easyRT::ddm_data(n = 1, drift = v, bs = a, bias = z*a, ndt = t, ndt_var = 0.001)
    return(tmp$data)
  }

  # generate fake response and reaction time for each trial.
  # these are the trial-level true parameters with output
  data2_true =
    dplyr::mutate(
      data2_true,
      id = rep(1:N, each = 25*rep_trial),
      t = rep(data1_true$t_0, each = 25*rep_trial),
      a = rep(data1_true$a_0, each = 25*rep_trial),
      z = rep(data1_true$z_0, each = 25*rep_trial),
      v_0 = rep(data1_true$v_0, each = 25*rep_trial),
      v_x1 = rep(data1_true$v_x1, each = 25*rep_trial),
      v_x2 = rep(data1_true$v_x2, each = 25*rep_trial),
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

  # only partial data is observed and sent to the model
  data1 = dplyr::select(data1_true, id, c1, c2, y)
  data2 = dplyr::select(data2_true, id, x1, x2, rt, response)

  return(list(
    data1 = data1,
    data2 = data2,
    data1_true = data1_true,
    data2_true = data2_true
  ))
}
