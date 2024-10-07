#' Generate simulated binary decision data using DDM
#'
#' @description
#' This function generates a fake dataset under different configurations
#' It can be used to test the performance of RegDDM under different conditions.
#' It is also used in testing the functionality of the package.
#'
#' @param N Number of subjects to simulate
#' @param n_each Number of trials per subject
#' @param n_xvar Number of trial-level variables influencing drift rate
#' @param beta_0 Intercept
#' @param beta_c1 Slope of c1
#' @param beta_c2 Slope of c2
#' @param beta_v_0 Slope of v_0
#' @param beta_v_x1 Slope of v_x1
#' @param beta_v_x2 Slope of v_x2
#' @param sigma_y Standard deviation of error term of y
#' @param sigma_v Contaminant level for drift rate v
#' @param y_family Family of distribution of y. Can be either "gaussian", "bernoulli" or "poisson"
#'
#' @export
#'
#' @examples
#' ## Not run:
#' fake_data = generate_fake_data()
#' ## End(Not run)
generate_fake_data <- function(
    N = 30,
    n_each = 100,
    n_xvar = 2, # number of trial-level variables included
    beta_0 = 0,
    beta_c1 = 0,
    beta_c2 = 0,
    beta_v_0 = 0,
    beta_v_x1 = 0,
    beta_v_x2 = 0,
    sigma_y = 1,
    sigma_v = 0,
    y_family = "gaussian"
){
  x1 = runif(n_each*N, -sqrt(3), sqrt(3))
  x2 = runif(n_each*N, -sqrt(3), sqrt(3))

  # generating the subject-level data
  data1_true =
    dplyr::tibble(
      id = 1:N,
      t_0 = runif(N, 0.2, 0.5),
      a_0 = runif(N, 1, 3),
      z_0 = runif(N, 0.4, 0.6),
      v_0 = rnorm(N, 1.5, 0.5),
      c1 = runif(N,0,2),
      c2 = runif(N,0,2),
      v_x1 = rnorm(N)*ifelse(n_xvar >= 1, 1, 0), # first trial-level variable
      v_x2 = rnorm(N)*ifelse(n_xvar == 2, 1, 0) # second trial-level variable
    )

  # used to eliminate the notes in R CMD check
  random_number = 0
  p = 0
  lambda = 0

  # generate y based on the family of distribution
  if(y_family == "gaussian"){
    data1_true = dplyr::mutate(
      data1_true,
      y = rep(beta_0, N) + beta_c1*.data$c1 + beta_c2*.data$c2 + beta_v_0*.data$v_0 + beta_v_x1*.data$v_x1 + beta_v_x2*.data$v_x2 + rnorm(N, 0, sigma_y)
    )
  }
  else if(y_family == "bernoulli"){
    data1_true = dplyr::mutate(
      data1_true,
      p = rep(beta_0, N) + beta_c1*.data$c1 + beta_c2*.data$c2 + beta_v_0*.data$v_0 + beta_v_x1*.data$v_x1 + beta_v_x2*.data$v_x2,
      p = 1/(1+exp(-p)),
      random_number = runif(N),
      y = ifelse(random_number < p, 1, 0)
    )
  }
  else if(y_family == "poisson"){
    data1_true = dplyr::mutate(
      data1_true,
      lambda = rep(beta_0, N) + beta_c1*.data$c1 + beta_c2*.data$c2 + beta_v_0*.data$v_0 + beta_v_x1*.data$v_x1 + beta_v_x2*.data$v_x2,
      lambda = exp(lambda),
      y = purrr::map_dbl(.x = lambda, ~rpois(1, .x))
    )
  }

  # generating the trial-level data
  data2_true = dplyr::tibble(x1 = x1, x2 = x2)

  # this function calculates drift rate given trial-level variables
  # by default, we assume the drift rate to be fully determined (sigma_v = 0)
  calc_v = function(v_0, v_x1, v_x2, x1, x2){
    return(v_0 + v_x1*x1 + v_x2*x2 + rnorm(1, 0, sigma_v))
  }

  # this function simulates DDM using easyRT
  r_ddm = function(a,z,t,v){
    # somehow, if st0 = 0, there will be some weird outcomes
    tmp = rtdists::rdiffusion(n = 1, a = a, v = v, z = z, t0 = t, st0 = 0.001)
    return(tmp)
  }

  # generate fake response and reaction time for each trial.
  # these are the trial-level true parameters with output
  # in our simulation, only drift rate is influenced by trial-level variables
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
      v = purrr::pmap_dbl(list(.data$v_0, .data$v_x1, .data$v_x2, .data$x1, .data$x2), calc_v),
      ddm = purrr::pmap(list(.data$a, .data$z, .data$t, .data$v), r_ddm)
    )

  data2_true =
    tidyr::unnest(data2_true, "ddm")

  data2_true =
    dplyr::mutate(
      data2_true,
      response = ifelse(.data$response == "upper", 1, 0)
    )

  data1 = dplyr::select(data1_true, "id", "y", "c1", "c2")
  data2 = dplyr::select(data2_true, "id", "response", "rt", "x1", "x2")

  return(list(
    data1 = data1,
    data2 = data2,
    data1_true = data1_true,
    data2_true = data2_true
  ))
}
