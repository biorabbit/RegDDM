#' @importFrom rstan traceplot
#' @export
traceplot = function(...){
  rstan::traceplot(...)
}
#' @importFrom rstan check_hmc_diagnostics
#' @export
check_hmc_diagnostics = rstan::check_hmc_diagnostics

#' @importFrom rstan summary
#' @export
summary = rstan::summary



