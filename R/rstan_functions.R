#' @importFrom rstan traceplot
#' @inherit rstan::traceplot
#' @export
traceplot = function(object, ...){
  return(rstan::traceplot(object$stan_fit, ...))
}




