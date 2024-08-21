#' @importFrom rstan traceplot
#' @inherit rstan::traceplot
#' @export
traceplot = function(object, ...){
  return(rstan::traceplot(object$stan_fit, ...))
}


#' @importFrom rstan summary
#' @export
summary = function(object, ...){
  return(rstan::summary(object$stan_fit, ...))
}




