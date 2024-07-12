#' @keywords internal
#' @noRd
parse_model = function(model){
  full_model = list(
    a = a~1,
    t = t~1,
    z = z~1,
    v = v~1,
    y = y~1
  )
  for(term in model){
    outcome = all.vars(term)[[1]]
    if(!outcome %in% c("a","t","z","v","y")){
      stop("model must only contain a, t, z, v, y as outcome")
    }
    full_model[[outcome]] = term
  }
  return(full_model)
}






