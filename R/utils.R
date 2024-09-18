#' Somehow stan does not support : in variable names thus we have to replace it with _.
#' @keywords internal
#' @noRd
replace_colon = function(var){
  return(stringr::str_replace_all(var, ":", "_"))
}


#' This function determines whether a vector is scaled or not
#' @keywords internal
#' @noRd
is_scaled = function(x){
  if(
    abs(mean(x))> 0.1 |
    sd(x) > 1.1 |
    sd(x) < 0.9
  ){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}
