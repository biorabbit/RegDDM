#' format the two tables into a list required by rstan
#'
#'
#'
#'
#' @export
format_data = function(
    data1,
    data2
){
  out_list = list(
    N = nrow(data1),
    n = nrow(data2),
    y = data1$y,
    response = data2$response,
    rt = data2$rt,
    id = data2$id
  )

  for(cov in colnames(data1)){
    if(cov == "id" | cov == "y"){
      next
    }
    out_list[[cov]] = data1[[cov]]
  }

  for(xvar in colnames(data2)){
    if(xvar == "id" | xvar == "rt" | xvar == "response"){
      next
    }
    out_list[[xvar]] = data2[[xvar]]
  }
  return(out_list)
}
