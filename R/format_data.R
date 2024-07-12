#' format the two tables into a list required by rstan
#'
#'
#'
#' @keywords internal
#' @noRd
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
    n_mis = sum(is.na(data1[[cov]]))
    if(n_mis == 0){
      out_list[[cov]] = data1[[cov]]
      next
    }
    mis_rows = dplyr::filter(data1,is.na(pull(data1, cov)))
    obs_rows = dplyr::filter(data1,!is.na(pull(data1, cov)))
    out_list[[paste0("N_obs_", cov)]] = nrow(obs_rows)
    out_list[[paste0("N_mis_", cov)]] = nrow(mis_rows)
    out_list[[paste0(cov, "_obs")]] = dplyr::pull(obs_rows, cov)
    out_list[[paste0("ii_obs_", cov)]] = dplyr::pull(obs_rows, id)
    out_list[[paste0("ii_mis_", cov)]] = dplyr::pull(mis_rows, id)


  }

  for(xvar in colnames(data2)){
    if(xvar == "id" | xvar == "rt" | xvar == "response"){
      next
    }
    out_list[[xvar]] = data2[[xvar]]
  }
  return(out_list)
}
