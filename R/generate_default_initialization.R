#' @keywords internal
#' @noRd
inverse_ddm_link = function(ddm_link, value){
  value = as.numeric(value)
  if(ddm_link == ""){
    return(value)
  }
  if(ddm_link == "exp"){
    return(log(value))
  }
  if(ddm_link == "inv_logit"){
    return(log(value/(1-value)))
  }
  stop("unsupported ddm link")
}

#' @export
generate_default_initialization = function(data1, data2, model, ddm_link){
  init_list = list()
  # for regression coefficients
  init_list[["beta_0"]] = mean(data1[['y']])
  init_list[["sigma_y"]] = sd(data1[['y']])
  for(predictor in attr(terms(model$y), "term.labels")){
    predictor = replace_colon(predictor)
    init_list[[paste0("beta_", predictor)]] = 0
  }

  N = nrow(data1)
  init_list[["a_0"]] = rep(inverse_ddm_link(ddm_link[["a"]], 1), N)
  init_list[["t_0"]] = rep(NA, N)
  init_list[["z_0"]] = rep(inverse_ddm_link(ddm_link[["z"]], 0.5), N)
  init_list[["v_0"]] = rep(inverse_ddm_link(ddm_link[["v"]], 0), N)

  init_list[["u_a_0"]] = 1
  init_list[["sig_a_0"]] = 0.5
  init_list[["u_t_0"]] = 0.2
  init_list[["sig_t_0"]] = 0.1
  init_list[["u_z_0"]] = 0.5
  init_list[["sig_z_0"]] = 0.2
  init_list[["u_v_0"]] = 0
  init_list[["sig_v_0"]] = 0.5


  min_reaction_time = dplyr::summarise(dplyr::group_by(data2, id), rt = min(rt))

  for(i in 1:N){
    init_list[["t_0"]][i] = inverse_ddm_link(ddm_link[["t"]], min_reaction_time[i,2]/2)
  }

  # initialize other ddm parameters to 0
  for(param in c("a", "t", "z", "v")){
    tmp_term = attr(terms(model[[param]]), "term.labels")
    for(predictor in tmp_term){
      predictor = replace_colon(predictor)
      init_list[[stringr::str_interp("u_${param}_${predictor}")]] = 0
      init_list[[stringr::str_interp("sig_${param}_${predictor}")]] = 1
      init_list[[stringr::str_interp("${param}_${predictor}")]] = rep(0, N)
    }
  }
  return(init_list)

}
