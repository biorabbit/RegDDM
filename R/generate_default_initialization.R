# #' given the inverse of link function at a specified value
# #' @keywords internal
# #' @noRd
# inverse_ddm_link = function(ddm_link, value){
#   value = as.numeric(value)
#   if(ddm_link == ""){
#     return(value)
#   }
#   if(ddm_link == "exp"){
#     return(log(value))
#   }
#   if(ddm_link == "inv_logit"){
#     return(log(value/(1-value)))
#   }
#   stop("unsupported ddm link")
# }

#' This function generates the default initialization list for RStan
#' Because DDM parameters have certain constraints, default initialization of
#' Rstan may result in Initilization failure.
#' RegDDM will adopt a smarter way. The user can also provide other inits.
#' @keywords internal
#' @noRd
# generate_default_initialization = function(data1, data2, model, ddm_link){
generate_default_initialization = function(data1, data2, model){
  init_list = list()

  # For GLM regression coefficients, start from no correlation.
  # The intercept is the mean of output and all slopes are 0.
  # Standard deviation (SD) of error term is the SD of output.
  init_list[["beta_0"]] = mean(data1[['y']])
  init_list[["sigma_y"]] = sd(data1[['y']])
  for(predictor in attr(terms(model$y), "term.labels")){
    predictor = replace_colon(predictor)
    init_list[[paste0("beta_", predictor)]] = 0
  }

  # Group mean and SD of baseline DDM parameters starts from some reasonable values.
  init_list[["u_a_0"]] = 1
  init_list[["sig_a_0"]] = 0.5
  init_list[["u_t_0"]] = 0.2
  init_list[["sig_t_0"]] = 0.1
  init_list[["u_z_0"]] = 0.5
  init_list[["sig_z_0"]] = 0.2
  init_list[["u_v_0"]] = 0
  init_list[["sig_v_0"]] = 0.5

  # Subject-level baseline DDM parameters also starts from reasonable values.
  N = nrow(data1)
  init_list[["a_0"]] = rep(1, N)
  init_list[["z_0"]] = rep(0.5, N)
  init_list[["v_0"]] = rep(0, N)

  # Note that baseline non-decision time needs special treatment.
  # They start from half of the minimal reaction time of each subject.
  init_list[["t_0"]] = rep(NA, N)
  min_reaction_time = dplyr::summarise(dplyr::group_by(data2, id), rt = min(rt))
  for(i in 1:N){
    # init_list[["t_0"]][i] = inverse_ddm_link(ddm_link[["t"]], min_reaction_time[i,2]/2)
    init_list[["t_0"]][i] = as.numeric(min_reaction_time[i,2]/2)
  }

  # Initialize other subject-level ddm parameters to 0.
  # Group mean and SD are initialized to 1 and 0 respectively.
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
