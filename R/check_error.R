#' This function checks for error in the data
#' @keywords internal
#' @noRd
check_data = function(
    data1,
    data2
){
  N = nrow(data1)
  n = nrow(data2)

  # check for errors in subject-level data
  if(! "y" %in% colnames(data1)){
    stop("Outcome y must be a variable of data1\n")
  }
  if(! "id" %in% colnames(data1)){
    stop("Must identify subjects using variable 'id' in data1\n")
  }
  if(length(data1$id) != length(unique(data1$id))){
    stop("id cannot contain duplicate values in data1\n")
  }
  data1 = dplyr::arrange(data1, id)

  for(i in 1:N){
    if(data1$id[i] != i){
      stop("id must be consecutive positive integers such as 1, 2, 3...\n")
    }
  }

  # check for errors in trial-level data
  if(! "id" %in% colnames(data2)){
    stop("Must identify subjects using variable 'id' in data2\n")
  }
  if(! "response" %in% colnames(data2)){
    stop("variable 'response' not found in data2. Encode the response of the subject using Yes = 1, No = 0\n")
  }
  if(! "rt" %in% colnames(data2)){
    stop("variable 'rt' not found in data2. Encode the response time of the subject in seconds")
  }

  for(id in unique(data2$id)){
    if(!id %in% data1$id){
      stop(paste0("subject ", i, " in data2 has no records in data1. Check your data!\n"))
    }
  }

  for(i in 1:N){
    if(!i %in% data2$id){
      stop(paste0("subject ", i, " in data1 has no records in data2. Check your data!\n"))
    }
  }

  for(i in 1:N){
    if(nrow(filter(data2,id==i))<10){
      warning(paste0("You have subject with less than 10 trials, which may lead poor estimation for their DDM parameters\n"))
      break
    }
  }

  unscaled_var_list = c()
  for(variable in colnames(data2)){
    if(variable %in% c("id", "response", "rt")){
      next()
    }
    if(
      abs(mean(dplyr::pull(data2, variable), na.rm = TRUE))> 1 |
      sd(dplyr::pull(data2, variable), na.rm = TRUE) > 2 |
      sd(dplyr::pull(data2, variable), na.rm = TRUE) < 0.5
    ){
      unscaled_var_list = c(unscaled_var_list, variable)
    }
  }
  if(length(unscaled_var_list) !=0){
    warning(paste0("trial-level variable: ", unscaled_var_list, " are not scaled. Default priors and constraints unavailable.\n"))
  }
}


#' This function checks for error in the model
#' @keywords internal
#' @noRd
check_model = function(
    xvar,
    cvar,
    model
){
  valid_parameters = c(cvar,"v_0","t_0","z_0","a_0")
  for(outcome in c("a", "t", "v", "z")){
    formula = model[[outcome]]
    variable_labels = rownames(attr(terms(formula), "factors"))
    for(variable in variable_labels){
      if(variable == outcome){
        next
      }
      if(!variable %in% xvar){
        stop(stringr::str_interp("${variable} in model:${outcome} is not found in the data"))
      }
    }
    term_labels = attr(terms(formula), "term.labels")
    for(term in term_labels){
      valid_parameters = c(valid_parameters, paste0(outcome,"_",term))
    }
  }

  #if(!identical(sort(names(model)), c("a", "t", "v", "y", "z") )){
  #  stop("Model must contain and only contain a, z, t, v, y")
  #}
  #for(ddm_param in c("a", "t", "z", "v")){
  #  for(param in model[[ddm_param]]){
  #    if(!param %in% xvar){
  #      stop(stringr::str_interp("${param} in model:${ddm_param} is not found in the data"))
  #    }
  #  }
  #}

  #if(y_formula == NA){
  #  warning(stringr::str_interp("You didn't define regression model for y\n"))
  #}
  #else{
  #  y_terms = attr(terms(y_formula), "term.labels")
  #  for(term in y_terms){
  #    if(!term %in% valid_parameters){
  #      stop(stringr::str_interp("${term} is not a valid term in formula for y. See '?regddm' for reference"))
  #    }
  #  }
  #}
  y_terms = attr(terms(model[["y"]]), "term.labels")
  for(term in y_terms){
    if(!term %in% valid_parameters){
      stop(stringr::str_interp("${term} is not a valid term in formula for y. See '?regddm' for reference\n"))
    }
  }
#  for(param in model[["y"]]){
#    if(param %in% cvar){
#      next()
#    }
#
#    if(!stringr::str_detect(param, "^[a,z,t,v]_.+")){
#      stop(stringr::str_interp("${param} is ill formatted. must be one of covariates or like 'v_reward'"))
#    }
#
#    if(!stringr::str_sub(param, 3) %in% c(xvar, "0")){
#      stop(stringr::str_interp("${param} does not contain a trial-level variable"))
#    }
#  }

#  for(param in cvar){
#    if(! param %in% model[["y"]]){
#      warning(stringr::str_interp("covariate ${param} is not used in the model"))
#    }
#  }
#  for(param in xvar){
#    count = 0
#    for(ddm_param in c("a", "t", "z", "v")){
#      if(param %in% model[[ddm_param]]){
#        count = count + 1
#      }
#    }
#    if(count == 0){
#      warning(stringr::str_interp("$trial-level variable {param} is not used in the model"))
#    }
#  }
}
