#' This function checks for (potential) problems in the data
#' @keywords internal
#' @noRd
check_data = function(
    data1,
    data2
){
  N = nrow(data1)
  n = nrow(data2)

  if(N == 1){
    stop("please provide at least 2 subjects for regression analysis.\n")
  }
  if(N < 5){
    warning("less than 5 subjects in the data. May experience difficulty in convergence.\n")
  }


  # check for errors in subject-level data
  if(! "y" %in% colnames(data1)){
    stop("Outcome 'y' must be a variable of data1\n")
  }
  if(! "id" %in% colnames(data1)){
    stop("Must identify subjects using variable 'id' in data1\n")
  }
  if(length(data1$id) != length(unique(data1$id))){
    stop("'id' cannot contain duplicate values in 'data1'\n")
  }

  # removed since we support other types of id now
  # data1 = dplyr::arrange(data1, id)
  # for(i in 1:N){
  #   if(data1$id[i] != i){
  #     stop("id must be consecutive positive integers such as 1, 2, 3...\n")
  #   }
  # }

  # check for missing values in subject-level data
  for(cov in colnames(data1)){
    if(cov %in% c("id", "y")){
      if(sum(is.na(dplyr::pull(data1,cov))) > 0){
        stop(stringr::str_interp("'${cov}' in 'data1' cannot contain missing values\n"))
      }
    }
    else{
      if(sum(!is.na(dplyr::pull(data1,cov))) < 2){
        stop(stringr::str_interp("'${cov}' in 'data1' must have at least two observations\n"))
      }
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
    stop("variable 'rt' not found in data2. Encode the response time of subjects in seconds\n")
  }
  if(min(data2$rt) < 0.1){
    warning("Some trials has response time(rt) less than 0.1s. Did your subjects make proper decision?\n")
  }
  if(min(data2$rt) > 10){
    warning("Some trials has response time(rt) greater than 10s. Please note that the unit is in seconds.\n")
  }

  for(id in unique(data2$id)){
    if(!id %in% data1$id){
      stop(paste0("subject ", id, " in data2 has no records in data1. Check your data!\n"))
    }
  }

  for(id in data1$id){
    if(!id %in% data2$id){
      stop(paste0("subject ", id, " in data1 has no records in data2. Check your data!\n"))
    }
  }

  for(i in data1$id){
    if(nrow(filter(data2, id == i)) < 10){
      warning(paste0("You have subject with less than 10 trials, which may lead poor model performance\n"))
      break
    }
  }

  # check for missing value is data2
  for(variable in colnames(data2)){
    if(sum(is.na(dplyr::pull(data2, variable))) > 0){
      stop(stringr::str_interp("'${variable}' in 'data2' cannot contain missing values\n"))
    }
  }

  # check for missing value in data2 and also whether the variables are scaled
  # this is moved when the model is generated (generate_model.R)
  # unscaled_var_list = c()
  # for(variable in colnames(data2)){
  #   if(sum(is.na(dplyr::pull(data2, variable))) > 0){
  #     stop(stringr::str_interp("'${variable}' in 'data2' cannot contain missing values\n"))
  #   }
  #   if(variable %in% c("id", "response", "rt")){
  #     next()
  #   }
  #   if(!is_scaled(dplyr::pull(data2, variable))){
  #     unscaled_var_list = c(unscaled_var_list, variable)
  #   }
  # }
  # if(length(unscaled_var_list) != 0){
  #   warning(paste0("trial-level variable: ", unscaled_var_list, " are not scaled. Default priors and constraints unavailable.\n"))
  # }
}


#' This function checks for error in the model
#' @keywords internal
#' @noRd
check_model = function(
    xvar,
    cvar,
    model
){
  # find all valid terms for the glm part
  valid_parameters = c(cvar,"v_0","t_0","z_0","a_0")

  # the influence of trial-level variable on ddm parameter is also valid
  for(outcome in c("a", "t", "v", "z")){
    formula = model[[outcome]]
    variable_labels = rownames(attr(terms(formula), "factors"))
    # check if trial-level variables are found in data2
    for(variable in variable_labels){
      if(variable == outcome){
        next
      }
      if(!variable %in% xvar){
        stop(stringr::str_interp("${variable} in model:${outcome} is not found in data2"))
      }
    }
    term_labels = attr(terms(formula), "term.labels")
    for(term in term_labels){
      valid_parameters = c(valid_parameters, replace_colon(paste0(outcome,"_",term)))
    }
  }

  # check if there is duplicate parameter name
  # this could happen when there are three variables 'x1', 'x2' and 'x1_x2'
  # and a model with a ~ x1 * x2 + x1_x2 structure is fit...
  # RegDDM will generate a_x1_x2 for x1, x2 interaction and also a_x1_x2 for x1_x2
  # currently no good solution since stan does not accept x1:x2 as varaible name
  duplicate_terms = dplyr::tibble(
    term = valid_parameters
  )
  duplicate_terms = dplyr::summarise(
    dplyr::group_by(
      duplicate_terms, by = term
    ),
    n = dplyr::n()
  )

  for(i in 1:nrow(duplicate_terms)){
    if(duplicate_terms[i,2]>1){
      stop(stringr::str_interp(
        "RegDDM generated identical name ${duplicate_terms[i,1]} for two parameters\n
       For example, having 'v_0' as a covariate and RegDDM generating another
      'v_0' for baseline drift rate will cause this problem. \n"
      ))
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
      stop(stringr::str_interp(
        "'${term}' is not a valid term in formula for y. See '?regddm' for reference\n
        Valid terms are ${valid_parameters} and their interactions"
      ))
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
