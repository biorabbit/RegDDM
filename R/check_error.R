#' This function checks for error in the data
#'
#'
#'
#'
#' @export
check_data = function(
    data1,
    data2
){
  N = nrow(data1)
  n = nrow(data2)

  # check for errors
  if(! "y" %in% colnames(data1)){
    stop("Outcome y must be a variable of data1")
  }
  if(! "id" %in% colnames(data1)){
    stop("Must identify subjects using variable 'id' in data1")
  }
  if(length(data1$id) != length(unique(data1$id))){
    stop("id cannot contain duplicate numbers in data1")
  }
  data1 = dplyr::arrange(data1, id)

  for(i in 1:N){
    if(data1$id[i] != i){
      stop("id must be consecutive positive integers such as 1, 2, 3...")
    }
  }

  if(! "id" %in% colnames(data2)){
    stop("Must identify subjects using variable 'id' in data2")
  }

  for(id in unique(data2$id)){
    if(!id %in% data1$id){
      stop(paste0("subject ", i, " in data2 has no records in data1. Check your data!"))
    }
  }

  for(i in 1:N){
    if(!i %in% data2$id){
      stop(paste0("subject ", i, " in data1 has no records in data2. Check your data!"))
    }
  }

  for(i in 1:N){
    if(nrow(filter(data2,id==i))<10){
      warning(paste0("You have subject with less than 10 trials, which may lead to large posterior variance for DDM parameters"))
    }
  }

  for(variable in colnames(data1)){
    if(variable == "id"){
      next()
    }
    if(
      abs(mean(dplyr::pull(data1, variable)))>1 |
      sd(dplyr::pull(data1, variable)) > 2 |
      sd(dplyr::pull(data1, variable)) < 0.5
    ){
      warning(paste0("variables are not scaled, which may influence model convergence."))
    }
  }
}


#' This function checks for error in the model
#'
#'
#'
#'
#' @export
check_model = function(
    xvar,
    cvar,
    model
){
  if(!identical(sort(names(model)), c("a", "t", "v", "y", "z") )){
    stop("Model must contain and only contain a, z, t, v, y")
  }
  for(ddm_param in c("a", "t", "z", "v")){
    for(param in model[[ddm_param]]){
      if(!param %in% xvar){
        stop(stringr::str_interp("${param} in model:${ddm_param} is not found in the data"))
      }
    }
  }

  for(param in model[["y"]]){
    if(param %in% cvar){
      next()
    }

    if(!stringr::str_detect(param, "^[a,z,t,v]_.+")){
      stop(stringr::str_interp("${param} is ill formatted. must be one of covariates or like 'v_reward'"))
    }

    if(!stringr::str_sub(param, 3) %in% c(xvar, "0")){
      stop(stringr::str_interp("${param} does not contain a trial-level variable"))
    }
  }

  for(param in cvar){
    if(! param %in% model[["y"]]){
      warning(stringr::str_interp("covariate ${param} is not used in the model"))
    }
  }
  for(param in xvar){
    count = 0
    for(ddm_param in c("a", "t", "z", "v")){
      if(param %in% model[[ddm_param]]){
        count = count + 1
      }
    }
    if(count == 0){
      warning(stringr::str_interp("$trial-level variable {param} is not used in the model"))
    }
  }
}
