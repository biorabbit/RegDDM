#' function to define if a column is continuous
#' @keywords internal
#' @noRd
is_continuous = function(column){
  if(!is.numeric(column)){
    return(FALSE)
  }
  if(length(na.omit(unique(column))) == 2){
    return(FALSE)
  }
  return(TRUE)
}


#' format the two tables into a list required by rstan
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
    id = match(data2$id,data1$id)
  )

  # find a value that does not exist in all data as placeholder for NA
  placeholder = 114514
  while(any(sapply(data1, function(x) placeholder %in% x))){
    placeholder = sample.int(999999, size = 1)
  }

  # replace NA in continuous variables by a placeholder
  # and NA in catagorical variables by a string "NA"
  for(cov in colnames(data1)){
    if(cov == "id" | cov == "y"){
      next
    }
    # for numeric variables that is not binary, replace the missing value with
    # a placeholder
    if(is_continuous(data1[[cov]])){
      data1[[cov]][is.na(data1[[cov]])] = placeholder
    }
    # for binary or categorical variables, transform into factors with NA as
    # a level
    else{
      data1[[cov]] = factor(data1[[cov]], exclude = NULL)
    }
  }

  # convert to a design matrix
  data1 = as.data.frame(model.matrix(~., data1))

  # replace placeholder back to NA
  data1 = as.data.frame(sapply(data1, function(x) replace(x, x == placeholder, NA)))
  data1 = data1[,2:ncol(data1)]

  # put all data in the format required by rstan
  c_names = c()
  for(cov in colnames(data1)){
    if(cov == "id" | cov == "y"){
      next
    }
    c_names = c(c_names, cov)
    # handle missing values separately
    n_mis = sum(is.na(data1[[cov]]))
    if(n_mis == 0){
      out_list[[cov]] = data1[[cov]]
    }
    else{
      mis_rows = dplyr::filter(data1, is.na(data1[[cov]]))
      obs_rows = dplyr::filter(data1, !is.na(data1[[cov]]))
      out_list[[paste0("N_obs_", cov)]] = nrow(obs_rows)
      out_list[[paste0("N_mis_", cov)]] = nrow(mis_rows)
      out_list[[paste0(cov, "_obs")]] = dplyr::pull(obs_rows, cov)
      out_list[[paste0("ii_obs_", cov)]] = which(!is.na(data1[[cov]]))
      out_list[[paste0("ii_mis_", cov)]] = which(is.na(data1[[cov]]))
    }
  }

  # deal with trial-level data.
  data2 = as.data.frame(model.matrix(~., data2))
  data2 = data2[,2:ncol(data2)]
  x_names = c()

  for(xvar in colnames(data2)){
    if(xvar == "id" | xvar == "rt" | xvar == "response"){
      next
    }
    out_list[[xvar]] = data2[[xvar]]
    x_names = c(x_names, xvar)
  }
  return(list(
    x_names = x_names,
    c_names = c_names,
    data = out_list))
}
