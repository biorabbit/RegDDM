#' A small function to format the model
#' @keywords internal
#' @noRd
parse_model = function(model,data1, data2){
full_model = list(
    a = a~1,
    t = t~1,
    z = z~1,
    v = v~1,
    y = y~1
  )
  for(param in model){
    outcome = all.vars(param)[[1]]
    if(!outcome %in% c("a","t","z","v","y")){
      stop("model must only contain a, t, z, v, y as outcome")
    }
    full_model[[outcome]] = param
  }

  y_replacement_list = list()

  for(param in c("a", "t", "z", "v")){
    formula_string = deparse(full_model[[param]])
    formula_string = paste0(formula_string, " ")

    all_term = attr(terms(full_model[[param]]), "variables")
    for(term in as.list(all_term)){
      # skip ddm parameters
      if(term == param){
        next
      }
      # if x is a factor variable with levels A B and C, we need to replace " x "
      # in the formula by " (xB + xC) "
      if(is.factor(data2[[term]])){
        original_string = paste0(" ", term ," ")
        levels_dummy = levels(data2[[term]])[-1]
        replacement_string = paste("(", paste(paste0(term, levels_dummy), collapse = " + "), ")")
        formula_string = stringr::str_replace_all(formula_string, original_string, replacement_string)
      }
    }

    all_term = attr(terms(full_model[[param]]), "term.labels")
    sub_formula = as.formula(substring(deparse(full_model[[param]]), 2))
    all_variables = colnames(model.matrix(sub_formula, data2))
    all_assign = attr(model.matrix(sub_formula, data2), "assign")
    for(i in 1:length(all_term)){
      if(length(all_term) == 0){
        break
      }
      replacement = all_variables[all_assign == i]
      replacement = paste0(param, "_", replacement)
      replacement = paste("(", paste(replacement, collapse = " + "), ")")
      replacement = replace_colon(replacement)
      y_replacement_list[[paste0(" ", param, "_", replace_colon(all_term[i]), " ")]] = replacement

    }

    full_model[[param]] = as.formula(formula_string)


    # formula = as.formula(substring(deparse(full_model[[term]])))
    # terms = colnames(model.matrix(formula, dat = data2))

  }

  # deal with y
  # append covariates with factors to the y_replacement_list
  all_cov = colnames(data1)
  for(cov in all_cov){
    if(is_continuous(data1[[cov]])){
      next
    }
    replacement = levels(factor(data1[[cov]], exclude = NULL))[-1]
    replacement = paste0(cov, replacement)
    replacement = paste("(", paste(replacement, collapse = " + "), ")")
    y_replacement_list[[paste0(" ", cov, " ")]] = replacement
  }

  formula_string = deparse(full_model[["y"]])
  formula_string = paste0(formula_string, " ")
  for(variable in names(y_replacement_list)){
    formula_string = stringr::str_replace_all(
      formula_string,
      variable,
      y_replacement_list[[variable]]
    )
  }

  full_model[["y"]] = as.formula(formula_string)

  return(full_model)
}



