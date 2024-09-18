#' This function summarizes the stan fit according to the regddm model structure
#' Making the results more tidy and easier to use.
#' @keywords internal
#' @noRd
#'
#'
#'
summary_results = function(fit,model,data1){
  res = as.data.frame(rstan::summary(fit)$summary)
  res$variable = rownames(res)
  res = dplyr::tibble(res)
  res = dplyr::select(res, variable, everything())

  extract_subject_ddm = function(statistics){
    subject_ddm =
      dplyr::filter(res, stringr::str_detect(variable, "^[atzv]_.+\\[\\d+\\]$"))

    subject_ddm =
      dplyr::mutate(
        subject_ddm,
        variable = stringr::str_remove(variable, "\\[\\d+\\]"),
        id = rep(data1$id,times = nrow(subject_ddm)/nrow(data1))
      )
    subject_ddm =
      dplyr::select(subject_ddm, id, !!rlang::sym(statistics), variable)
    subject_ddm =
      tidyr::pivot_wider(subject_ddm, names_from = variable, values_from = !!rlang::sym(statistics))

    return(subject_ddm)
  }

  glm_coefficiets =
    dplyr::filter(res, stringr::str_detect(variable, "^beta_") | stringr::str_detect(variable, "^sigma$"))

  subject_ddm_param = list(
    mean = extract_subject_ddm("mean"),
    sd = extract_subject_ddm("sd"),
    CrI_l = extract_subject_ddm("2.5%"),
    CrI_h = extract_subject_ddm("97.5%"),
    Rhat = extract_subject_ddm("Rhat")
  )

  group_param = dplyr::filter(
    res,
    stringr::str_detect(variable,"^(mu|sigma)_.+$")
  )

  missing_value = dplyr::filter(
    res,
    stringr::str_detect(variable,"^.+_mis.*$")
  )

  max_r_hat = max(res$Rhat)

  return(
    list(
      glm_coefficiets = glm_coefficiets, # glm regression parameters
      subject_ddm_param = subject_ddm_param, # ddm parameters of each subject
      group_param = group_param, # group mean and SD of DDM parameters
      missing_value = missing_value, # estimated missing covariates
      max_r_hat = max_r_hat,
      model = model,
      stan_fit = fit # additional information can be found from the original stan model.
    )
  )
}



