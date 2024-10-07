# Test that improper data structure will trigger an error or warning
# Such as missing required columns, mismatch of subjects
test_that("data check works", {
  data("regddm_tutorial")

  # test that duplicate id will trigger an error
  data1 = regddm_tutorial$data1
  data2 = regddm_tutorial$data2
  data1$id[1:2] = c(1,1)
  model = list()
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # test that missing id will trigger an error
  data1 = dplyr::select(regddm_tutorial$data1, -"id")
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  data1 = regddm_tutorial$data1
  data2 = dplyr::select(regddm_tutorial$data2, -"id")
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  data2 = dplyr::select(regddm_tutorial$data2, -"rt")
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  data2 = dplyr::select(regddm_tutorial$data2, -"response")
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # test that improper coding of response will trigger an error
  data2 = dplyr::mutate(regddm_tutorial$data2, response = ifelse(response == 1, "upper", "lower"))
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # test that abnormal value of response time rt will trigger an warning
  data2 = dplyr::mutate(regddm_tutorial$data2, rt = rt* 100)
  expect_warning(regddm(data1, data2, model, fit_model = FALSE))

  # test that missing values in data2 will trigger an error
  data2 = regddm_tutorial$data2
  data2[["x1"]][1] = NA
  model = list(v ~ x1)
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # test that data1 and data2 must have exactly the same subjects
  data1 = regddm_tutorial$data1
  data2 = regddm_tutorial$data2
  data1[["id"]][1] = 1001
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

})

# Test that improper specification of models will trigger an error
test_that("model check works", {

  data("regddm_tutorial")
  data1 = regddm_tutorial$data1
  data2 = regddm_tutorial$data2

  # check that including a covariate that does not exist will trigger an error
  model = list(
    y ~ c3
  )
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # check that including a non-existing trial level variable will trigger an error
  model = list(
    v ~ x3
  )
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # check that improper naming of variables will trigger an error
  data1 = dplyr::rename(regddm_tutorial$data1, v_0 = c1)
  model = list(y ~ v_0)
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

  # check that multiple regression models will trigger an error
  data1 = regddm_tutorial$data1
  model = list(y ~ v_0, c1 ~ v_0)
  expect_error(regddm(data1, data2, model, fit_model = FALSE))

})

# Test that the simulated data generation function works
# this function is mostly used as a testing tool for RegDDM
test_that("fake data generation works", {
  expect_no_error(
    generate_fake_data()
  )
  expect_no_error(
    generate_fake_data(y_family = "bernoulli", n = 50, n_each = 50)
  )
  expect_no_error(
    generate_fake_data(y_family = "poisson", n_xvar = 0, beta_v_0 = 1, sigma_v = 0.3)
  )
})


# the following codes needs to fit stan models, which takes 4 cores and a long time
# some tests will not be performed on CRAN but locally.
# warnings are suppressed because it doesn't influence RegDDM functionality
test_that("example code works", {suppressWarnings({
  skip_on_cran()
  data(regddm_tutorial)
  model = list(v ~ x1, y ~ v_0 + v_x1 + c1)
  expect_no_error(
    regddm(
      regddm_tutorial$data1,
      regddm_tutorial$data2,
      model,
      fit_model = FALSE
    )
  )
})})

test_that("model with interaction works", {suppressWarnings({
  skip_on_cran()

  # interaction in trial-level variable
  fake_data = generate_fake_data(N = 30, n_xvar = 2, n_each = 100)
  model = list(
    v ~ x1 * x2,
    y ~ v_0 + v_x1_x2
  )
  expect_no_error(
    regddm(fake_data$data1, fake_data$data2, model)
  )

  # interaction in subject-level variable
  fake_data = generate_fake_data(N = 30, n_xvar = 1, n_each = 100)
  model = list(
    v ~ x1,
    y ~ v_0 + v_x1 * c1
  )
  expect_no_error(
    regddm(fake_data$data1, fake_data$data2, model)
  )
})})


test_that("model works for factor variables", {suppressWarnings({
  skip_on_cran()
  data("regddm_tutorial")

  # factor in trial-level variable
  model = list(
    v ~ x2,
    y ~ v_0 + v_x2
  )
  expect_no_error(
    regddm(regddm_tutorial$data1, regddm_tutorial$data2, model)
  )

  # factor in subject-level variable
  model = list(
    y ~ v_0 * c2
  )
  expect_no_error(
    regddm(regddm_tutorial$data1, regddm_tutorial$data2, model)
  )

})})


test_that("model works for Bernoulli and Poisson family", {suppressWarnings({
  skip_on_cran()

  # for bernoulli family
  fake_data = generate_fake_data(N = 100, n_xvar = 0, n_each = 50, y_family = "bernoulli")
  model = list(y ~ v_0)
  expect_no_error(
    regddm(fake_data$data1, fake_data$data2, model, family = "bernoulli")
  )

  # for poisson family
  fake_data = generate_fake_data(N = 50, n_xvar = 0, n_each = 50, y_family = "poisson")
  model = list(y ~ v_0)
  expect_no_error(
    regddm(fake_data$data1, fake_data$data2, model, family = "poisson")
  )
})})


test_that("model works for DDM parameter as outcome", {suppressWarnings({
  skip_on_cran()
  fake_data = generate_fake_data(N = 50, n_xvar = 0, n_each = 50)
  model = list(v_0 ~ c1 + a_0)
  expect_no_error(regddm(fake_data$data1, fake_data$data2, model))
})})


test_that("model works with prior = FALSE", {suppressWarnings({
  data("regddm_tutorial")
  skip_on_cran()

  model = list(v ~ x1, y ~ v_0 + v_x1 + c1)
  expect_no_error(
    regddm(
      regddm_tutorial$data1,
      regddm_tutorial$data2,
      model,
      prior =  FALSE
    )
  )
})})




