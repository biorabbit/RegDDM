# the following codes needs to fit stan models, which takes 4 cores and a long time
# some tests will not be performed on CRAN but locally.
test_that("example code works", {
  skip_on_cran()
  data(regddm_tutorial)
  model = list(v ~ x1, y ~ v_0 + v_x1 + c1)
  expect_no_error(
    regddm(
      regddm_tutorial$data1,
      regddm_tutorial$data2,
      model,
      stan_filename = ""
    )
  )
})


test_that("data check works", {
  data("regddm_tutorial")

  # test that duplicate id will trigger an error
  data1 = regddm_tutorial$data1
  data2 = regddm_tutorial$data2
  data1$id[1:2] = c(1,1)
  model = list()
  expect_error(regddm(data1, data2, model, fit_model = FALSE, stan_filename = ""))

  # test that missing id will trigger an error
  data1 = dplyr::select(regddm_tutorial$data1, -id)
  expect_error(regddm(data1, data2, model, fit_model = FALSE, stan_filename = ""))

  data1 = regddm_tutorial$data1
  data2 = dplyr::select(regddm_tutorial$data2, -id)
  expect_error(regddm(data1, data2, model, fit_model = FALSE, stan_filename = ""))

  data2 = dplyr::select(regddm_tutorial$data2, -rt)
  expect_error(regddm(data1, data2, model, fit_model = FALSE, stan_filename = ""))

  data2 = dplyr::select(regddm_tutorial$data2, -response)
  expect_error(regddm(data1, data2, model, fit_model = FALSE, stan_filename = ""))

  # test that improper coding of response will trigger an error
  data2 = dplyr::mutate(regddm_tutorial$data2, response = ifelse(response == 1, "upper", "lower"))
  expect_error(regddm(data1, data2, model, fit_model = FALSE, stan_filename = ""))

  # test that abnormal value of response time rt will trigger an warning
  data2 = dplyr::mutate(regddm_tutorial$data2, rt = rt* 100)
  expect_warning(regddm(data1, data2, model, fit_model = FALSE, stan_filename = ""))

  # test that missing values in data2 will trigger an error
  data2 = regddm_tutorial$data2
  data2[["x1"]][1] = NA
  model = list(v ~ x1)
  expect_error(regddm(data1, data2, model, fit_model = FALSE, stan_filename = ""))

  # test that data1 and data2 must have exactly the same subjects
  data1 = regddm_tutorial$data1
  data2 = regddm_tutorial$data2
  data1[["id"]][1] = 1001
  expect_error(regddm(data1, data2, model, fit_model = FALSE, stan_filename = ""))

})

test_that("model check works", {

  data("regddm_tutorial")
  data1 = regddm_tutorial$data1
  data2 = regddm_tutorial$data2

  # check that including a covariate that does not exist will trigger an error
  model = list(
    y ~ c3
  )
  expect_error(regddm(data1, data2, model, fit_model = FALSE, stan_filename = ""))

  # check that including a non-existing trial level variable will trigger an error
  model = list(
    v ~ x3
  )
  expect_error(regddm(data1, data2, model, fit_model = FALSE, stan_filename = ""))

  # check that improper naming of variables will trigger an error
  data1 = dplyr::rename(regddm_tutorial$data1, v_0 = c1)
  model = list(y ~ v_0)
  expect_error(regddm(data1, data2, model, fit_model = FALSE, stan_filename = ""))

})

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


test_that("model with interaction works", {
  skip_on_cran()

  # interaction in trial-level variable
  fake_data = generate_fake_data(N = 30, n_xvar = 2, n_each = 100)
  model = list(
    v ~ x1 * x2,
    y ~ v_0 + v_x1_x2
  )
  expect_no_error(
    regddm(fake_data$data1, fake_data$data2, model, stan_filename = "")
  )

  # interaction in subject-level variable
  fake_data = generate_fake_data(N = 30, n_xvar = 1, n_each = 100)
  model = list(
    v ~ x1,
    y ~ v_0 + v_x1 * c1
  )
  expect_no_error(
    regddm(fake_data$data1, fake_data$data2, model, stan_filename = "")
  )
})


test_that("model works for factor variables in both data1 and data2", {
  skip_on_cran()
  data("regddm_tutorial")

  # factor in trial-level variable
  model = list(
    v ~ x2,
    y ~ v_0 + v_x2
  )
  expect_no_error(
    regddm(regddm_tutorial$data1, regddm_tutorial$data2, model, stan_filename = "")
  )

  # factor in subject-level variable
  model = list(
    y ~ v_0 * c2
  )
  expect_no_error(
    regddm(regddm_tutorial$data1, regddm_tutorial$data2, model, stan_filename = "")
  )

})


test_that("model works for Bernoulli and Poisson family", {
  skip_on_cran()

  # for bernoulli family
  fake_data = generate_fake_data(N = 100, n_xvar = 0, n_each = 50, y_family = "bernoulli")
  model = list(y ~ v_0)
  expect_no_error(
    regddm(fake_data$data1, fake_data$data2, model, stan_filename = "", family = "bernoulli")
  )

  # for poisson family
  fake_data = generate_fake_data(N = 50, n_xvar = 0, n_each = 50, y_family = "poisson")
  model = list(y ~ v_0)
  expect_no_error(
    regddm(fake_data$data1, fake_data$data2, model, stan_filename = "", family = "poisson")
  )

})


test_that("model works for DDM parameter as outcome", {
  skip_on_cran()
  fake_data = generate_fake_data(N = 50, n_xvar = 0, n_each = 50)
  model = list(v_0 ~ c1 + a_0)
  expect_no_error(regddm(fake_data$data1, fake_data$data2, model, stan_filename = ""))
})


test_that("model works with prior = FALSE", {
  data("regddm_tutorial")
  skip_on_cran()

  model = list(v ~ x1, y ~ v_0 + v_x1 + c1)
  expect_no_error(
    regddm(
      regddm_tutorial$data1,
      regddm_tutorial$data2,
      model,
      stan_filename = "",
      prior =  FALSE
    )
  )
})




