# the following codes needs to fit stan models, which takes 4 cores and a long time
# some tests will not be performed on CRAN but locally.
test_that("example code works", {
  skip_on_cran()
  data(regddm_data)
  data1 = regddm_data$data1
  data2 = regddm_data$data2
  model = list(v ~ memload, y ~ v_0 + v_memload + age + education)
  fit = regddm(
    regddm_data$data1,
    regddm_data$data2,
    model,
    fit_model = FALSE,
    warmup = 500,
    iter = 1000
  )
})


test_that("data check works", {
  data("regddm_data")
  data1 = regddm_data$data1
  data2 = regddm_data$data2
  model = list(
    v ~ memload,
    y ~ v_memload + v_0 + age + education
  )

  # test that duplicate id will trigger an error
  data1_test = data1
  data2_test = data2
  data1_test$id[1:2] = c(1,1)
  expect_error(regddm(data1_test, data2_test, model, fit_model = FALSE))

  # test that missing required columns will trigger an error
  data1_test = dplyr::select(data1, -id)
  expect_error(regddm(data1_test, data2_test, model, fit_model = FALSE))

  data1_test = dplyr::select(data1, -y)
  expect_error(regddm(data1_test, data2_test, model, fit_model = FALSE))

  data1_test = data1
  data2_test = dplyr::select(data1, -id)
  expect_error(regddm(data1_test, data2_test, model, fit_model = FALSE))

  data2_test = dplyr::select(data1, -rt)
  expect_error(regddm(data1_test, data2_test, model, fit_model = FALSE))

  data2_test = dplyr::select(data1, -response)
  expect_error(regddm(data1_test, data2_test, model, fit_model = FALSE))

  # test that improper coding of response will trigger an error
  data2_test = dplyr::mutate(data2, response = ifelse(response == 1, "upper", "lower"))
  expect_error(regddm(data1_test, data2_test, model, fit_model = FALSE))

  # test that abnormal value of response time rt will trigger an warning
  data2_test = dplyr::mutate(data2, rt = rt* 100)
  expect_warning(regddm(data1_test, data2_test, model, fit_model = FALSE))

  # test that missing values in data2 will trigger an error
  data2_test = data2
  data2_test[["memload"]][1] = NA
  expect_error(regddm(data1_test, data2_test, model, fit_model = FALSE))

  # test that data1 and data2 must have exactly the same subjects
  data1_test = data1
  data1_test[["id"]][1] = 1
  data2_test = data2
  expect_error(regddm(data1_test, data2_test, model, fit_model = FALSE))

})

test_that("model check works"){

  data("regddm_data")
  data1 = regddm_data$data1
  data2 = regddm_data$data2

  # check that including a covariate that does not exist will trigger an error
  model_test = list(
    v ~ memload,
    y ~ v_memload + v_0 + age + education + c1
  )
  expect_error(regddm(data1, data2, model_test, fit_model = FALSE))

  # check that including a non-existing trial level variable will trigger an error
  model_test = list(
    v ~ memload + x1,
    y ~ v_memload + v_0 + age + education
  )
  expect_error(regddm(data1, data2, model_test, fit_model = FALSE))
}

test_that("fake data generation works", {
  expect_no_error(
    fake_data = generate_fake_data()
  )
})


test_that("missing covariates modeling works", {
  skip_on_cran()
  fake_data = generate_fake_data(N = 20, n_xvar = 0, n_each = 50)
  # convert continuous predictor to binary one to test missing value modeling in both conditions
  data1 = dplyr::mutate(fake_data[["data1"]], c1 = ifelse(c1 > 0, 1, 0))
  data1[["c1"]][1:2] = c(NA, NA)
  data1[["c2"]][2] = NA
  data2 = fake_data[["data2"]]
  model = list(y ~ c1 + c2 + v_0)
  expect_no_error(regddm(data1, data2, model))
})

test_that("model with interaction works", {
  skip_on_cran()
  fake_data = generate_fake_data(N = 40, n_xvar = 2, n_each = 50)
  data1 = fake_data[["data1"]]
  data2 = fake_data[["data2"]]
  model = list(
    v ~ x1 * x2,
    y ~ v_x1*c1 + v_x2*c2 + v_x1_x2
  )
  expect_no_error(regddm(data1, data2, model))

})

test_that("model works for factor variables in both data1 and data2", {
  skip_on_cran()
  # binary data is treated as factors
  fake_data = generate_fake_data(N = 50, n_xvar = 1, n_each = 100)
  data1 = dplyr::mutate(fake_data[["data1"]], c1 = factor(ifelse(c1 > 0, 1, 0)))
  data2 = dplyr::mutate(fake_data[["data2"]], x1 = factor(ifelse(x1 > 0, 1, 0)))
  model = list(
    v ~ x1,
    y ~ v_x1*c1 + v_x1 * c2
  )
  expect_no_error(regddm(data1, data2, model))

})


test_that("model works for Bernoulli and Poisson family", {
  skip_on_cran()
  # for bernoulli family, more subject is required
  fake_data = generate_fake_data(N = 100, n_xvar = 0, n_each = 30, family = "bernoulli")
  data1 = fake_data[["data1"]]
  data2 = fake_data[["data2"]]
  model = list(y ~ v_0 + c1)
  expect_no_error(regddm(data1, data2, model, family = "bernoulli"))


  fake_data = generate_fake_data(N = 50, n_xvar = 0, n_each = 50, family = "poisson")
  data1 = fake_data[["data1"]]
  data2 = fake_data[["data2"]]
  model = list(y ~ v_0 + c1)
  expect_no_error(regddm(data1, data2, model, family = "poisson"))

})

test_that("model works for identical ddm link function", {
  skip_on_cran()
  fake_data = generate_fake_data(N = 50, n_xvar = 0, n_each = 50)
  data1 = fake_data[["data1"]]
  data2 = fake_data[["data2"]]
  model = list(y ~ v_0 + c1)
  expect_no_error(regddm(data1, data2, model, ddm_link = "ident"))
})

test_that("model works with prior = FALSE", {
  skip_on_cran()
  fake_data = generate_fake_data(N = 50, n_xvar = 0, n_each = 50)
  data1 = fake_data[["data1"]]
  data2 = fake_data[["data2"]]
  model = list(y ~ v_0 + c1)
  expect_no_error(regddm(data1, data2, model, prior = FALSE))
})




