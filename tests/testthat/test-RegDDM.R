test_that("data check works", {
  data("regddm_data")
  data1 = regddm_data$data1
  data2 = regddm_data$data2

  # test that duplicate id will trigger an error
  data1_test = data1
  data2_test = data2
  data1_test$id
  expect_equal(2 * 2, 4)



})

test_that("model check works", {

})

test_that("fake data generation works", {

})

# the following code needs to fit stan models, which takes 4 cores and a long time
# all tests will not be performed on CRAN but locally.
test_that("normal model works", {
  skip_on_cran()
})

test_that("missing covariates modeling works", {
  skip_on_cran()
})

test_that("model works for Bernoulli and Poisson family", {
  skip_on_cran()
})

test_that("model works for identical ddm link function", {
  skip_on_cran()
})

test_that("model with interaction works", {
  skip_on_cran()
})




