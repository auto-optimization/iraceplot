test_that("get_oarameters_names() a string vector with the name of the parameters", {
  p_names <- iraceResults$parameters$names

  expect_identical(get_parameters_names(iraceResults), p_names)
})
