test_that("multiplication works", {
  expect_error(sampling_heatmap(iraceResults, param_names = "dlb"))
  expect_error(sampling_heatmap(iraceResults, param_names = c("ants","other")))
  expect_error(sampling_heatmap(iraceResults))
})
