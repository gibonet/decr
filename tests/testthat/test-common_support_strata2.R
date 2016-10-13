context("test common_support_strata2")

test_that("common_support_strata2 works", {
  data(invented_wages)
  out2 <- common_support_strata2(invented_wages, treatment = "gender",
                                 variables = c("sector", "education"),
                                 y = "wage", weights = "sample_weights")
  expect_is(object = out2, class = "data.frame")
})

test_that("common_support_strata2 with and without weights", {
  data(invented_wages)
  invented_wages$ones <- 1L
  out2 <- common_support_strata2(invented_wages, treatment = "gender",
                                 variables = c("sector", "education"),
                                 y = "wage", weights = "ones")
  out3 <- common_support_strata2(invented_wages, treatment = "gender",
                                 variables = c("sector", "education"),
                                 y = "wage", weights = NULL)
  expect_equal(object = out2, expected = out3)
})


test_that("common_support_strata2 output has rows equal to starting data", {
  data(invented_wages)
  out2 <- common_support_strata2(invented_wages, treatment = "gender",
                                 variables = c("sector", "education"),
                                 y = "wage", weights = "sample_weights")
  expect_equal(object = nrow(out2), expected = nrow(invented_wages))
})


