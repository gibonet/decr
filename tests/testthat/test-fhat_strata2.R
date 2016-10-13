context("test fhat_strata2")

test_that("fhat_strata2 works", {
  data(invented_wages)
  out2 <- common_support_strata2(invented_wages, treatment = "gender",
                                 variables = c("sector", "education"),
                                 y = "wage", weights = "sample_weights")
  out3 <- fhat_strata2(out2)
  expect_is(object = out3, class = "data.frame")
})
