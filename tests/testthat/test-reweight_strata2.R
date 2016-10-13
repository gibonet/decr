context("test reweight_strata2")

test_that("reweight_strata2 works", {
  data(invented_wages)
  out2 <- common_support_strata2(invented_wages, treatment = "gender",
                                 variables = c("sector", "education"),
                                 y = "wage", weights = "sample_weights")
  out3 <- fhat_strata2(out2)
  out4 <- reweight_strata2(out3)
  expect_is(object = out4, class = "data.frame")
})
