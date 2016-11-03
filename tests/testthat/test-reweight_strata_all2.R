context("test reweight_strata_all2")

test_that("reweight_strata_all2 works", {
  data(invented_wages)
  out <- reweight_strata_all2(invented_wages, treatment = "gender",
                              variables = c("sector", "education"),
                              y = "wage", weights = "sample_weights")
  expect_is(object = out, class = "data.frame")
})


test_that("reweight_strata_all2 output has rows equal to starting data", {
  data(invented_wages)
  out <- reweight_strata_all2(invented_wages, treatment = "gender",
                              variables = c("sector", "education"),
                              y = "wage", weights = "sample_weights")
  expect_equal(object = nrow(out), expected = nrow(invented_wages))
})


test_that("reweight_strata_all3 works", {
  data(invented_wages)
  out2 <- common_support_strata2(invented_wages, treatment = "gender",
                                 variables = c("sector", "education"),
                                 y = "wage", weights = "sample_weights")
  out3 <- reweight_strata_all3(.cs_strata = out2)
  expect_is(object = out3, class = "data.frame")
})


test_that("reweight_strata_all3 output has rows equal to starting data", {
  data(invented_wages)
  out2 <- common_support_strata2(invented_wages, treatment = "gender",
                                 variables = c("sector", "education"),
                                 y = "wage", weights = "sample_weights")
  out3 <- reweight_strata_all3(.cs_strata = out2)
  expect_equal(object = nrow(out3), expected = nrow(invented_wages))
})

