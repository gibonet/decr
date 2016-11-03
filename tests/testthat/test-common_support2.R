context("test common_support2")

test_that("common_support2 works", {
  data(invented_wages)
  out <- common_support2(data = invented_wages,
                         treatment = "gender",
                         variables = c("sector", "education"))
  expect_is(object = out, class = "data.frame")
})

test_that("common_support2 doesn't produce NA in the common_support column", {
  tmp <- data.frame(g = sample(c("A", "B"), size = 20, replace = TRUE),
                    x1 = sample(c("A", "B", NA), size = 20, replace = TRUE),
                    x2 = sample(c("C", "D", NA), size = 20, replace = TRUE))
  out <- common_support2(data = tmp, treatment = "g", variables = c("x1", "x2"))
  expect_true(sum(is.na(out$common_support)) == 0)
})
