context("outliers")

modelLm <- fitTD(testTD, design = "rowcol", traits = paste0("t", 1:4),
                 engine = "lme4")

test_that("outliersSSA functions properly", {
  out1 <- outlierSSA(modelLm, trials = "E1", traits = "t1", verbose = FALSE)
  expect_is(out1, "list")
  expect_length(out1, 2)
  expect_is(out1$indicator, "list")
  expect_null(out1$indicator[["E1"]][["t1"]])
  expect_null(out1$outliers)
})

test_that("outliersSSA functions properly for multiple traits", {
  out14 <- outlierSSA(modelLm, trials = "E1", traits = paste0("t", 1:4),
                      verbose = FALSE)
  expect_is(out14, "list")
  expect_length(out14, 2)
  expect_is(out14$indicator, "list")
  expect_named(out14$indicator, "E1")
  expect_named(out14$indicator[["E1"]], paste0("t", 1:4))
  expect_null(out14$outliers)
})

test_that("option what functions properly", {
  out1 <- outlierSSA(modelLm, trials = "E1", traits = "t1", what = "random",
                     verbose = FALSE)
  expect_is(out1, "list")
  expect_length(out1, 2)
  expect_is(out1$indicator, "list")
  expect_null(out1$indicator[["E1"]][["t1"]])
  expect_null(out1$outliers)
})

test_that("option rLimit funtions properly", {
  out1 <- outlierSSA(modelLm, trials = "E1", traits = "t1", rLimit = 1,
                     verbose = FALSE)
  expect_length(out1$indicator[["E1"]][["t1"]], 4)
  expect_equal(nrow(out1$outliers), 4)
  expect_equal(out1$outliers$res, c(1.73562640255469, -1.73562640255469,
                                    1.48592104460245, -1.48592104460245))
})

test_that("option rLimit funtions properly for multiple traits", {
  out14 <- outlierSSA(modelLm, trials = "E1", traits = paste0("t", 1:4),
                      rLimit = 1, verbose = FALSE)
  expect_equivalent(sapply(X = out14$indicator[["E1"]], FUN = length),
                    c(4, 6, 2, 2))
  expect_equal(nrow(out14$outliers), 14)
})

test_that("option commonFactors functions properly", {
  out1 <- outlierSSA(modelLm, trials = "E1", traits = "t1", rLimit = 1,
                     commonFactors = "subBlock", verbose = FALSE)
  expect_length(out1$indicator[["E1"]][["t1"]], 4)
  expect_equal(nrow(out1$outliers), 12)
  expect_equal(sum(out1$outliers$similar), 8)
})

test_that("option verbose functions properly", {
  printOut1 <- capture.output(out1 <- outlierSSA(modelLm, trials = "E1",
                                                 traits = "t1", verbose = TRUE))
  printOut2 <- capture.output(out1 <- outlierSSA(modelLm, trials = "E1",
                                                 traits = "t1", what = "random",
                                                 rLimit = 2, verbose = TRUE))
  expect_equal(printOut1, "No large standardized residuals.")
  expect_true("Large standardized residuals." %in% printOut2)
  expect_true(any(grepl(pattern = "2.031948", x = printOut2)))
})
