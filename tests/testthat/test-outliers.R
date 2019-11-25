context("outliers")

modelLm <- fitTD(testTD, design = "rowcol", traits = paste0("t", 1:4),
                 engine = "lme4")

test_that("checks in outliersSSA function properly", {
  expect_error(outlierSSA(), "SSA should be a valid object of class SSA")
  expect_error(outlierSSA(modelLm, trials = "E2"),
               "trials has to be a character vector defining trials in modelLm")
  expect_error(outlierSSA(modelLm, traits = 1) ,
               "traits should be NULL or a character vector")
  expect_warning(outlierSSA(modelLm, traits = "t5"),
                 "The following traits are not modeled for E1: t5")
  expect_error(outlierSSA(modelLm, traits = "t1", rLimit = -1),
               "rLimit should be NULL or a single numerical value greater than 0")
  expect_error(outlierSSA(modelLm, traits = "t1", commonFactors = "comFac"),
               "commonFactors has to be a character vector defining columns")
  modelLm$E1$mRand <- NULL
  expect_warning(outlierSSA(modelLm, traits = "t1", what = "random"),
                 "Model with genotype random not available for trial E1")
})

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
  expect_equal(sum(out1$outliers$outlier), 4)
})

test_that("option verbose functions properly", {
  printOut1 <- capture.output(out1 <- outlierSSA(modelLm, trials = "E1",
                                                 traits = "t1", verbose = TRUE))
  printOut2 <- capture.output(out2 <- outlierSSA(modelLm, trials = "E1",
                                                 traits = "t1", what = "random",
                                                 rLimit = 2, verbose = TRUE))
  expect_equal(printOut1, "No large standardized residuals.")
  expect_true("Large standardized residuals." %in% printOut2)
  expect_true(any(grepl(pattern = "2.031948", x = printOut2)))
})
