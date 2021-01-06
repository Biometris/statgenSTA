context("outliers")

modelLm <- fitTD(testTD, design = "rowcol", traits = paste0("t", 1:4),
                 engine = "lme4")

test_that("checks in outliersSTA function properly", {
  expect_error(outlierSTA(), "STA should be a valid object of class STA")
  expect_error(outlierSTA(modelLm, trials = "E2"),
               "trials has to be a character vector defining trials in modelLm")
  expect_error(outlierSTA(modelLm, traits = 1) ,
               "traits should be NULL or a character vector")
  expect_warning(outlierSTA(modelLm, traits = "t5"),
                 "The following traits are not modeled for E1: t5")
  expect_error(outlierSTA(modelLm, traits = "t1", rLimit = -1),
               "rLimit should be NULL or a single numerical value greater than 0")
  expect_error(outlierSTA(modelLm, traits = "t1", commonFactors = "comFac"),
               "commonFactors has to be a character vector defining columns")
  modelLm$E1$mRand <- NULL
  expect_warning(outlierSTA(modelLm, traits = "t1", what = "random"),
                 "Model with genotype random not available for trial E1")
})

test_that("outliersSTA functions properly", {
  out1 <- outlierSTA(modelLm, trials = "E1", traits = "t1", verbose = FALSE)
  expect_is(out1, "list")
  expect_length(out1, 2)
  expect_is(out1$indicator, "list")
  expect_null(out1$indicator[["E1"]][["t1"]])
  expect_null(out1$outliers)
})

test_that("outliersSTA functions properly for multiple traits", {
  out14 <- outlierSTA(modelLm, trials = "E1", traits = paste0("t", 1:4),
                      verbose = FALSE)
  expect_is(out14, "list")
  expect_length(out14, 2)
  expect_is(out14$indicator, "list")
  expect_named(out14$indicator, "E1")
  expect_named(out14$indicator[["E1"]], paste0("t", 1:4))
  expect_null(out14$outliers)
})

test_that("option what functions properly", {
  out1 <- outlierSTA(modelLm, trials = "E1", traits = "t1", what = "random",
                     verbose = FALSE)
  expect_is(out1, "list")
  expect_length(out1, 2)
  expect_is(out1$indicator, "list")
  expect_null(out1$indicator[["E1"]][["t1"]])
  expect_null(out1$outliers)
})

test_that("option rLimit funtions properly", {
  out1 <- outlierSTA(modelLm, trials = "E1", traits = "t1", rLimit = 1,
                     verbose = FALSE)
  expect_length(out1$indicator[["E1"]][["t1"]], 6)
  expect_equal(nrow(out1$outliers), 6)
  expect_equal(out1$outliers$res,
               c(1.13191821629099, -1.27945389572384, -1.13191821629098,
                 1.27945389572384, -1.23458129246234, 1.23458129246234))
})

test_that("option rLimit funtions properly for multiple traits", {
  out14 <- outlierSTA(modelLm, trials = "E1", traits = paste0("t", 1:4),
                      rLimit = 1, verbose = FALSE)
  expect_equivalent(sapply(X = out14$indicator[["E1"]], FUN = length),
                    c(6, 10, 0, 2))
  expect_equal(nrow(out14$outliers), 18)
})

test_that("option commonFactors functions properly", {
  out1 <- outlierSTA(modelLm, trials = "E1", traits = "t1", rLimit = 1,
                     commonFactors = "subBlock", verbose = FALSE)
  expect_length(out1$indicator[["E1"]][["t1"]], 6)
  expect_equal(nrow(out1$outliers), 18)
  expect_equal(sum(out1$outliers$outlier), 6)
})

test_that("option verbose functions properly", {
  printOut1 <- capture.output(out1 <- outlierSTA(modelLm, trials = "E1",
                                                 traits = "t1", verbose = TRUE))
  printOut2 <- capture.output(out2 <- outlierSTA(modelLm, trials = "E1",
                                                 traits = "t1", what = "random",
                                                 rLimit = 1, verbose = TRUE))
  expect_equal(printOut1, "No large standardized residuals.")
  expect_true("Large standardized residuals." %in% printOut2)
  expect_true(any(grepl(pattern = "1.082710", x = printOut2)))
})
