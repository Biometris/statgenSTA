context("Statistical Helper Functions")

test_that("function seVar gives correct results", {
  expect_equal(seVar(testData$t1), 80.1983989585527)
  expect_equivalent(seVar(testData[c("t1", "t2")]),
                    c(80.1983989585527, 0.0386387244078258))
  expect_equivalent(seVar(as.matrix(testData[c("t1", "t2")])),
                    c(80.1983989585527, 0.0386387244078258))
  expect_equal(seVar(testData$t3), NA_real_)
  expect_equal(seVar(testData$t3, na.rm = TRUE), 12.7296670386034)
  expect_equivalent(seVar(testData[c("t1", "t3")]),
                    c(80.1983989585527, NA))
  expect_equivalent(seVar(testData[c("t1", "t3")], na.rm = TRUE),
                    c(80.1983989585527, 12.7296670386034))
})

test_that("function skewness gives correct results", {
  expect_equal(skewness(testData$t1), 0.400950835964985)
  expect_equivalent(skewness(testData[c("t1", "t2")]),
                    c(0.400950835964985, -0.215149588132529))
  expect_equivalent(skewness(as.matrix(testData[c("t1", "t2")])),
                    c(0.400950835964985, -0.215149588132529))
  expect_equal(skewness(testData$t3), NA_real_)
  expect_equal(skewness(testData$t3, na.rm = TRUE), -0.201787119775979)
  expect_equivalent(skewness(testData[c("t1", "t3")]),
                    c(0.400950835964985, NA))
  expect_equivalent(skewness(testData[c("t1", "t3")], na.rm = TRUE),
                    c(0.400950835964985, -0.201787119775979))
})

test_that("function seKurtosis gives correct results", {
  expect_equal(seSkewness(100), 0.24137977904013)
  expect_warning(seSkew <- seSkewness(2),
                 "the standard error of skewness cannot be calculated")
  expect_equal(seSkew, NA)
})

test_that("function kurtosis gives correct results", {
  expect_equal(kurtosis(testData$t1), -0.342794098910884)
  expect_equivalent(kurtosis(testData[c("t1", "t2")]),
                    c(-0.342794098910884, 0.105277086648978))
  expect_equivalent(kurtosis(as.matrix(testData[c("t1", "t2")])),
                    c(-0.342794098910884, 0.105277086648978))
  expect_equal(kurtosis(testData$t3), NA_real_)
  expect_equal(kurtosis(testData$t3, na.rm = TRUE), -0.455004238429161)
  expect_equivalent(kurtosis(testData[c("t1", "t3")]),
                    c(-0.342794098910884, NA))
  expect_equivalent(kurtosis(testData[c("t1", "t3")], na.rm = TRUE),
                    c(-0.342794098910884, -0.455004238429161))
})

test_that("function seKurtosis gives correct results", {
  expect_equal(seKurtosis(100), 0.478331132994813)
  expect_warning(seKurt <- seKurtosis(2),
                 "the standard error of kurtosis cannot be calculated")
  expect_equal(seKurt, NA)
})


