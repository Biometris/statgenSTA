context("Statistical Helper Functions")

test_that("function seVar gives correct results", {
  expect_equal(seVar(testData$t1), 81.6643849025553)
  expect_equivalent(seVar(testData[c("t1", "t2")]),
                    c(81.6643849025553, 0.0381738341194061))
  expect_equivalent(seVar(as.matrix(testData[c("t1", "t2")])),
                    c(81.6643849025553, 0.0381738341194061))
  expect_equal(seVar(testData$t3), NA_real_)
  expect_equal(seVar(testData$t3, na.rm = TRUE), 10.7842792902852)
  expect_equivalent(seVar(testData[c("t1", "t3")]),
                    c(81.6643849025553, NA))
  expect_equivalent(seVar(testData[c("t1", "t3")], na.rm = TRUE),
                    c(81.6643849025553, 10.7842792902852))
})

test_that("function skewness gives correct results", {
  expect_equal(skewness(testData$t1), 0.626728847814835)
  expect_equivalent(skewness(testData[c("t1", "t2")]),
                    c(0.626728847814835, -0.0584844288330201))
  expect_equivalent(skewness(as.matrix(testData[c("t1", "t2")])),
                    c(0.626728847814835, -0.0584844288330201))
  expect_equal(skewness(testData$t3), NA_real_)
  expect_equal(skewness(testData$t3, na.rm = TRUE), -0.0683589419435095)
  expect_equivalent(skewness(testData[c("t1", "t3")]),
                    c(0.626728847814835, NA))
  expect_equivalent(skewness(testData[c("t1", "t3")], na.rm = TRUE),
                    c(0.626728847814835, -0.0683589419435095))
})

test_that("function seSkewness gives correct results", {
  expect_equal(seSkewness(100), 0.24137977904013)
  expect_warning(seSkew <- seSkewness(2),
                 "the standard error of skewness cannot be calculated")
  expect_equal(seSkew, NA)
})

test_that("function kurtosis gives correct results", {
  expect_equal(kurtosis(testData$t1), -0.371370665680684)
  expect_equivalent(kurtosis(testData[c("t1", "t2")]),
                    c(-0.371370665680684, -0.322837145891091))
  expect_equivalent(kurtosis(as.matrix(testData[c("t1", "t2")])),
                    c(-0.371370665680684, -0.322837145891091))
  expect_equal(kurtosis(testData$t3), NA_real_)
  expect_equal(kurtosis(testData$t3, na.rm = TRUE), -0.4226177429994)
  expect_equivalent(kurtosis(testData[c("t1", "t3")]),
                    c(-0.371370665680684, NA))
  expect_equivalent(kurtosis(testData[c("t1", "t3")], na.rm = TRUE),
                    c(-0.371370665680684, -0.4226177429994))
})

test_that("function seKurtosis gives correct results", {
  expect_equal(seKurtosis(100), 0.478331132994813)
  expect_warning(seKurt <- seKurtosis(2),
                 "the standard error of kurtosis cannot be calculated")
  expect_equal(seKurt, NA)
})


