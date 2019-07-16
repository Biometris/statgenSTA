context("extract general options")

modelLm <- fitTD(testTD, design = "rcbd", traits = "t1", engine = "lme4")
test_that("checks in extract function properly", {
  expect_error(extract(1), "SSA has to be an object of class SSA")
  expect_error(extract(modelLm, traits = 1),
               "traits should be NULL or a character vector")
  expect_error(extract(modelLm, keep = 1),
               "keep should be NULL or a character vector")
  expect_error(extract(modelLm, traits = "t5"),
               "All traits should be columns in E1")
  expect_error(extract(modelLm, keep = "myKp"),
               "All keep should be columns in E1")
  expect_null(extract(modelLm, traits = "t2")$E1)
})

test_that("option keep functions properly", {
  expect_named(extract(modelLm, what = "BLUEs", keep = "trial")[[1]][["BLUEs"]],
               c("genotype", "trial", "t1"))
  expect_named(extract(modelLm, what = "fitted", keep = "repId")[[1]][["fitted"]],
               c("genotype", "repId", "t1"))
  ## Columns that have duplicate values should be dropped with a warning.
  expect_warning(ext <- extract(modelLm, what = "BLUEs", keep = "checkId"),
                 "Duplicate values for")
  expect_named(ext[[1]][["BLUEs"]], c("genotype", "t1"))
  expect_warning(ext2 <- extract(modelLm, what = "BLUEs",
                                 keep = c("trial", "checkId")),
                 "Duplicate values for")
  expect_named(ext2[[1]][["BLUEs"]], c("genotype", "trial", "t1"))
})

test_that("option restoreColNames functions properly", {
  ## Restoring original colnames should work with and without keeping columns.
  expect_named(extract(modelLm, what = "BLUEs",
                       restoreColNames = TRUE)[[1]][["BLUEs"]],
               c("seed", "t1"))
  expect_named(extract(modelLm, what = "BLUEs", keep = "trial",
                       restoreColNames = TRUE)[[1]][["BLUEs"]],
               c("seed", "field", "t1"))
  ## Duplicate mappings are a potential problem.
  expect_named(extract(modelLm, what = "fitted",
                       keep = c("rowCoord", "rowId"),
                       restoreColNames = TRUE)[[1]][["fitted"]],
               c("seed", "rep", "Y", "t1"))
})

test_that("function createBaseData functions properly for bdPred = FALSE", {
  testDF <- testTD[["E1"]]
  ## Setting no extra options should just copy column predicted.
  expect_equal(createBaseData(testDF, predicted = "genotype")$baseData$genotype,
               testDF$genotype)
  ## Keep should copy those columns as well.
  expect_equal(createBaseData(testDF, predicted = "genotype",
                              keep = c("family", "trial"))$baseData,
               testDF[, c("genotype", "family", "trial")])
  ## useRepId = TRUE should copy repId as well.
  expect_equal(createBaseData(testDF, predicted = "genotype",
                              useRepId = TRUE)$baseData,
               testDF[, c("genotype", "repId")])
})

test_that("function createBaseData functions properly for bdPred = TRUE", {
  testDF <- testTD[["E1"]]
  ## Setting no extra options should copy unique values in predicted.
  expect_equal(createBaseData(testDF, predicted = "genotype",
                              bdPred = TRUE)$baseDataPred$genotype,
               unique(testDF$genotype))
  ## Keep should copy those columns as well.
  ## A warning should be issued for columns that contain duplicate values.
  expect_warning(testBD <- createBaseData(testDF, predicted = "genotype",
                                          keep = c("family", "trial"),
                                          bdPred = TRUE)$baseDataPred,
                 'Duplicate values for "family"')
  expect_named(testBD, c("genotype", "trial"))
})

test_that("function restoreColNames functions properly", {
  testDF <- testTD[["E1"]]
  testRen <- attr(x = testDF, which = "renamedCols")
  ## Running with restore = FALSE should do nothing.
  expect_equal(testDF, restoreColNames(testDF))
  ## Restoring a single column.
  expect_named(restoreColNames(testDF, renamedCols = testRen[1, ],
                               restore = TRUE),
               c("seed", "family", "trial", "repId", "checkId", "colId",
                 "rowId", "subBlock", "t1", "t2", "t3", "t4", "rowCoord",
                 "colCoord"))
  ## Restoring multiple non-adjacent columns.
  expect_named(restoreColNames(testDF, renamedCols = testRen[c(1, 3), ],
                               restore = TRUE),
               c("seed", "family", "trial", "rep", "checkId", "colId",
                 "rowId", "subBlock", "t1", "t2", "t3", "t4", "rowCoord",
                 "colCoord"))
  ## Set of columns with the same original column.
  expect_named(restoreColNames(testDF, renamedCols = testRen[c(5, 7), ],
                               restore = TRUE),
               c("genotype", "family", "trial", "repId", "checkId", "colId",
                 "Y", "subBlock", "t1", "t2", "t3", "t4", "colCoord"))
  ## Attributes should be kept.
  expect_named(attributes(restoreColNames(testDF, renamedCols = testRen,
                                          restore = TRUE)),
               c("names", "row.names", "trLocation", "renamedCols", "class"))
})






