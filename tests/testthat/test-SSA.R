context("Class SSA")

### Summary.

modelSp <- fitTD(testTD, design = "rowcol", traits = "t1")
test_that("checks in summary.SSA functions properly", {
  expect_error(summary(modelSp, trials = "E2"),
               "trials has to be a character vector defining trials in modelSp")
  expect_error(summary(modelSp, trait = "t5"),
               "trait has to be a single character string defining a column in")
  expect_error(summary(modelSp, trait = "t2"),
               "No fitted model found for t2 in E1")
  modelSp2 <- fitTD(testTD, design = "rowcol", traits = c("t1", "t2"))
  expect_error(summary(modelSp2), "No trait provided but multiple traits found")
})

test_that("summary.SSA produces correct output for SpATS", {
  sumSp <- summary(modelSp)
  expect_length(sumSp, 6)
  expect_null(sumSp$selSpatMod)
  expect_equal(nrow(sumSp$stats), 9)
  expect_equal(dim(sumSp$meanTab), c(15, 4))
  expect_equivalent(sumSp$heritability, 0.65)
  expect_equal(nrow(sumSp$sed), 0)
  expect_equal(nrow(sumSp$lsd), 0)
})

test_that("summary.SSA produces correct output for lme4", {
  modelLm <- fitTD(testTD, design = "rowcol", traits = "t1", engine = "lme4")
  sumLm <- summary(modelLm)
  expect_length(sumLm, 6)
  expect_null(sumLm$selSpatMod)
  expect_equal(nrow(sumLm$stats), 9)
  expect_equal(dim(sumLm$meanTab), c(15, 4))
  expect_equivalent(sumLm$heritability, 0.444115736298294)
  expect_equal(nrow(sumLm$sed), 0)
  expect_equal(nrow(sumLm$lsd), 0)
})

test_that("summary.SSA produces correct output for asreml", {
  skip_on_cran()
  modelAs <- fitTD(testTD, design = "rowcol", traits = "t1", engine = "asreml")
  sumAs <- summary(modelAs)
  expect_length(sumAs, 6)
  expect_null(sumAs$selSpatMod)
  expect_equal(nrow(sumAs$stats), 9)
  expect_equal(dim(sumAs$meanTab), c(15, 4))
  expect_equivalent(sumAs$heritability, 0.615070547646503)
  expect_equal(nrow(sumAs$sed), 3)
  expect_equal(nrow(sumAs$lsd), 3)
})

test_that("option sortBy functions properly for summary.SSA", {
  sumSp1 <- summary(modelSp)
  sumSp2 <- summary(modelSp, sortBy = "BLUEs")
  sumSp3 <- summary(modelSp, sortBy = "BLUPs")
  expect_equal(sumSp1, sumSp2)
  expect_equal(rank(sumSp3$meanTab[["BLUPs"]]), 15:1)
})

test_that("summary.SSA produces correct output for multiple trials", {
  testTD[["E2"]] <- testTD[["E1"]]
  testTD[["E2"]][["trial"]] <- "E2"
  modelSp <- fitTD(testTD, design = "rowcol", traits = "t1")
  sumSp <- summary(modelSp, traits = "t1")
  expect_length(sumSp, 3)
  expect_is(sumSp$sumTab, "matrix")
  expect_equal(dim(sumSp$sumTab), c(2, 9))
  expect_equal(sumSp$what, "BLUEs")
})

test_that("option nBest functions properly", {
  sumSp <- summary(modelSp, nBest = 5)
  expect_equal(dim(sumSp$meanTab), c(5, 4))
})

### Subsetting.

test_that("subsetting SSA objects works correctly", {
  expect_is(modelSp["E1"], "SSA")
  expect_is(modelSp["E1"], "list")
  expect_equal(attr(modelSp, "timestamp"), attr(modelSp["E1"], "timestamp"))
})

### Print summary.

test_that("print.summary.SSA functions properly", {
  sumSp <- capture.output(print(summary(modelSp)))
  sumSp2 <- capture.output(print(summary(modelSp, nBest = NA)))
  expect_true(all(c("Summary statistics for t1 in E1  ",
                    "Estimated heritability ",
                    "Predicted means (BLUEs & BLUPs) ") %in% sumSp))
  expect_false(any(grepl("Best", sumSp2)))
  skip_on_cran()
  modelAs <- fitTD(testTD, design = "rowcol", traits = "t1", engine = "asreml")
  sumAs <- capture.output(print(summary(modelAs)))
  expect_true(all(c("Standard Error of Difference (genotype modeled as fixed effect) ",
                    "Least Significant Difference (genotype modeled as fixed effect) ") %in%
                    sumAs))
})

test_that("print.summary.SSA functions properly for multiple trials", {
  testTD[["E2"]] <- testTD[["E1"]]
  testTD[["E2"]][["trial"]] <- "E2"
  modelSp <- fitTD(testTD, design = "rowcol", traits = "t1")
  sumSp <- capture.output(print(summary(modelSp)))
  expect_true("Summary statistics for BLUEs of t1 " %in% sumSp)
})

### SSAtoCross.

test_that("checks in SSAtoCross function properly", {
  expect_error(SSAtoCross(1), "SSA is not a valid object of class SSA")
  expect_error(SSAtoCross(modelSp, traits = "t5"),
               "traits has to be a character vector defining columns in")
  expect_error(SSAtoCross(modelSp, trial = "E2"),
               "single character string defining a trial in SSA")
  expect_error(SSAtoCross(SSA = modelSp, genoFile = 1),
               "genoFile is not a valid filename")
  modelSp2 <- fitTD(TD = testTD, design = "rowcol", traits = "t1",
                    trials = c("E1", "E1"))
  expect_error(SSAtoCross(modelSp2), "No trial provided but multiple trials")
})

test_that("function SSAtoCross functions properly", {
  myModel <- fitTD(TD = TDHeat05, design = "res.rowcol", traits = "yield")
  cross <- SSAtoCross(SSA = myModel,
                      genoFile = system.file("extdata", "markers.csv",
                                             package = "statgenSSA"))
  expect_is(cross, "cross")
  expect_is(cross$pheno, "data.frame")
  expect_equal(dim(cross$pheno), c(169, 2))
})

### SSAtoTD.

test_that("checks in SSAtoTD function properly", {
  expect_error(SSAtoTD(1), "SSA is not a valid object of class SSA")
  expect_error(SSAtoTD(modelSp, traits = "t5"),
               "traits has to be a character vector defining columns in")
  modelSp2a <- fitTD(testTD, design = "rowcol", traits = "t1", what = "fixed")
  modelSp2b <- fitTD(testTD, design = "rowcol", traits = "t1", what = "random")
  expect_warning(SSAtoTD(modelSp2a, traits = "t1"),
                 "BLUPs and seBLUPs can only be extracted if a model with")
  expect_warning(SSAtoTD(modelSp2b, traits = "t1"),
                 "BLUEs and seBLUEs can only be extracted if a model with")
  expect_error(suppressWarnings(SSAtoTD(modelSp2a, traits = "t1",
                                        what = "BLUPs")),
               "No statistics left to extract.")
  expect_warning(SSAtoTD(modelSp2b, traits = "t1", addWt = TRUE),
                 "Weights can only be added if a model with genotype fixed is")
})

test_that("function SSAtoTD functions properly", {
  TDSp <- SSAtoTD(SSA = modelSp)
  expect_is(TDSp, "TD")
  expect_equal(colnames(TDSp$E1),
               c("genotype", "trial", "BLUEs_t1", "seBLUEs_t1", "BLUPs_t1",
                 "seBLUPs_t1"))
  TDSp2 <- SSAtoTD(SSA = modelSp, what = "BLUEs")
  expect_named(TDSp2$E1, c("genotype", "trial", "t1"))
  expect_warning(TDSp3 <- SSAtoTD(SSA = modelSp, what = "BLUEs", addWt = TRUE),
                 "Weights can only be added together with seBLUEs")
  expect_named(TDSp3$E1, c("genotype", "trial", "BLUEs_t1", "seBLUEs_t1", "wt"))
  expect_warning(TDSp4 <- SSAtoTD(SSA = modelSp, keep = "family"),
                 "Duplicate values for")
  expect_named(TDSp4$E1, c("genotype", "trial", "BLUEs_t1", "seBLUEs_t1",
                           "BLUPs_t1", "seBLUPs_t1"))
})

### Report.

test_that("checks in report.SSA function properly", {
  skip_on_cran()
  expect_error(report(modelSp, trials = "E2"),
               "trials has to be a character vector defining trials in modelSp")
  expect_error(report(modelSp, traits = 1),
               "traits should be NULL or a character vector")
  expect_warning(report(modelSp, traits = "t5"),
                 "traits not available for trial E1")
  modelSp2a <- fitTD(testTD, design = "rowcol", traits = "t1", what = "fixed")
  expect_warning(report(modelSp2a, traits = "t1", what = "random"),
                 "Model with genotype random not available for")
})

test_that("function report.SSA functions properly" ,{
  ## Reporting doesn't work on cran because of usage of pdflatex.
  skip_on_cran()
  testTD[["E2"]] <- testTD[["E1"]]
  modelSp <- fitTD(testTD, design = "rowcol", traits = c("t1", "t2"))
  tmpFile = tempfile(fileext = ".pdf")
  expect_silent(report(modelSp, trial = "E1", trait = "t1", outfile = tmpFile))
  expect_true(file.exists(gsub(pattern = ".pdf",
                               replacement = "_E1_t1_fixed.pdf", x = tmpFile)))
  expect_true(file.exists(gsub(pattern = ".pdf",
                               replacement = "_E1_t1_fixed.tex", x = tmpFile)))
})
