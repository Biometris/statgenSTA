context("Class STA")

### Summary.

modelSp <- fitTD(testTD, design = "rowcol", traits = "t1")
test_that("checks in summary.STA functions properly", {
  expect_error(summary(modelSp, trials = "E2"),
               "trials has to be a character vector defining trials in modelSp")
  expect_error(summary(modelSp, trait = "t5"),
               "trait has to be a single character string defining a column in")
  expect_error(summary(modelSp, trait = "t2"),
               "No fitted model found for t2 in E1")
  modelSp2 <- fitTD(testTD, design = "rowcol", traits = c("t1", "t2"))
  expect_error(summary(modelSp2), "No trait provided but multiple traits found")
})

test_that("summary.STA produces correct output for SpATS", {
  sumSp <- summary(modelSp)
  expect_length(sumSp, 7)
  expect_null(sumSp$selSpatMod)
  expect_equal(nrow(sumSp$stats), 9)
  expect_equal(dim(sumSp$meanTab), c(15, 4))
  expect_equivalent(sumSp$heritability, 0.65)
  expect_equal(nrow(sumSp$sed), 0)
  expect_equal(nrow(sumSp$lsd), 0)
  expect_null(sumSp$spatSumTab)
})

test_that("summary.STA produces correct output for lme4", {
  modelLm <- fitTD(testTD, design = "rowcol", traits = "t1", engine = "lme4")
  sumLm <- summary(modelLm)
  expect_length(sumLm, 7)
  expect_null(sumLm$selSpatMod)
  expect_equal(nrow(sumLm$stats), 9)
  expect_equal(dim(sumLm$meanTab), c(15, 4))
  expect_equivalent(sumLm$heritability, 0.444115736298294,
                    ## Added tolerance for new defaults in lme4-1.2-23.
                    tolerance = 1e-5)
  expect_equal(nrow(sumLm$sed), 0)
  expect_equal(nrow(sumLm$lsd), 0)
  expect_null(sumLm$spatSumTab)
})

test_that("summary.STA produces correct output for asreml", {
  skip_if_not_installed("asreml")
  modelAs <- fitTD(testTD, design = "rowcol", traits = "t1", engine = "asreml")
  sumAs <- summary(modelAs)
  expect_length(sumAs, 7)
  expect_null(sumAs$selSpatMod)
  expect_equal(nrow(sumAs$stats), 9)
  expect_equal(dim(sumAs$meanTab), c(15, 4))
  expect_equivalent(sumAs$heritability, 0.615070547646503)
  expect_equal(nrow(sumAs$sed), 3)
  expect_equal(nrow(sumAs$lsd), 3)
  expect_null(sumAs$spatSumTab)
})

test_that("summary.STA produces correct output for asreml with spatial models", {
  skip_if_not_installed("asreml")
  modelAsTs <- fitTD(testTD, design = "ibd", traits = "t1",
                     spatial = TRUE, engine = "asreml")
  sumAsTs <- summary(modelAsTs)
  expect_length(sumAsTs, 7)
  expect_equal(sumAsTs$selSpatMod, "none")
  expect_equal(nrow(sumAsTs$stats), 9)
  expect_equal(dim(sumAsTs$meanTab), c(15, 4))
  expect_equivalent(sumAsTs$heritability, 0.615070384479675)
  expect_equal(nrow(sumAsTs$sed), 3)
  expect_equal(nrow(sumAsTs$lsd), 3)
  expect_equal(dim(sumAsTs$spatSumTab), c(7, 10))
})


test_that("option sortBy functions properly for summary.STA", {
  sumSp1 <- summary(modelSp)
  sumSp2 <- summary(modelSp, sortBy = "BLUEs")
  sumSp3 <- summary(modelSp, sortBy = "BLUPs")
  expect_equal(sumSp1, sumSp2)
  expect_equal(rank(sumSp3$meanTab[["BLUPs"]]), 15:1)
})

test_that("summary.STA produces correct output for multiple trials", {
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

test_that("subsetting STA objects works correctly", {
  expect_is(modelSp["E1"], "STA")
  expect_is(modelSp["E1"], "list")
  expect_equal(attr(modelSp, "timestamp"), attr(modelSp["E1"], "timestamp"))
})

### Print summary.

test_that("print.summary.STA functions properly", {
  sumSp <- capture.output(summary(modelSp))
  sumSp2 <- capture.output(summary(modelSp, nBest = NA))
  expect_true(all(c("Summary statistics for t1 in E1  ",
                    "Estimated heritability ",
                    "Predicted means (BLUEs & BLUPs) ") %in% sumSp))
  expect_false(any(grepl("Best", sumSp2)))
  skip_if_not_installed("asreml")
  modelAs <- fitTD(testTD, design = "rowcol", traits = "t1", engine = "asreml")
  modelAsTs <- fitTD(testTD, design = "ibd", traits = "t1", spatial = TRUE,
                     engine = "asreml")
  sumAs <- capture.output(summary(modelAs))
  sumAsTs <- capture.output(summary(modelAsTs))
  expect_true(all(c("Standard Error of Difference (genotype modeled as fixed effect) ",
                    "Least Significant Difference (genotype modeled as fixed effect) ") %in%
                    sumAs))
  expect_true(all(c("Overview of tried spatial models ",
                    "Selected spatial model:  none ") %in%
                    sumAsTs))
})

test_that("print.summary.STA functions properly for multiple trials", {
  testTD[["E2"]] <- testTD[["E1"]]
  testTD[["E2"]][["trial"]] <- "E2"
  modelSp <- fitTD(testTD, design = "rowcol", traits = "t1")
  sumSp <- capture.output(summary(modelSp))
  expect_true("Summary statistics for BLUEs of t1 " %in% sumSp)
})

### STAtoCross.

test_that("checks in STAtoCross function properly", {
  expect_error(STAtoCross(1), "STA is not a valid object of class STA")
  expect_error(STAtoCross(modelSp, traits = "t5"),
               "The following traits are not modeled for E1: t5")
  expect_error(STAtoCross(modelSp, trial = "E2"),
               "single character string defining a trial in STA")
  expect_error(STAtoCross(STA = modelSp, genoFile = 1),
               "genoFile is not a valid filename")
  modelSp2 <- fitTD(TD = testTD, design = "rowcol", traits = "t1",
                    trials = c("E1", "E1"))
  expect_error(STAtoCross(modelSp2), "No trial provided but multiple trials")
})

test_that("function STAtoCross functions properly", {
  myModel <- fitTD(TD = TDHeat05, design = "res.rowcol", traits = "yield")
  cross <- STAtoCross(STA = myModel,
                      genoFile = system.file("extdata", "markers.csv",
                                             package = "statgenSTA"))
  expect_is(cross, "cross")
  expect_is(cross$pheno, "data.frame")
  expect_equal(dim(cross$pheno), c(169, 2))
})

### STAtoTD.

test_that("checks in STAtoTD function properly", {
  expect_error(STAtoTD(1), "STA is not a valid object of class STA")
  expect_error(STAtoTD(modelSp, traits = "t5"),
               "traits has to be a character vector defining columns in")
  modelSp2a <- fitTD(testTD, design = "rowcol", traits = "t1", what = "fixed")
  modelSp2b <- fitTD(testTD, design = "rowcol", traits = "t1", what = "random")
  expect_warning(STAtoTD(modelSp2a, traits = "t1"),
                 "BLUPs and seBLUPs can only be extracted if a model with")
  expect_warning(STAtoTD(modelSp2b, traits = "t1"),
                 "BLUEs and seBLUEs can only be extracted if a model with")
  expect_error(suppressWarnings(STAtoTD(modelSp2a, traits = "t1",
                                        what = "BLUPs")),
               "No statistics left to extract.")
  expect_warning(STAtoTD(modelSp2b, traits = "t1", addWt = TRUE),
                 "Weights can only be added if a model with genotype fixed is")
})

test_that("function STAtoTD functions properly", {
  TDSp <- STAtoTD(STA = modelSp)
  expect_is(TDSp, "TD")
  expect_equal(colnames(TDSp$E1),
               c("genotype", "trial", "BLUEs_t1", "seBLUEs_t1", "BLUPs_t1",
                 "seBLUPs_t1"))
  TDSp2 <- STAtoTD(STA = modelSp, what = "BLUEs")
  expect_named(TDSp2$E1, c("genotype", "trial", "t1"))
  expect_warning(TDSp3 <- STAtoTD(STA = modelSp, what = "BLUEs", addWt = TRUE),
                 "Weights can only be added together with seBLUEs")
  expect_named(TDSp3$E1, c("genotype", "trial", "BLUEs_t1", "seBLUEs_t1", "wt"))
  expect_warning(TDSp4 <- STAtoTD(STA = modelSp, keep = "family"),
                 "Duplicate values for")
  expect_named(TDSp4$E1, c("genotype", "trial", "BLUEs_t1", "seBLUEs_t1",
                           "BLUPs_t1", "seBLUPs_t1"))
})

### Report.

test_that("checks in report.STA function properly", {
  skip_on_cran()
  skip_on_ci()
  expect_error(report(modelSp, trials = "E2"),
               "trials has to be a character vector defining trials in modelSp")
  expect_error(report(modelSp, traits = 1),
               "traits should be NULL or a character vector")
  expect_error(report(modelSp, trial = "E1", trait = "t1", outfile = "tmp.pd"),
               "Invalid output filename provided")
  expect_error(report(modelSp, trial = "E1", trait = "t1", outfile = "t m.pdf"),
               "outfile path cannot contain spaces")
  expect_warning(report(modelSp, traits = "t5"),
                 "The following traits are not modeled for E1: t5")
  modelSp2a <- fitTD(testTD, design = "rowcol", traits = "t1", what = "fixed")
  expect_warning(report(modelSp2a, traits = "t1", what = "random"),
                 "Model with genotype random not available for")
})

test_that("function report.STA functions properly", {
  ## Reporting doesn't work on cran because of usage of pdflatex.
  skip_on_cran()
  skip_on_ci()
  tmpFile <- tempfile(fileext = ".pdf")
  expect_silent(report(modelSp, trial = "E1", trait = "t1", outfile = tmpFile))
  expect_true(file.exists(paste0(tools::file_path_sans_ext(tmpFile),
                                 "_E1_t1_fixed.pdf")))
  expect_true(file.exists(paste0(tools::file_path_sans_ext(tmpFile),
                                 "_E1_t1_fixed.tex")))
  expect_silent(report(modelSp, trial = "E1", trait = "t1"))
  ## Cleanup.
  unlink(c("modelReport_*", "figures"), recursive = TRUE)
})
