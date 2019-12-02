context("Modeling")

## Helper function for testing base structure that has to be consistent
## for all SSA objects independent of engine and options.
expect_SSA <- function(SSA) {
  test_that(paste(deparse(substitute(SSA)), "has correct SSA structure"), {
    expect_is(SSA, "SSA")
    for (tr in names(SSA)) {
      expect_length(SSA[[tr]], 9)
      expect_named(SSA[[tr]], c("mRand", "mFix", "TD", "traits", "design",
                                "spatial", "engine", "predicted", "sumTab"))
      expect_is(SSA[[tr]]$TD, "TD")
    }
  })
}

## Helper function for testing base structure for fitted models within an
## SSA object. Param class is used to indicate the output type of the fitted
## model. Normally this is identical to the engine in SSA but for lme4 this
## can vary depending on the fitted model.
expect_SSAMod <- function(SSA,
                          what,
                          class = NULL) {
  if (is.null(class)) {
    class <- SSA[[1]]$engine
  }
  for (tr in names(SSA)) {
    SSAMod <- SSA[[tr]][[what]]
    test_that(paste(deparse(substitute(what)), "in", deparse(substitute(SSA)),
                    "has correct structure"), {
                      expect_is(SSAMod, "list")
                      expect_length(SSAMod, length(SSA[[tr]]$traits))
                      expect_named(SSAMod, SSA[[tr]]$traits)
                      for (trait in SSA[[tr]]$traits) {
                        expect_is(SSAMod[[trait]], class)
                      }
                    })
  }
}

test_that("running models creates objects with correct structure", {
  modelSp <- fitTD(testTD, design = "rowcol", traits = "t1")
  expect_SSA(modelSp)
  expect_SSAMod(modelSp, "mRand")
  expect_SSAMod(modelSp, "mFix")
  expect_equal(modelSp[["E1"]]$traits, "t1")
  expect_equal(modelSp[["E1"]]$design, "rowcol")
  expect_equal(modelSp[["E1"]]$spatial[["t1"]], "2 dimensional P-splines")
  expect_equal(modelSp[["E1"]]$engine, "SpATS")
})

test_that("option what produces expected output", {
  modelSp <- fitTD(testTD, design = "res.ibd", traits = "t1")
  modelSpF <- fitTD(testTD, design = "res.ibd", traits = "t1", what = "fixed")
  expect_SSA(modelSpF)
  expect_null(modelSpF[["E1"]]$mRand)
  expect_SSAMod(modelSpF, "mFix")
  expect_equal(modelSpF[["E1"]]$mFix, modelSp[["E1"]]$mFix)
  modelSpR <- fitTD(testTD, design = "res.ibd", traits = "t1", what = "random")
  expect_SSA(modelSpR)
  expect_SSAMod(modelSpR, "mRand")
  expect_equal(modelSp[["E1"]]$mRand, modelSpR[["E1"]]$mRand)
  expect_null(modelSpR[["E1"]]$mFix)
})

test_that("running models for multiple traits produces correct output structure", {
  modelSp2 <- fitTD(testTD, design = "rowcol", traits = paste0("t", 1:2))
  modelSp3 <- fitTD(testTD, design = "rcbd", traits = paste0("t", 1:3))
  modelSp4 <- fitTD(testTD, design = "rowcol", traits = paste0("t", 1:4))
  expect_SSA(modelSp2)
  expect_SSA(modelSp3)
  expect_SSA(modelSp4)
  expect_SSAMod(modelSp2, "mRand")
  expect_SSAMod(modelSp3, "mRand")
  expect_SSAMod(modelSp4, "mRand")
  expect_SSAMod(modelSp2, "mFix")
  expect_SSAMod(modelSp3, "mFix")
  expect_SSAMod(modelSp4, "mFix")
})

test_that("running models for multiple traits doesn't change trait results", {
  modelSp1 <- fitTD(testTD, design = "rowcol", traits = "t1")
  modelSp2 <- fitTD(testTD, design = "rowcol", traits = paste0("t", 1:2))
  modelSp3 <- fitTD(testTD, design = "rowcol", traits = paste0("t", 1:3))
  modelSp4 <- fitTD(testTD, design = "rowcol", traits = paste0("t", 1:4))
  expect_equal(modelSp1[["E1"]]$mRand, modelSp2[["E1"]]$mRand["t1"])
  expect_equal(modelSp2[["E1"]]$mRand, modelSp3[["E1"]]$mRand[paste0("t", 1:2)])
  expect_equal(modelSp3[["E1"]]$mRand, modelSp4[["E1"]]$mRand[paste0("t", 1:3)])
  expect_equal(modelSp1[["E1"]]$mFix, modelSp2[["E1"]]$mFix["t1"])
  expect_equal(modelSp2[["E1"]]$mFix, modelSp3[["E1"]]$mFix[paste0("t", 1:2)])
  expect_equal(modelSp3[["E1"]]$mFix, modelSp4[["E1"]]$mFix[paste0("t", 1:3)])
})

test_that("option covariates produces expected output structure", {
  modelSpCov <- fitTD(testTD, design = "rowcol", traits = "t1",
                      covariates = "repId")
  expect_SSA(modelSpCov)
  expect_SSAMod(modelSpCov, "mRand")
  expect_SSAMod(modelSpCov, "mFix")
  expect_true(grepl(pattern = "repId",
                    x = deparse(modelSpCov[["E1"]]$mRand$t1$model$fixed)))
  expect_true(grepl(pattern = "repId",
                    x = deparse(modelSpCov[["E1"]]$mFix$t1$model$fixed)))
})

test_that("option useCheckId produces expected output structure", {
  modelSpCi <- fitTD(testTD, design = "rowcol", traits = "t1",
                     useCheckId = TRUE)
  expect_SSA(modelSpCi)
  expect_SSAMod(modelSpCi, "mRand")
  expect_SSAMod(modelSpCi, "mFix")
  expect_true(grepl(pattern = "checkId",
                    x = deparse(modelSpCi[["E1"]]$mRand$t1$model$fixed)))
  expect_true(grepl(pattern = "checkId",
                    x = deparse(modelSpCi[["E1"]]$mFix$t1$model$fixed)))
})

test_that("option spatial produces expected output structure", {
  modelSp <- fitTD(testTD, design = "rowcol", traits = "t1")
  modelSpTs <- fitTD(testTD, design = "rowcol", traits = "t1", spatial = TRUE)
  expect_SSA(modelSpTs)
  expect_SSAMod(modelSpTs, "mRand")
  expect_SSAMod(modelSpTs, "mFix")
  ## SpATS should use spatial as default. Timestamp will be different.
  expect_equivalent(modelSp, modelSpTs)
})

test_that("option nSeg in control produces correct output", {
  ## Test using equivalence because of timestamp.
  modelSp <- fitTD(testTD, design = "rowcol", traits = "t1",
                   control = list(nSeg = 1))
  modelSp1 <- fitTD(testTD, design = "rowcol", traits = "t1",
                    control = list(nSeg = c(1, 1)))
  expect_equivalent(modelSp, modelSp1)
  expect_error(fitTD(testTD, design = "rowcol", traits = "t1",
                     control = list(nSeg = list(c(1, 1)))),
               "should be a named item in list of nSeg")
  expect_warning(fitTD(testTD, design = "rowcol", traits = "t1",
                     control = list(nSeg = c(0, 1))),
                 "Invalid value for control parameter nSeg")
  modelSp2 <- fitTD(testTD, design = "rowcol", traits = "t1",
                    control = list(nSeg = list(E1 = c(1, 1))))
  expect_equivalent(modelSp, modelSp2)
  modelSp3 <- fitTD(testTD, design = "rowcol", traits = "t1",
                    control = list(nSeg = list(E3 = c(1, 1), E1 = c(1, 1))))
  expect_equivalent(modelSp, modelSp3)
})

test_that("option nestDiv in control produces correct output", {
  ## Test using equivalence because of timestamp.
  modelSp <- fitTD(testTD, design = "rowcol", traits = "t1",
                   control = list(nestDiv = 3))
  modelSp1 <- fitTD(testTD, design = "rowcol", traits = "t1",
                    control = list(nestDiv = c(3, 3)))
  expect_equivalent(modelSp, modelSp1)
  expect_warning(fitTD(testTD, design = "rowcol", traits = "t1",
                       control = list(nestDiv = 0)),
                 "Invalid value for control parameter nestDiv")
})

test_that("option progress functions properly", {
  expect_output(fitTD(testTD, design = "rowcol", traits = "t1",
                      progress = TRUE),
                "Fitting models for t1 in E1")
})

test_that("Trial with missing data is handled properly when fitting models", {
  ## Set all observations to NA for 1 trait to create data that causes the
  ## model engines to crash.
  ## fitTD should be able to handle this and still produce output for
  ## the other models.
  testTD[["E1"]][["t2"]] <- NA
  expect_warning(modelSp <- fitTD(testTD, design = "rowcol",
                                  traits = c("t1", "t2")),
                 "Error in SpATS")
  expect_SSA(modelSp)
})

test_that("Design is modified when replicates contain only 1 distinct value", {
  ## Set replicates to 1 for 1 field to test that design is changed to
  ## corresponding design without replicates.
  testTD[["E1"]][["repId"]] <- 1
  expect_warning(modelSp <- fitTD(testTD, design = "res.rowcol", traits = "t1"),
                 "Design changed")
  expect_equal(modelSp$E1$design, "rowcol")
})

test_that("Model checks function properly", {
  expect_error(fitTD(), "TD should be a valid object of class TD")
  expect_error(fitTD(testTD, trial = "E2"), "trial should be in TD")
  expect_error(fitTD(testTD, trial = "E1", design = "myDes"),
               "design should either be an attribute of TD or one of")
  expect_error(fitTD(testTD, trial = "E1", traits = 1, design = "rowcol"),
               "traits should be NULL or a character vector")
  expect_error(fitTD(testTD, trial = "E1", traits = "t5", design = "rowcol"),
               "All traits should be columns in")
  expect_error(fitTD(testTD, trial = "E1", traits = "t1", design = "rowcol",
                     covariates = 1),
               "covariates should be NULL or a character vector")
  expect_error(fitTD(testTD, trial = "E1", traits = "t1", design = "rowcol",
                     covariates = "myCovar"),
               "All covariates should be columns in E1")
  expect_error(fitTD(testTD, trial = "E1", traits = "t1", design = "rowcol",
                     spatial = 1),
               "spatial should be a single logical value")
  expect_error(fitTD(testTD, trial = "E1", traits = "t1", design = "rowcol",
                     engine = "myEng"),
               "engine should be one of SpATS, lme4, asreml")
  expect_error(fitTD(testTD, trial = "E1", traits = "t1", design = "rowcol",
                     control = 1),
               "control has to be NULL or a list")
  testTD$E1$colId <- NULL
  testTD$E1$colCoord <- NULL
  expect_error(fitTD(testTD, trial = "E1", traits = "t1", design = "rowcol"),
               "colId should be a column in E1")
})

test_that("Fitting models functions properly when trait contains space", {
  ## Create a trait with a space in its name.
  ## fitTD should be able to handle this.
  testTD[["E1"]][["t 2"]] <- testTD[["E1"]][["t2"]]
  modelSp <- fitTD(testTD, design = "rowcol", engine = "SpATS",
                   traits = c("t1", "t 2"))
  expect_SSA(modelSp)
})
