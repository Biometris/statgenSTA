context("Modeling")

if (requireNamespace("asreml", quietly = TRUE)) {
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
    modelAs <- fitTD(testTD, design = "rcbd", traits = "t1", engine = "asreml")
    expect_SSA(modelAs)
    expect_SSAMod(modelAs, "mRand")
    expect_SSAMod(modelAs, "mFix")
    expect_equal(modelAs[["E1"]]$traits, "t1")
    expect_equal(modelAs[["E1"]]$design, "rcbd")
    expect_false(modelAs[["E1"]]$spatial[["t1"]])
    expect_equal(modelAs[["E1"]]$engine, "asreml")
  })

  test_that("option what produces expected output", {
    modelAsF <- fitTD(testTD, design = "rowcol", traits = "t1", what = "fixed",
                      engine = "asreml")
    expect_SSA(modelAsF)
    expect_null(modelAsF[["E1"]]$mRand)
    expect_SSAMod(modelAsF, "mFix")
    modelAsR <- fitTD(testTD, design = "rowcol", traits = "t1", what = "random",
                      engine = "asreml")
    expect_SSA(modelAsR)
    expect_SSAMod(modelAsR, "mRand")
    expect_null(modelAsR[["E1"]]$mFix)
  })

  ## Covariates.

  test_that("option covariates produces expected output structure", {
    modelAsCov <- fitTD(testTD, design = "rowcol", traits = "t1",
                        covariates = "repId", engine = "asreml")
    expect_SSA(modelAsCov)
    expect_SSAMod(modelAsCov, "mRand")
    expect_SSAMod(modelAsCov, "mFix")
    if (asreml4()) {
      expect_true(grepl(pattern = "repId",
                        x = deparse(modelAsCov[["E1"]]$mRand$t1$formulae$fixed)))
      expect_true(grepl(pattern = "repId",
                        x = deparse(modelAsCov[["E1"]]$mFix$t1$formulae$fixed)))
    } else {
      expect_true(grepl(pattern = "repId",
                        x = deparse(modelAsCov[["E1"]]$mRand$t1$fixed.formula)))
      expect_true(grepl(pattern = "repId",
                        x = deparse(modelAsCov[["E1"]]$mFix$t1$fixed.formula)))
    }
  })

  ### spatial.

  test_that("option spatial produces expected output structure", {
    modelAsTs <- fitTD(testTD, design = "ibd", traits = "t1",
                       spatial = TRUE, engine = "asreml")
    expect_SSA(modelAsTs)
    expect_SSAMod(modelAsTs, "mRand")
    expect_SSAMod(modelAsTs, "mFix")
    expect_is(modelAsTs[["E1"]]$spatial, "list")
    expect_length(modelAsTs[["E1"]]$spatial, 1)
    expect_named(modelAsTs[["E1"]]$spatial, "t1")
    expect_equal(modelAsTs[["E1"]]$spatial$t1, "none")
  })

  test_that("option spatial functions properly with missing data", {
    testTD$E1 <- testTD$E1[-1, ]
    expect_silent(modelAsTs <- fitTD(testTD, design = "res.ibd", traits = "t1",
                                     spatial = TRUE, engine = "asreml"))
  })

  test_that("option spatial functions properly with extra data", {
    testTD$E1 <- rbind(testTD$E1, testTD$E1[1, ])
    expect_warning(modelAsTs <- fitTD(testTD, design = "res.ibd", traits = "t1",
                                      spatial = TRUE, engine = "asreml"),
                   "There should only be one plot at each combination of")
  })

  test_that("option criterion functions properly for asreml spatial", {
    expect_warning(fitTD(testTD, design = "ibd", traits = "t1",
                         spatial = TRUE, engine = "asreml",
                         control = list(criterion = 1)),
                   "Invalid value for control parameter criterion")
    modelAsTs <- fitTD(testTD, design = "ibd", traits = "t1",
                       spatial = TRUE, engine = "asreml",
                       control = list(criterion = "BIC"))
    expect_equal(modelAsTs[["E1"]]$spatial$t1, "none")
  })

  ### Missing data.

  test_that("Trial with missing data is handled properly when fitting models", {
    ## Set all observations to NA for 1 trait to create data that causes the
    ## model engines to crash.
    ## fitTD should be able to handle this and still produce output for
    ## the other models.
    testTD[["E1"]][["t2"]] <- NA
    expect_warning(modelAs <- fitTD(testTD, design = "rowcol",
                                    traits = c("t1", "t2"), engine = "asreml"),
                   "Error in asreml")
    expect_SSA(modelAs)
    expect_warning(modelAs2 <- fitTD(testTD, design = "rowcol",
                                     traits = c("t1", "t2"), engine = "asreml",
                                     spatial = TRUE),
                   "Error in asreml")
    expect_SSA(modelAs2)
  })

  test_that("Fitting models functions properly when trait contains space", {
    ## Create a trait with a space in its name.
    ## fitTD should be able to handle this.
    testTD[["E1"]][["t 2"]] <- testTD[["E1"]][["t2"]]
    expect_error(modelAs <- fitTD(testTD, design = "rowcol", engine = "asreml",
                                  traits = c("t1", "t 2")),
                 "asreml cannot fit models when trait contains white space")
  })
}
