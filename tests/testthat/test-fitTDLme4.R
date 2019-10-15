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
  modelLm <- fitTD(testTD, design = "rcbd", traits = "t1", engine = "lme4")
  expect_SSA(modelLm)
  expect_SSAMod(modelLm, "mRand", class = "lmerMod")
  expect_SSAMod(modelLm, "mFix", class = "lm")
  expect_equal(modelLm[["E1"]]$traits, "t1")
  expect_equal(modelLm[["E1"]]$design, "rcbd")
  expect_false(modelLm[["E1"]]$spatial)
  expect_equal(modelLm[["E1"]]$engine, "lme4")
})

test_that("option what produces expected output", {
  modelLm <- fitTD(testTD, design = "rcbd", traits = "t1", engine = "lme4")
  modelLmF <- fitTD(testTD, design = "rcbd", traits = "t1", what = "fixed",
                    engine = "lme4")
  expect_SSA(modelLmF)
  expect_null(modelLmF[["E1"]]$mRand)
  expect_SSAMod(modelLmF, "mFix", "lm")
  expect_equal(modelLmF[["E1"]]$mFix, modelLm[["E1"]]$mFix)
  modelLmR <- fitTD(testTD, design = "rcbd", traits = "t1", what = "random",
                    engine = "lme4")
  expect_SSA(modelLmR)
  expect_SSAMod(modelLmR, "mRand", "lmerMod")
  expect_equal(modelLm[["E1"]]$mRand, modelLmR[["E1"]]$mRand)
  expect_null(modelLmR[["E1"]]$mFix)
})

test_that("option covariates produces expected output structure", {
  modelLmCov <- fitTD(testTD, design = "rcbd", traits = "t1",
                      covariates = "repId", engine = "lme4")
  expect_SSA(modelLmCov)
  expect_SSAMod(modelLmCov, "mRand", "lmerMod")
  expect_SSAMod(modelLmCov, "mFix", "lm")
  expect_true("repId" %in% colnames(modelLmCov[["E1"]]$mRand$t1@frame))
  expect_true("repId" %in% colnames(modelLmCov[["E1"]]$mFix$t1$model))
})

test_that("option useCheckId produces expected output structure", {
  modelLmCi <- fitTD(testTD, design = "rcbd", traits = "t1", useCheckId = TRUE,
                     engine = "lme4")
  expect_SSA(modelLmCi)
  expect_SSAMod(modelLmCi, "mRand", "lmerMod")
  expect_SSAMod(modelLmCi, "mFix", "lm")
  expect_true("checkId" %in% colnames(modelLmCi[["E1"]]$mRand$t1@frame))
  expect_true("checkId" %in% colnames(modelLmCi[["E1"]]$mFix$t1$model))
})

test_that("option trySpatial produces expected output structure", {
  expect_warning(fitTD(testTD, design = "rowcol", traits = "t1",
                       trySpatial = TRUE, engine = "lme4"),
                 "Spatial models can only be fitted using SpATS or asreml.")
})

test_that("Trial with missing data is handled properly when fitting models", {
  ## Set all observations to NA for 1 trait to create data that causes the
  ## model engines to crash.
  ## fitTD should be able to handle this and still produce output for
  ## the other models.
  testTD[["E1"]][["t2"]] <- NA
  expect_warning(modelLm <- fitTD(testTD, design = "rowcol",
                                  traits = c("t1", "t2"), engine = "lme4"),
                 "Error in lmer")
  expect_SSA(modelLm)
})

test_that("Fitting models functions properly when trait contains space", {
  ## Create a trait with a space in its name.
  ## fitTD should be able to handle this.
  testTD[["E1"]][["t 2"]] <- testTD[["E1"]][["t2"]]
  expect_silent(modelLm <- fitTD(testTD, design = "rcbd", engine = "lme4",
                                 traits = c("t1", "t 2")))
  expect_SSA(modelLm)
})

