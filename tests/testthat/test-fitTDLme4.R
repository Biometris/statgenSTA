context("lme4 modeling")

## Helper function for testing base structure that has to be consistent
## for all STA objects independent of engine and options.
expect_STA <- function(STA) {
  test_that(paste(deparse(substitute(STA)), "has correct STA structure"), {
    expect_is(STA, "STA")
    for (tr in names(STA)) {
      expect_length(STA[[tr]], 10)
      expect_named(STA[[tr]], c("mRand", "mFix", "TD", "traits", "design",
                                "spatial", "engine", "predicted", "sumTab",
                                "useCheckId"))
      expect_is(STA[[tr]]$TD, "TD")
    }
  })
}

## Helper function for testing base structure for fitted models within an
## STA object. Param class is used to indicate the output type of the fitted
## model. Normally this is identical to the engine in STA but for lme4 this
## can vary depending on the fitted model.
expect_STAMod <- function(STA,
                          what,
                          class = NULL) {
  if (is.null(class)) {
    class <- STA[[1]]$engine
  }
  for (tr in names(STA)) {
    STAMod <- STA[[tr]][[what]]
    test_that(paste(deparse(substitute(what)), "in", deparse(substitute(STA)),
                    "has correct structure"), {
                      expect_is(STAMod, "list")
                      expect_length(STAMod, length(STA[[tr]]$traits))
                      expect_named(STAMod, STA[[tr]]$traits)
                      for (trait in STA[[tr]]$traits) {
                        expect_is(STAMod[[trait]], class)
                      }
                    })
  }
}

test_that("running models creates objects with correct structure", {
  modelLm <- fitTD(testTD, design = "rcbd", traits = "t1", engine = "lme4")
  expect_STA(modelLm)
  expect_STAMod(modelLm, "mRand", class = "lmerMod")
  expect_STAMod(modelLm, "mFix", class = "lm")
  expect_equal(modelLm[["E1"]]$traits, "t1")
  expect_equal(modelLm[["E1"]]$design, "rcbd")
  expect_false(modelLm[["E1"]]$spatial)
  expect_equal(modelLm[["E1"]]$engine, "lme4")
})

test_that("option what produces expected output", {
  modelLm <- fitTD(testTD, design = "rcbd", traits = "t1", engine = "lme4")
  modelLmF <- fitTD(testTD, design = "rcbd", traits = "t1", what = "fixed",
                    engine = "lme4")
  expect_STA(modelLmF)
  expect_null(modelLmF[["E1"]]$mRand)
  expect_STAMod(modelLmF, "mFix", "lm")
  expect_equal(modelLmF[["E1"]]$mFix, modelLm[["E1"]]$mFix)
  modelLmR <- fitTD(testTD, design = "rcbd", traits = "t1", what = "random",
                    engine = "lme4")
  expect_STA(modelLmR)
  expect_STAMod(modelLmR, "mRand", "lmerMod")
  expect_equal(modelLm[["E1"]]$mRand, modelLmR[["E1"]]$mRand)
  expect_null(modelLmR[["E1"]]$mFix)
})

test_that("option covariates produces expected output structure", {
  modelLmCov <- fitTD(testTD, design = "rcbd", traits = "t1",
                      covariates = "repId", engine = "lme4")
  expect_STA(modelLmCov)
  expect_STAMod(modelLmCov, "mRand", "lmerMod")
  expect_STAMod(modelLmCov, "mFix", "lm")
  expect_true("repId" %in% colnames(modelLmCov[["E1"]]$mRand$t1@frame))
  expect_true("repId" %in% colnames(modelLmCov[["E1"]]$mFix$t1$model))
})

test_that("option useCheckId produces expected output structure", {
  expect_error(fitTD(testTD, design = "rcbd", traits = "t1", useCheckId = TRUE,
                     engine = "lme4"),
               "genotype as fixed effect and useCheckId = TRUE is not possible")
  modelLmCi <- fitTD(testTD, design = "rcbd", traits = "t1", useCheckId = TRUE,
                     engine = "lme4", what = "random")
  expect_STA(modelLmCi)
  expect_STAMod(modelLmCi, "mRand", "lmerMod")
  expect_true("checkId" %in% colnames(modelLmCi[["E1"]]$mRand$t1@frame))
})

test_that("option spatial produces expected output structure", {
  expect_warning(fitTD(testTD, design = "rowcol", traits = "t1",
                       spatial = TRUE, engine = "lme4"),
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
  expect_STA(modelLm)
})

test_that("Fitting models functions properly when trait contains space", {
  ## Create a trait with a space in its name.
  ## fitTD should be able to handle this.
  testTD[["E1"]][["t 2"]] <- testTD[["E1"]][["t2"]]
  expect_message(modelLm <- fitTD(testTD, design = "rcbd", engine = "lme4",
                                  traits = c("t1", "t 2")))
  expect_STA(modelLm)
})

