#' Extract statistics from fitted models
#'
#' @description
#' Extract and calculate various results for fitted models such as BLUEs, BLUPs,
#' unit errors and heritabilities. For a full list of results that can be
#' extracted, see the table below.\cr\cr
#' The result(s) to extract can be specified in \code{what}.\cr
#' If a single result is extracted, this result is returned as a
#' \code{data.frame}, If this is not possible, because the format of the result
#' is incompatible with the \code{data.frame} format, the result is returned as
#' a list. E.g. if BLUEs are extracted, the output of the function is a
#' \code{data.frame} with BLUEs. However if varCompF is extracted, the
#' output of the function is a \code{list}.\cr\cr
#' Results that are returned as \code{data.frame} are marked as such in the
#' asDataFrame column in the table. If the default return value for a result
#' is a \code{data.frame} that can be overridden by the user by specifying
#' \code{asDataFrame = FALSE}. When doing so the result will be returned as a
#' list of data.frames, one per trial. They other way round is not possible.
#' If a result is returned as a \code{list} according to the table, it cannot
#' be returned as a \code{data.frame}.\cr
#' If multiple results are extracted at the same time, these are always
#' returned as a list.
#' \cr\cr
#' Most results can only be calculated if a model is fitted with genotype as
#' fixed or with genotype as random. E.g. to compute heritabilities a model
#' should be fitted with genotype as random effect. This is indicated in the
#' table in the column model with "F" and "R" respectively.
#'
#' Possible options for \code{what} are:
#'
## Code below is not working at the moment -
## See https://github.com/r-lib/roxygen2/issues/1171
## After the code the output is copied, this looks okay, but is less flexible
# ```{r extractOpts, results="as.is", echo=FALSE, eval=FALSE}
# ## Generate table of options for extract from internal data.
# optsTab <- statgenSTA:::extractOptions[, c("result", "model", "description")]
# optsTab[["data.frame"]] <-
# ifelse(statgenSTA:::extractOptions[["asDataFrame"]] == 0, "", "yes")
# optsTab <- optsTab[order(optsTab[["model"]]), ]
# knitr::kable(optsTab, align = c("llll"), row.names = FALSE)
# ```
#' |result       |model |description                                                                 |asDataFrame |
#'|:------------|:-----|:---------------------------------------------------------------------------|:-----------|
#'  |BLUEs        |F     |Best Linear Unbiased Estimators                                             |yes         |
#'  |seBLUEs      |F     |standard errors of the BLUEs                                                |yes         |
#'  |ue           |F     |unit errors - only for lme4 and asreml                                      |yes         |
#'  |varCompF     |F     |variance components for the model with genotype as fixed component          |            |
#'  |fitted       |F     |fitted values for the model with genotype as fixed component                |yes         |
#'  |residF       |F     |residuals for the model with genotype as fixed component                    |yes         |
#'  |stdResF      |F     |standardized residuals for the model with genotype as fixed component       |yes         |
#'  |wald         |F     |results of the wald test - only for lme4 and asreml                         |            |
#'  |CV           |R     |Coefficient of Variation                                                    |yes         |
#'  |rDfF         |F     |residual degrees of freedom for the model with genotype as fixed component  |yes         |
#'  |sed          |F     |standard error of difference - only for asreml                              |            |
#'  |lsd          |F     |least significant difference - only for asreml                              |            |
#'  |BLUPs        |R     |Best Linear Unbiased Predictors                                             |yes         |
#'  |seBLUPs      |R     |standard errors of the BLUPs                                                |yes         |
#'  |heritability |R     |broad sense heritability                                                    |yes         |
#'  |varCompR     |R     |variance components for the model with genotype as random component         |            |
#'  |varGen       |R     |genetic variance component                                                  |yes         |
#'  |varErr       |R     |residual variance component                                                 |yes         |
#'  |varSpat      |R     |spatial variance components - only for SpATS                                |            |
#'  |rMeans       |R     |fitted values for the model with genotype as random component               |yes         |
#'  |ranEf        |R     |random genetic effects                                                      |yes         |
#'  |residR       |R     |residuals for the model with genotype as random component                   |yes         |
#'  |stdResR      |R     |standardized residuals for the model with genotype as random component      |yes         |
#'  |rDfR         |R     |residual degrees of freedom for the model with genotype as random component |yes         |
#'  |effDim       |R     |effective dimensions - only for SpATS                                       |            |
#'  |ratEffDim    |R     |ratios of the effective dimensions - only for SpATS                         |            |
#'
#' @param STA An object of class STA.
#' @param trials A character vector of trials for which the statistics should be
#' computed. If not supplied, statistics are computed for all trials that have
#' been modeled.
#' @param traits A character vector of traits for which the statistics should be
#' computed. If not supplied, statistics are computed for all traits that have
#' been modeled.
#' @param what A character vector indicating which statistics should be
#' computed. Most statistics are available for all models, some only for models
#' fitted using a certain engine. If this is the case, this is indicated in the
#' list with options in details.\cr
#' If \code{what = "all"}, all available statistics are computed.
#' @param asDataFrame Should the output be reshaped to a data.frame. This is
#' only possible if the number of statistics to extract is one.
#' @param keep A character vector of column(s) in the object of class
#' \code{\link{TD}} used for modeling. These columns will be kept as output when
#' computing fitted values, residuals, standardized residuals and rMeans.
#' Columns can also be kept when computing (se)BLUEs and (se)BLUPs but only if
#' the column to keep contains unique values for the modeled variables, i.e. a
#' column repId with several different values per genotype cannot be kept.
#' @param restoreColNames Should the original column names be restored in the
#' output of the extracted data?
#'
#' @return Depending on the input either a data.frame or a list with, per
#' trial for which statistics have been extracted, a list of those statistics.
#'
#' @seealso \code{\link{fitTD}}
#'
#' @examples
#' ## Fit model using SpATS.
#' modSp <- fitTD(TD = TDHeat05,
#'                design = "res.rowcol",
#'                traits = "yield")
#'
#' ## Extract all available statistics from the fitted model.
#' extr <- extractSTA(modSp)
#'
#' ## Extract only the BLUEs from the fitted model.
#' BLUEs <- extractSTA(modSp,
#'                     what = "BLUEs")
#'
#' ## Extract only the BLUEs from the fitted model and keep trial as variable in
#' ## the output.
#' BLUEs2 <- extractSTA(modSp,
#'                      what = "BLUEs",
#'                      keep = "trial")
#'
#' @importFrom utils capture.output
#' @export
extractSTA <- function(STA,
                       trials = names(STA),
                       traits = NULL,
                       what = "all",
                       asDataFrame = length(what) == 1 && what != "all",
                       keep = NULL,
                       restoreColNames = FALSE) {
  ## Checks.
  if (missing(STA) || !inherits(STA, "STA")) {
    stop("STA has to be an object of class STA.\n")
  }
  trials <- chkTrials(trials, STA)
  chkChar(traits)
  chkChar(keep)
  if (asDataFrame) {
    if (length(what) > 1) {
      stop("Converting output to data.frame is only possible if what has ",
           "lenght 1.\n")
    }
    ## Trial is added to keep columns, but only if a trial column is
    ## actually present in the data.
    keep <- unique(c(if (length(STA) != 1 ||
                         hasName(STA[[1]]$TD[[1]], "trial")) "trial",
                     keep))
  }
  resTot <- sapply(X = trials, FUN = function(trial) {
    STATr <- STA[[trial]]
    traitsTr <- chkTraits(traits, trial, STATr)
    if (!all(keep %in% colnames(STATr$TD[[trial]]))) {
      stop("All keep should be columns in ", trial, ".\n")
    }
    traitsTr <- traitsTr[sapply(X = traitsTr, FUN = function(trait) {
      !is.null(STATr$mRand[[trait]]) || !is.null(STATr$mFix[[trait]])})]
    if (length(traitsTr) == 0) {
      return(NULL)
    }
    engine <- STATr$engine
    ## Set useRepId to TRUE when it is used as fixed effect in the model.
    useRepId <- STATr$design %in% c("res.ibd", "res.rowcol", "rcbd")
    ## Extract statistics from fitted model.
    result <- do.call(what = paste0("extractSTA", tools::toTitleCase(engine)),
                      args = list(STA = STATr, traits = traitsTr, what = what,
                                  useRepId = useRepId, keep = keep,
                                  restore = restoreColNames))
    attr(x = result, which = "traits") <- traitsTr
    attr(x = result, which = "design") <- STATr$design
    attr(x = result, which = "engine") <- engine
    return(result)
  }, simplify = FALSE)

  if (asDataFrame) {
    ## Get type of conversion that needs to be done.
    ## 1 for simple row binding, 2 for conversion of nested list.
    convertType <- extractOptions[extractOptions[["result"]] == what,
                                  "asDataFrame"]
    if (convertType == 0) {
      warning("Conversion to data.frame not possible for ", what, ".\n",
              "Returning the unconverted results.\n")
      return(resTot)
    } else if (convertType == 1) {
      ## Bind data.frames in resTot together to one large data.frame.
      resDf <- dfBind(unlist(resTot, recursive = FALSE))
    } else if (convertType == 2) {
      ## Extract nested results.
      resNested <- sapply(X = resTot, FUN = `[`, what)
      ## Get traits from result.
      traitsRes <- unique(unlist(sapply(X = resNested, names)))
      ## Create a base data.frame for the output.
      resDf <- data.frame(trial = names(resTot), stringsAsFactors = FALSE)
      for (trait in traitsRes) {
        ## Fill values for current trait.
        ## This respects missing values.
        resDf[[trait]] <- sapply(X = resNested, FUN = `[`, trait)
      }
    }
    return(resDf)
  } else {
    return(resTot)
  }
}

#' Extract statistics from model fitted using SpATS
#'
#' @noRd
#' @importFrom SpATS predict.SpATS
#' @keywords internal
extractSTASpATS <- function(STA,
                            traits = NULL,
                            what = "all",
                            keep = NULL,
                            useRepId,
                            restore = FALSE) {
  mf <- STA$mFix[names(STA$mFix) %in% traits]
  mr <- STA$mRand[names(STA$mRand) %in% traits]
  TD <- STA$TD[[1]]
  renCols <- attr(TD, "renamedCols")
  predicted <- STA$predicted
  useCheckId <- !is.null(mr) &&
    "checkId" %in% attr(terms(mr[[1]]$model$fixed), "term.labels")
  whatPred <- c("BLUEs", "seBLUEs", "BLUPs", "seBLUPs", "ranEf")
  what <- extractOptSel(what = what, fixed = !is.null(mf),
                        random = !is.null(mr), engine = "SpATS")
  ## Fitted values and residuals are not set to NA by SpATS in case the original
  ## data is NA. Get missing values per trait to set them to NA later.
  naTr <- sapply(X = traits, FUN = function(trait) {
    which(is.na(TD[[trait]]))
  }, simplify = FALSE)
  ## Create baseData and baseDataPred to which further results will be merged.
  base <- createBaseData(TD, predicted, keep, useRepId, useCheckId,
                         bdPred = any(what %in% whatPred))
  baseData <- base$baseData
  baseDataPred <- base$baseDataPred
  ## Create empty result list.
  result <- setNames(vector(mode = "list", length = length(what)),
                     what)
  ## Compute BLUEs and se of BLUEs from fixed model.
  if ("BLUEs" %in% what) {
    predVals <- lapply(X = traits, FUN = function(trait) {
      predVal <- predict(mf[[trait]], which = predicted,
                         predFixed = "marginal")[c(predicted, "predicted.values")]
      colnames(predVal) <- c(predicted, trait)
      return(predVal)
    })
    BLUEs <- Reduce(f = function(x, y) merge(x, y, all = TRUE),
                    x = predVals, init = baseDataPred)
    result[["BLUEs"]] <- restoreColNames(renDat = BLUEs, renamedCols = renCols,
                                         restore = restore)
  }
  if ("seBLUEs" %in% what) {
    predErrs <- lapply(X = traits, FUN = function(trait) {
      predErr <- predict(mf[[trait]], which = predicted,
                         predFixed = "marginal")[c(predicted, "standard.errors")]
      colnames(predErr) <- c(predicted, trait)
      return(predErr)
    })
    seBLUEs <- Reduce(f = function(x, y) merge(x, y, all = TRUE),
                      x = predErrs, init = baseDataPred)
    result[["seBLUEs"]] <- restoreColNames(renDat = seBLUEs,
                                           renamedCols = renCols,
                                           restore = restore)
  }
  ## Compute BLUPs and se of BLUPs from mixed model.
  if ("BLUPs" %in% what) {
    whichPred <- c(predicted, if (useCheckId) "checkId")
    predVals <- lapply(X = traits, FUN = function(trait) {
      predVal <- predict(mr[[trait]], which = whichPred,
                         predFixed = "marginal")[c(whichPred, "predicted.values")]
      colnames(predVal) <- c(whichPred, trait)
      return(predVal)
    })
    BLUPs <- Reduce(f = function(x, y) merge(x, y, all = TRUE),
                    x = predVals, init = baseDataPred)
    result[["BLUPs"]] <- restoreColNames(renDat = BLUPs, renamedCols = renCols,
                                         restore = restore)
  }
  if ("seBLUPs" %in% what) {
    whichPred <- c(predicted, if (useCheckId) "checkId")
    predErrs <- lapply(X = traits, FUN = function(trait) {
      predErr <- predict(mr[[trait]], which = whichPred,
                         predFixed = "marginal")[c(whichPred, "standard.errors")]
      colnames(predErr) <- c(whichPred, trait)
      return(predErr)
    })
    seBLUPs <- Reduce(f = function(x, y) merge(x, y, all = TRUE),
                      x = predErrs, init = baseDataPred)
    result[["seBLUPs"]] <- restoreColNames(renDat = seBLUPs,
                                           renamedCols = renCols,
                                           restore = restore)
  }
  ## Compute generalized heritability.
  if ("heritability" %in% what) {
    result[["heritability"]] <- sapply(X = mr, FUN = function(mr0) {
      unname(SpATS::getHeritability(mr0))
    })
  }
  ## Extract variance components for genotype fixed.
  if ("varCompF" %in% what) {
    result[["varCompF"]] <- lapply(X = mf, FUN = extractVarComp,
                                   engine = "SpATS")
  }
  ## Extract variance components for genotype random.
  if ("varCompR" %in% what) {
    result[["varCompR"]] <- lapply(X = mr, FUN = extractVarComp,
                                   engine = "SpATS")
  }
  ## Extract genetic variance.
  if ("varGen" %in% what) {
    result[["varGen"]] <- sapply(X = mr, FUN = function(mr0) {
      unname(mr0$var.comp[predicted])
    })
  }
  ## Extract residual variance.
  if ("varErr" %in% what) {
    result[["varErr"]] <- sapply(X = mr, FUN = function(mr0) {
      unname(mr0$psi[1])
    })
  }
  ## Extract spatial variance.
  if ("varSpat" %in% what) {
    result[["varSpat"]] <- sapply(X = mr, FUN = function(mr0) {
      mr0$var.comp[grep(pattern = "Coord", x = names(mr0$var.comp))]
    })
  }
  ## Extract fitted values.
  if ("fitted" %in% what) {
    fitVal <- cbind(baseData, sapply(X = traits, FUN = function(trait) {
      fitVals <- fitted(mf[[trait]])
      fitVals[naTr[[trait]]] <- NA
      fitVals
    }))
    result[["fitted"]] <- restoreColNames(renDat = fitVal, renamedCols = renCols,
                                          restore = restore)
  }
  ## Extract residuals.
  if ("residF" %in% what) {
    resVal <- cbind(baseData, sapply(X = traits, FUN = function(trait) {
      resVals <- residuals(mf[[trait]])
      resVals[naTr[[trait]]] <- NA
      resVals
    }))
    result[["residF"]] <- restoreColNames(renDat = resVal, renamedCols = renCols,
                                          restore = restore)
  }
  ## Extract standardized residuals.
  if ("stdResF" %in% what) {
    stdRes <- cbind(baseData, sapply(X = mf, FUN = function(mf0) {
      residuals(mf0) / sd(residuals(mf0), na.rm = TRUE)
    }))
    result[["stdResF"]] <- restoreColNames(renDat = stdRes, renamedCols = renCols,
                                           restore = restore)
  }
  ## Extract rMeans.
  if ("rMeans" %in% what) {
    rMeans <- cbind(baseData, sapply(X = traits, FUN = function(trait) {
      fitVals <- fitted(mr[[trait]])
      fitVals[naTr[[trait]]] <- NA
      fitVals
    }))
    result[["rMeans"]] <- restoreColNames(renDat = rMeans, renamedCols = renCols,
                                          restore = restore)
  }
  ## Extract random effects.
  if ("ranEf" %in% what) {
    ranEffs <- lapply(X = traits, FUN = function(trait) {
      effs <- mr[[trait]]$coeff[names(mr[[trait]]$coeff) %in%
                                  baseDataPred[[predicted]]]
      ranEff <- data.frame(names(effs), effs)
      colnames(ranEff) <- c(predicted, trait)
      return(ranEff)
    })
    ranEf <- Reduce(f = function(x, y) merge(x, y, all = TRUE),
                    x = ranEffs, init = baseDataPred)
    result[["ranEf"]] <- restoreColNames(renDat = ranEf, renamedCols = renCols,
                                         restore = restore)
  }
  ## Extract residuals for genotype random.
  if ("residR" %in% what) {
    resVal <- cbind(baseData, sapply(X = traits, FUN = function(trait) {
      resVals <- residuals(mr[[trait]])
      resVals[naTr[[trait]]] <- NA
      resVals
    }))
    result[["residR"]] <- restoreColNames(renDat = resVal,
                                          renamedCols = renCols,
                                          restore = restore)
  }
  ## Extract standardized residuals for genotype random.
  if ("stdResR" %in% what) {
    stdRes <- cbind(baseData, sapply(X = mr, FUN = function(mr0) {
      residuals(mr0) / sd(residuals(mr0), na.rm = TRUE)
    }))
    result[["stdResR"]] <- restoreColNames(renDat = stdRes,
                                           renamedCols = renCols,
                                           restore = restore)
  }
  if ("CV" %in% what) {
    ## Compute Coefficient of Variation.
    result[["CV"]] <- sapply(X = mf, FUN = function(mf0) {
      100 * sqrt(mf0$psi[1]) / mean(fitted(mf0), na.rm = TRUE)
    })
  }
  ## Extract residual degrees of freedom.
  if ("rDfF" %in% what) {
    result[["rDfF"]] <- sapply(X = mf, FUN = function(mf0) {
      round(mf0[["nobs"]] - sum(mf0[["eff.dim"]]))
    })
  }
  ## Extract residual degrees of freedom.
  if ("rDfR" %in% what) {
    result[["rDfR"]] <- sapply(X = mr, FUN = function(mr0) {
      round(mr0[["nobs"]] - sum(mr0[["eff.dim"]]))
    })
  }
  ## Extract effective dimensions.
  if ("effDim" %in% what) {
    result[["effDim"]] <- lapply(X = mr, FUN = function(mr0) {
      ## Get row order for effective dimensions in SpATS summary output.
      capture.output(effDimOrd <- rownames(summary(mr0)$p.table.dim))
      effDimOrd <- effDimOrd[1:(length(effDimOrd) - 4)]
      ## Extract directly from model since summary rounds the numbers.
      effDim <- data.frame(effDim = mr0[["eff.dim"]])
      ## order rows as in SpATS summary output.
      effDim <- effDim[effDimOrd, , drop = FALSE]
      ## Rename rows for more user readable output.
      renameRows(effDim)
    })
  }
  ## Extract ratio's of effective dimensions.
  if ("ratEffDim" %in% what) {
    result[["ratEffDim"]] <- lapply(X = mr, FUN = function(mr0) {
      capture.output(ratTot <- summary(mr0)$p.table.dim[, "Ratio"])
      ratTot <- setNames(as.numeric(ratTot[1:(length(ratTot) - 4)]),
                         names(ratTot[1:(length(ratTot) - 4)]))
      ratTot <- data.frame(ratEffDim = ratTot)
      ## Rename rows for more user readable output.
      renameRows(ratTot)
    })
  }
  return(result)
}

#' Extract statistics from model fitted using lme4
#'
#' @noRd
#' @keywords internal
extractSTALme4 <- function(STA,
                           traits = NULL,
                           what = "all",
                           keep = NULL,
                           useRepId,
                           restore = FALSE) {
  mf <- STA$mFix[names(STA$mFix) %in% traits]
  mr <- STA$mRand[names(STA$mRand) %in% traits]
  TD <- STA$TD[[1]]
  renCols <- attr(TD, "renamedCols")
  predicted <- STA$predicted
  useCheckId <- !is.null(mr) &&
    "checkId" %in% attr(terms(formula(mr[[1]])), "term.labels")
  whatPred <- c("BLUEs", "seBLUEs", "BLUPs", "seBLUPs", "ranEf")
  what <- extractOptSel(what = what, fixed = !is.null(mf),
                        random = !is.null(mr), engine = "lme4")
  ## Create baseData and baseDataPred to which further results will be merged.
  base <- createBaseData(TD, predicted, keep, useRepId, useCheckId,
                         bdPred = any(what %in% whatPred))
  baseData <- base$baseData
  baseDataPred <- base$baseDataPred
  ## Create empty result list.
  result <- setNames(vector(mode = "list", length = length(what)),
                     what)
  ## Use emmeans to convert mf to emmGrid per trait.
  ## Option lmer.df = "asymptotic" for speed. Default options gives
  ## exact the same results as asreml but is very slow.
  em <- sapply(X = mf, FUN = function(mf0) {
    emmeans::emmeans(mf0, specs = predicted, lmer.df = "asymptotic")
  }, simplify = FALSE)
  ## Summarize emGrid to calculate BLUEs en se per trait.
  emStats <- sapply(X = em, FUN = summary, simplify = FALSE)
  ## Extract BLUEs and se of BLUEs from emStats.
  if ("BLUEs" %in% what) {
    predVals <- lapply(X = traits, FUN = function(trait) {
      setNames(emStats[[trait]][c(predicted, "emmean")],
               c(predicted, trait))
    })
    BLUEs <- Reduce(f = function(x, y) merge(x, y, all = TRUE),
                    x = predVals, init = baseDataPred)
    result[["BLUEs"]] <- restoreColNames(renDat = BLUEs, renamedCols = renCols,
                                         restore = restore)
  }
  if ("seBLUEs" %in% what) {
    predErrs <- lapply(X = traits, FUN = function(trait) {
      setNames(emStats[[trait]][c(predicted, "SE")],
               c(predicted, trait))
    })
    seBLUEs <- Reduce(f = function(x, y) merge(x, y, all = TRUE),
                      x = predErrs, init = baseDataPred)
    result[["seBLUEs"]] <- restoreColNames(renDat = seBLUEs,
                                           renamedCols = renCols,
                                           restore = restore)
  }
  ## Compute BLUPs and se of BLUPs from mixed model.
  if ("BLUPs" %in% what) {
    predVals <- lapply(X = traits, FUN = function(trait) {
      ## Extract fixed effects.
      fixEfMr <- lme4::fixef(mr[[trait]])
      ## Get positions of 'repId' within fixed effects.
      repIdPos <- grep(pattern = "repId",
                       x = names(fixEfMr))
      repIdMix <- fixEfMr[repIdPos]
      ## Compute Blups.
      coefs <- coef(mr[[trait]])[[predicted]]
      predVal <- data.frame(rownames(coefs),
                            coefs[, "(Intercept)"] + mean(c(repIdMix, 0)))
      colnames(predVal) <- c(predicted, trait)
      return(predVal)
    })
    BLUPs <- Reduce(f = function(x, y) merge(x, y, all = TRUE),
                    x = predVals, init = baseDataPred)
    result[["BLUPs"]] <- restoreColNames(renDat = BLUPs, renamedCols = renCols,
                                         restore = restore)
  }
  if ("seBLUPs" %in% what) {
    predErrs <- lapply(X = traits, FUN = function(trait) {
      ranEffs <- lme4::ranef(mr[[trait]], condVar = TRUE)[[predicted]]
      predErr <- data.frame(rownames(ranEffs),
                            as.vector(sqrt(attr(ranEffs, "postVar"))))
      colnames(predErr) <- c(predicted, trait)
      return(predErr)
    })
    seBLUPs <- Reduce(f = function(x, y) merge(x, y, all = TRUE),
                      x = predErrs, init = baseDataPred)
    result[["seBLUPs"]] <- restoreColNames(renDat = seBLUPs,
                                           renamedCols = renCols,
                                           restore = restore)
  }
  ## Compute unit errors.
  if ("ue" %in% what) {
    unitErrs <- lapply(X = traits, FUN = function(trait) {
      ## Extract and invert variance covariance matrix.
      V <- vcov(em[[trait]])
      Vinv <- try(chol2inv(chol(V)), silent = TRUE)
      ## Compute unit errors.
      if (!inherits(Vinv, "try-error")) {
        ue <- 1 / diag(Vinv)
      } else {
        ue <- 1 / diag(solve(V))
      }
      unitErr <- data.frame(levels(em[[trait]]), ue)
      colnames(unitErr) <- c(predicted, trait)
      return(unitErr)
    })
    ue <- Reduce(f = function(x, y) merge(x, y, all = TRUE),
                 x = unitErrs, init = baseDataPred)
    result[["ue"]] <- restoreColNames(renDat = ue, renamedCols = renCols,
                                      restore = restore)
  }
  ## Extract variance components for genotype fixed.
  if ("varCompF" %in% what) {
    result[["varCompF"]] <- lapply(X = mf, FUN = extractVarComp,
                                   engine = "lme4")
  }
  ## Extract variance components for genotype random.
  if ("varCompR" %in% what) {
    result[["varCompR"]] <- lapply(X = mr, FUN = extractVarComp,
                                   engine = "lme4")
  }
  ## Extract variances.
  if (any(c("varGen", "varErr", "heritability") %in% what)) {
    varCor <- lapply(X = mr, FUN = lme4::VarCorr)
    varGen <- sapply(X = varCor, FUN = function(vc) {
      vc[[predicted]][1, 1]
    })
    varErr <- sapply(X = varCor, FUN = "attr", "sc") ^ 2
    if ("varGen" %in% what) {
      result[["varGen"]] <- varGen
    }
    if ("varErr" %in% what) {
      result[["varErr"]] <- varErr
    }
    if ("heritability" %in% what) {
      ## Estimate heritability on a line mean basis.
      if (useRepId) {
        result[["heritability"]] <- round(varGen /
                                            (varGen + (varErr / length(unique(TD$repId)))), 2)
      } else {
        result[["heritability"]] <- round(varGen / (varGen + varErr), 2)
      }
    }
  }
  ## Extract fitted values.
  if ("fitted" %in% what) {
    fitVal <- cbind(baseData, sapply(X = mf, FUN = fitted))
    result[["fitted"]] <- restoreColNames(renDat = fitVal, renamedCols = renCols,
                                          restore = restore)
  }
  ## Extract residuals.
  if ("residF" %in% what) {
    resVal <- cbind(baseData, sapply(X = mf, FUN = residuals))
    result[["residF"]] <- restoreColNames(renDat = resVal, renamedCols = renCols,
                                          restore = restore)
  }
  ## Extract standardized residuals.
  if ("stdResF" %in% what) {
    stdRes <- cbind(baseData,
                    sapply(X = mf, FUN = function(mf0) {
                      if (inherits(mf0, "lm")) {
                        stdRes <- rstandard(mf0)
                      } else if (inherits(mf0, "lmerMod")) {
                        stdRes <- residuals(mf0, scaled = TRUE)
                      }
                    }))
    result[["stdResF"]] <- restoreColNames(renDat = stdRes, renamedCols = renCols,
                                           restore = restore)
  }
  ## Compute rMeans.
  ## Use napredict to fill in NAs in data with NAs.
  if ("rMeans" %in% what) {
    rMeans <- cbind(baseData,
                    sapply(X = mr, FUN = function(mr0) {
                      napredict(attr(model.frame(mr0), "na.action"),
                                x = lme4::getME(mr0, "mu"))
                    }))
    result[["rMeans"]] <- restoreColNames(renDat = rMeans, renamedCols = renCols,
                                          restore = restore)
  }
  ## Extract random effects.
  if ("ranEf" %in% what) {
    ranEffs <- lapply(X = traits, FUN = function(trait) {
      effs <- lme4::ranef(mr[[trait]], drop = TRUE)[[predicted]]
      ranEff <- data.frame(names(effs), effs)
      colnames(ranEff) <- c(predicted, trait)
      return(ranEff)
    })
    ranEf <- Reduce(f = function(x, y) merge(x, y, all = TRUE),
                    x = ranEffs, init = baseDataPred)
    result[["ranEf"]] <- restoreColNames(renDat = ranEf, renamedCols = renCols,
                                         restore = restore)
  }
  ## Extract residuals for genotype random.
  if ("residR" %in% what) {
    resVal <- cbind(baseData, sapply(X = mr, FUN = residuals))
    result[["residR"]] <- restoreColNames(renDat = resVal,
                                          renamedCols = renCols,
                                          restore = restore)
  }
  ## Extract standardized residuals for genotype random.
  if ("stdResR" %in% what) {
    stdRes <- cbind(baseData,
                    sapply(X = mr, FUN = function(mr0) {
                      if (inherits(mr0, "lm")) {
                        stdRes <- rstandard(mr0)
                      } else if (inherits(mr0, "lmerMod")) {
                        stdRes <- residuals(mr0, scaled = TRUE)
                      }
                    }))
    result[["stdResR"]] <- restoreColNames(renDat = stdRes,
                                           renamedCols = renCols,
                                           restore = restore)
  }
  ## Compute wald test.
  if ("wald" %in% what) {
    result[["wald"]] <- lapply(X = em, FUN = emmeans::test, joint = TRUE)
  }
  if ("CV" %in% what) {
    ## Compute Coefficient of Variation.
    result[["CV"]] <- sapply(X = mf, FUN = function(mf0) {
      100 * sigma(mf0) / mean(fitted(mf0), na.rm = TRUE)
    })
  }
  if ("rDfF" %in% what) {
    result[["rDfF"]] <- sapply(X = mf, FUN = df.residual)
  }
  if ("rDfR" %in% what) {
    result[["rDfR"]] <- sapply(X = mr, FUN = df.residual)
  }
  return(result)
}

#' Extract statistics from model fitted using asreml
#'
#' @noRd
#' @keywords internal
extractSTAAsreml <- function(STA,
                             traits = NULL,
                             what = "all",
                             keep = NULL,
                             useRepId,
                             restore = FALSE) {
  if (!requireNamespace("asreml", quietly = TRUE)) {
    stop("asreml cannot be successfully loaded.\n")
  }
  mf <- STA$mFix[names(STA$mFix) %in% traits]
  mr <- STA$mRand[names(STA$mRand) %in% traits]
  TD <- STA$TD[[1]]
  renCols <- attr(TD, "renamedCols")
  predicted <- STA$predicted
  useCheckId <- !is.null(mr) &&
    "checkId" %in% attr(terms(mr[[1]]$call$fixed), "term.labels")
  whatPred <- c("BLUEs", "seBLUEs", "BLUPs", "seBLUPs", "ranEf")
  what <- extractOptSel(what = what, fixed = !is.null(mf),
                        random = !is.null(mr), engine = "asreml")
  ## Create baseData and baseDataPred to which further results will be merged.
  base <- createBaseData(TD, predicted, keep, useRepId, useCheckId,
                         bdPred = any(what %in% whatPred))
  baseData <- base$baseData
  baseDataPred <- base$baseDataPred
  ## Create empty result list.
  result <- setNames(vector(mode = "list", length = length(what)), what)
  ## Construct assocForm for use in associate in predict.
  if (length(grep(pattern = "+ checkId +", x = getCall(mf[[1]]))) > 0) {
    #assocForm <- formula("~ checkId:genotype")
    assocForm <- formula("~ NULL")
  } else {
    assocForm <- formula("~ NULL")
  }
  ## Extract BLUEs and se of BLUEs from fixed model.
  if ("BLUEs" %in% what) {
    ## asreml3 saves the predictions inside the asreml object.
    ## asreml4 creates a list containing nothing but the predictions.
    predVals <- lapply(X = traits, FUN = function(trait) {
      mfPred <- predictAsreml(mf[[trait]], TD = TD, associate = assocForm)
      setNames(if (asreml4()) {
        mfPred$pvals[c(predicted, "predicted.value")]
      } else {
        mfPred$predictions$pvals[c(predicted, "predicted.value")]
      }, c(predicted, trait))
    })
    BLUEs <- Reduce(f = merge, x = predVals, init = baseDataPred)
    result[["BLUEs"]] <- restoreColNames(renDat = BLUEs, renamedCols = renCols,
                                         restore = restore)
  }
  if ("seBLUEs" %in% what) {
    ## asreml3 saves the predictions inside the asreml object.
    ## asreml4 creates a list containing nothing but the predictions.
    predErrs <- lapply(X = traits, FUN = function(trait) {
      mfPred <- predictAsreml(mf[[trait]], TD = TD, associate = assocForm)
      setNames(if (asreml4()) {
        mfPred$pvals[c(predicted, "std.error")]
      } else {
        mfPred$predictions$pvals[c(predicted, "standard.error")]
      }, c(predicted, trait))
    })
    seBLUEs <- Reduce(f = merge, x = predErrs, init = baseDataPred)
    result[["seBLUEs"]] <- restoreColNames(renDat = seBLUEs,
                                           renamedCols = renCols,
                                           restore = restore)
  }
  ## Extract BLUPs and se of BLUPs from fixed model.
  if ("BLUPs" %in% what) {
    ## asreml3 saves the predictions inside the asreml object.
    ## asreml4 creates a list containing nothing but the predictions.
    predVals <- lapply(X = traits, FUN = function(trait) {
      mrPred <- predictAsreml(mr[[trait]], TD = TD)
      setNames(if (asreml4()) {
        mrPred$pvals[c(predicted, "predicted.value")]
      } else {
        mrPred$predictions$pvals[c(predicted, "predicted.value")]
      }, c(predicted, trait))
    })
    BLUPs <- Reduce(f = merge, x = predVals, init = baseDataPred)
    result[["BLUPs"]] <- restoreColNames(renDat = BLUPs, renamedCols = renCols,
                                         restore = restore)
  }
  if ("seBLUPs" %in% what) {
    ## asreml3 saves the predictions inside the asreml object.
    ## asreml4 creates a list containing nothing but the predictions.
    predErrs <- lapply(X = traits, FUN = function(trait) {
      mrPred <- predictAsreml(mr[[trait]], TD = TD)
      setNames(if (asreml4()) {
        mrPred$pvals[c(predicted, "std.error")]
      } else {
        mrPred$predictions$pvals[c(predicted, "standard.error")]
      }, c(predicted, trait))
    })
    seBLUPs <- Reduce(f = merge, x = predErrs, init = baseDataPred)
    result[["seBLUPs"]] <- restoreColNames(renDat = seBLUPs, renamedCols = renCols,
                                           restore = restore)
  }
  ## Compute unit errors.
  if ("ue" %in% what) {
    ue <- cbind(baseDataPred, sapply(X = mf, FUN = function(mf0) {
      ## asreml3 saves the predictions inside the asreml object.
      ## asreml4 creates a list containing nothing but the predictions.
      mfPred <- predictAsreml(mf0, TD = TD)
      ## Extract V from mf.
      V <- if (asreml4()) {
        mfPred$vcov
      } else {
        mfPred$predictions$vcov
      }
      ## Remove NA genotype.
      ## Introduced by adding extra rows/columns to do spatial analysis.
      V <- V[!is.na(levels(TD[["genotype"]])), !is.na(levels(TD[["genotype"]]))]
      ## Remove columns and rows containing NA.
      VMiss <- apply(X = V, MARGIN = 2, FUN = anyNA)
      V <- V[!VMiss, !VMiss]
      Vinv <- try(chol2inv(chol(V)), silent = TRUE)
      ## Compute unit errors.
      ue <- rep(x = NA, times = nrow(baseDataPred))
      if (!inherits(Vinv, "try-error")) {
        ue[!VMiss] <- 1 / diag(Vinv)
      } else {
        ue[!VMiss] <- 1 / diag(solve(V))
      }
      return(ue)
    }))
    result[["ue"]] <- restoreColNames(renDat = ue, renamedCols = renCols,
                                      restore = restore)
  }
  ## Extract variance components for genotype fixed.
  if ("varCompF" %in% what) {
    result[["varCompF"]] <- lapply(X = mf, FUN = extractVarComp,
                                   engine = "asreml")
  }
  ## Extract variance components for genotype random.
  if ("varCompR" %in% what) {
    result[["varCompR"]] <- lapply(X = mr, FUN = extractVarComp,
                                   engine = "asreml")
  }
  ## Extract variances.
  ## In asreml3 variances are stored in gammas, in asreml4 in vparameters.
  ## Also the naming is different.
  varGen <- sapply(X = mr, FUN = function(mr0) {
    if (asreml4()) {
      unname(mr0$vparameters[predicted] * mr0$sigma2)
    } else {
      unname(mr0$gammas[pattern = paste0(predicted, "!", predicted, ".var")] *
               mr0$sigma2)
    }
  })
  if ("varGen" %in% what) {
    result[["varGen"]] <- varGen
  }
  if ("varErr" %in% what) {
    result[["varErr"]] <- sapply(X = mr, FUN = function(mr0) {
      if (asreml4()) {
        if ("units" %in% names(mr0$vparameters)) {
          paramVal <- unname(mr0$vparameters["units"])
        } else {
          paramVal <- unname(mr0$vparameters[names(mr0$vparameters) %in%
                                               c("units!R", "rowId:colId!R",
                                                 "rowCoord:colCoord!R",
                                                 "iexp(rowCoord,colCoord)!R")])
        }
      } else {
        paramVal <- unname(mr0$gammas[c("R!variance", "units!units.var")])
      }
      paramVal * mr0$sigma2
    })
  }
  ## Estimate heritability on a line mean basis.
  ## asreml3 saves the predictions inside the asreml object.
  ## asreml4 creates a list containing nothing but the predictions.
  if ("heritability" %in% what) {
    result[["heritability"]] <- sapply(X = traits, FUN = function(trait) {
      mrPred <- predictAsreml(model = mr[[trait]], classify = predicted,
                              vcov = FALSE, TD = TD, only = predicted,
                              sed = TRUE)
      sedSq <- if (asreml4()) {
        mrPred$sed ^ 2
      } else {
        mrPred$predictions$sed ^ 2
      }
      round(unname(1 - mean(sedSq[lower.tri(sedSq)]) /
                     (2 * varGen[[trait]])), 2)
    })
  }
  ## Extract fitted values.
  if ("fitted" %in% what) {
    fitVal <- cbind(baseData, sapply(X = mf, FUN = function(mf0) {
      fitted(mf0)[genoOrCheck(mf0$call$data, "genotype", useCheckId)]
    }))
    result[["fitted"]] <- restoreColNames(renDat = fitVal, renamedCols = renCols,
                                          restore = restore)
  }
  ## Extract residuals.
  if ("residF" %in% what) {
    resVal <- cbind(baseData, sapply(X = mf, FUN = function(mf0) {
      residuals(mf0, type = "response")[genoOrCheck(mf0$call$data,
                                                    "genotype", useCheckId)]
    }))
    result[["residF"]] <- restoreColNames(renDat = resVal, renamedCols = renCols,
                                          restore = restore)
  }
  ## Extract standardized residuals.
  if ("stdResF" %in% what) {
    stdRes <- cbind(baseData, sapply(X = mf, FUN = function(mf0) {
      residuals(mf0, type = "stdCond")[genoOrCheck(mf0$call$data,
                                                   "genotype", useCheckId)]
    }))
    result[["stdResF"]] <- restoreColNames(renDat = stdRes, renamedCols = renCols,
                                           restore = restore)
  }
  ## Extract rMeans.
  if ("rMeans" %in% what) {
    rMeans <- cbind(baseData, sapply(X = mr, FUN = function(mr0) {
      fitted(mr0)[genoOrCheck(mr0$call$data, "genotype", useCheckId)]
    }))
    result[["rMeans"]] <- restoreColNames(renDat = rMeans, renamedCols = renCols,
                                          restore = restore)
  }
  ## Extract random effects.
  if ("ranEf" %in% what) {
    ranEffs <- lapply(X = traits, FUN = function(trait) {
      coefs <- mr[[trait]]$coe$random
      ## In asreml3 coefficients are stored as a vector,
      ## in asreml4 as a data.frame.
      coefNames <- if (asreml4()) rownames(coefs) else names(coefs)
      ranEff <- data.frame(gsub(pattern = paste0(predicted, "_"),
                                replacement = "",
                                x = coefNames[grep(pattern = predicted,
                                                   x = coefNames)]),
                           coefs[grep(pattern = predicted, x = coefNames)])
      colnames(ranEff) <- c(predicted, trait)
      return(ranEff)
    })
    ranEf <- Reduce(f = merge, x = ranEffs, init = baseDataPred)
    result[["ranEf"]] <- restoreColNames(renDat = ranEf, renamedCols = renCols,
                                         restore = restore)
  }
  ## Extract residuals for genotype random.
  if ("residR" %in% what) {
    resVal <- cbind(baseData, sapply(X = mr, FUN = function(mr0) {
      residuals(mr0, type = "response")[genoOrCheck(mr0$call$data,
                                                    "genotype", useCheckId)]
    }))
    result[["residR"]] <- restoreColNames(renDat = resVal,
                                          renamedCols = renCols,
                                          restore = restore)
  }
  ## Extract standardized residuals.
  if ("stdResR" %in% what) {
    stdRes <- cbind(baseData, sapply(X = mr, FUN = function(mr0) {
      residuals(mr0, type = "stdCond")[genoOrCheck(mr0$call$data,
                                                   "genotype", useCheckId)]
    }))
    result[["stdResR"]] <- restoreColNames(renDat = stdRes,
                                           renamedCols = renCols,
                                           restore = restore)
  }
  ## Compute wald test.
  if ("wald" %in% what) {
    result[["wald"]] <- lapply(X = mf, function(mf0) {
      capture.output(wtt <- asreml::wald.asreml(mf0, ssType = "conditional",
                                                denDF = "numeric", maxiter = 200,
                                                data = TD, trace = FALSE),
                     file = tempfile())
      pos <- grep(pattern = predicted, x = row.names(wtt$Wald))
      chi2 <- wtt$Wald$F.con[pos] * wtt$Wald$Df[pos]
      prob <- 1 - pchisq(q = chi2, df = wtt$Wald$Df[pos])
      list(chi2 = c(chi2 = chi2, df = wtt$Wald$Df[pos], P = prob),
           Ftest = c(Fstat = wtt$Wald$F.con[pos],
                     df1 = wtt$Wald$Df[pos],
                     df2 = wtt$Wald$denDF[pos],
                     P = wtt$Wald$Pr[pos]))
    })
  }
  ## Compute Coefficient of Variation.
  if ("CV" %in% what) {
    result[["CV"]] <- sapply(X = mf, function(mf0) {
      100 * summary(mf0)$sigma / mean(fitted(mf0), na.rm = TRUE)
    })
  }
  ## Extract residual degrees of freedom.
  if ("rDfF" %in% what) {
    result[["rDfF"]] <- sapply(X = mf, FUN = "[[", "nedf")
  }
  ## Extract residual degrees of freedom for genotype random.
  if ("rDfR" %in% what) {
    result[["rDfR"]] <- sapply(X = mr, FUN = "[[", "nedf")
  }
  ## Extract standard error of difference.
  ## asreml3 saves the predictions inside the asreml object.
  ## asreml4 creates a list containing nothing but the predictions.
  if ("sed" %in% what) {
    result[["sed"]] <- lapply(X = mf, FUN = function(mf0) {
      mfPred <- predictAsreml(mf0, TD = TD)
      if (asreml4()) {
        mfPred$avsed
      } else {
        mfPred$predictions$avsed
      }
    })
  }
  ## Compute lsd with significance level 5%.
  ## asreml3 saves the predictions inside the asreml object.
  ## asreml4 creates a list containing nothing but the predictions.
  if ("lsd" %in% what) {
    result[["lsd"]] <- lapply(X = mf, FUN = function(mf0) {
      mfPred <- predictAsreml(mf0, TD = TD)
      if (asreml4()) {
        qt(p = .975, df = mf0$nedf) * mfPred$avsed
      } else {
        qt(p = .975, df = mf0$nedf) * mfPred$predictions$avsed
      }
    })
  }
  return(result)
}

#' Helper function for determining if an observation is a valid
#' genotype of check genotype.
#' Used for excluding columns that have been added to create rectangular
#' row-column data required for asreml.
#'
#' @noRd
#' @keywords internal
genoOrCheck <- function(dat,
                        predicted = "genotype",
                        useCheckId = FALSE) {
  geno <- !is.na(as.character(dat[[predicted]]))
  if (useCheckId) {
    check <- is.na(as.character(dat[[predicted]])) &
      !is.na(as.character(dat[["checkId"]]))
    return(geno | check)
  }
  return(geno)
}


#' Helper function for creating baseData
#'
#' @noRd
#' @keywords internal
createBaseData <- function(TD,
                           predicted,
                           keep = NULL,
                           useRepId = FALSE,
                           useCheckId = FALSE,
                           bdPred = FALSE) {
  ## Create baseData consisting of predicted variable, possibly repId and
  ## selected keep columns.
  baseData <- TD[, colnames(TD) %in% c(predicted,
                                       ifelse(useRepId, "repId", ""),
                                       ifelse(useCheckId, "checkId", ""),
                                       keep), drop = FALSE]
  ## Remove NA values for predicted.
  ## Possibly inserted when adding empty lines for missing row/column info.
  baseData <- baseData[genoOrCheck(baseData, predicted, useCheckId), ,
                       drop = FALSE]
  ## Create baseData for predictions with predicted variable(s).
  if (bdPred) {
    baseDataPred <- unique(TD[!is.na(as.character(TD[[predicted]])),
                              predicted, drop = FALSE])
    ## Add columns in keep one-by-one. Only data that is constant within
    ## predicted is actually kept. Other columns are dropped with a warning.
    for (col in keep) {
      TDKeep <- unique(TD[, c(predicted, col)])
      if (!anyDuplicated(TDKeep[, predicted])) {
        baseDataPred <- merge(baseDataPred, TDKeep, by = predicted)
      } else {
        warning("Duplicate values for ", deparse(substitute(col)), ". ",
                "Column dropped.\n", call. = FALSE)
      }
    }
  } else {
    baseDataPred <- NULL
  }
  return(list(baseData = baseData, baseDataPred = baseDataPred))
}

#' Helper function for adding back original colnames
#'
#' @noRd
#' @keywords internal
restoreColNames <- function(renDat,
                            renamedCols = NULL,
                            restore = FALSE) {
  if (restore && !is.null(renamedCols)) {
    renCols <- colnames(renDat)
    ## Get original column names from renamedCols data.frame.
    origCols <- sapply(X = renCols, FUN = function(renCol) {
      if (renCol %in% renamedCols$new) {
        renamedCols$orig[renCol == renamedCols$new]
      } else {
        ## If no renaming took place keep current name.
        renCol
      }
    })
    colnames(renDat) <- origCols
    ## Columns might be duplicated now because one column was renamed to multiple
    ## new columns. Remove those duplicated columns.
    ## Set column to NULL to prevent attributes from being dropped.
    renDat[duplicated(colnames(renDat))] <- NULL
  }
  return(renDat)
}
