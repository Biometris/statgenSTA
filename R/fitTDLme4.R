#' Fit Single Trial Model using lme4
#'
#' Fit Single Trial Model using lme4
#'
#' @inheritParams fitTD
#'
#' @seealso \code{\link{fitTD}}
#'
#' @noRd
#' @keywords internal
fitTDLme4 <- function(TD,
                      trial = NULL,
                      traits,
                      what = c("fixed", "random"),
                      covariates = NULL,
                      useCheckId = FALSE,
                      control = NULL,
                      spatial = FALSE,
                      design = "rowcol",
                      checks = TRUE,
                      ...) {
  ## Base check.
  if (missing(TD) || !inherits(TD, "TD")) {
    stop("TD should be a valid object of class TD.\n")
  }
  if (checks) {
    ## Checks.
    checkOut <- modelChecks(TD = TD, trial = trial, design = design,
                            traits = traits, what = what,
                            covariates = covariates, spatial = spatial,
                            engine = "lme4", useCheckId = useCheckId,
                            control = control)
    ## Convert output to variables.
    list2env(x = checkOut, envir = environment())
  }
  TDTr <- TD[[trial]]
  ## Should repId be used as fixed effect in the model.
  useRepIdFix <- design %in% c("res.ibd", "res.rowcol", "rcbd")
  ## Indicate extra random effects.
  if (design %in% c("ibd", "res.ibd")) {
    randEff <- "subBlock"
  } else if (design %in% c("rowcol", "res.rowcol")) {
    randEff <- c("rowId", "colId")
  } else if (design == "rcbd") {
    randEff <- character()
  }
  ## Construct formula for fixed part.
  fixedForm <- paste("~",
                     if (useRepIdFix) "repId" else "1",
                     if (useCheckId) "+ checkId",
                     if (!is.null(covariates)) paste(c("", covariates),
                                                     collapse = "+"))
  ## Construct formula for random part. Include repId depending on design.
  if (length(randEff) != 0) {
    randomForm <- paste0("(1 | ", if (useRepIdFix) "repId:",
                         paste(randEff,
                               collapse = paste(") + (1 | ",
                                                if (useRepIdFix) "repId:")),
                         ")")
  } else {
    randomForm <- character()
  }
  if ("random" %in% what) {
    mr <- sapply(X = traits, FUN = function(trait) {
      ## Fit model with genotype random.
      modTrR <- tryCatchExt({
        if (all(is.na(TDTr[[trait]]))) {
          stop("Only NA values for trait ", trait, " in trial ", trial, ".\n")
        }
        lme4::lmer(as.formula(paste0("`", trait, "`", fixedForm,
                                     "+ (1 | genotype) ",
                                     if (length(randomForm) != 0)
                                      paste("+", randomForm))), data = TDTr,
                   na.action = na.exclude, ...)
      })
      if (length(modTrR$warning) != 0) {
        warning("Warning in lmer for genotype random, trait ", trait,
                " in trial ", trial, ":\n", modTrR$warning, "\n", call. = FALSE)
      }
      if (is.null(modTrR$error)) {
        return(modTrR$value)
      } else {
        warning("Error in lmer for genotype random, trait ", trait,
                " in trial ", trial, ":\n", modTrR$error, "\n", call. = FALSE)
        return(NULL)
      }
    }, simplify = FALSE)
  } else {
    mr <- NULL
  }
  if ("fixed" %in% what) {
    ## Fit model with genotype fixed.
    ## lme4 cannot handle models without random effect so in that case lm
    ## is called.
    mf <- sapply(X = traits, FUN = function(trait) {
      if (length(randomForm) != 0) {
        modTrF <- tryCatchExt({
          if (all(is.na(TDTr[[trait]]))) {
            stop("Only NA values for trait ", trait, " in trial ", trial, ".\n")
          }
          lme4::lmer(as.formula(paste0("`", trait, "`", fixedForm,
                                       "+ genotype + ", randomForm)),
                     data = TDTr, na.action = na.exclude, ...)
          })
      } else {
        modTrF <- tryCatchExt({
          if (all(is.na(TDTr[[trait]]))) {
            stop("Only NA values for trait ", trait, " in trial ", trial, ".\n")
          }
          lm(as.formula(paste0("`", trait, "`", fixedForm, "+ genotype")),
             data = TDTr, na.action = na.exclude, ...)
          })
      }
      if (length(modTrF$warning) != 0) {
        warning("Warning in lmer for genotype fixed, trait ", trait,
                " in trial ", trial, ":\n", modTrF$warning, "\n", call. = FALSE)
      }
      if (is.null(modTrF$error)) {
        return(modTrF$value)
      } else {
        warning("Error in lmer for genotype fixed, trait ", trait,
                " in trial ", trial, ":\n", modTrF$error, "\n", call. = FALSE)
        return(NULL)
      }
    }, simplify = FALSE)
  } else {
    mf <- NULL
  }
  ## Create TD for output.
  ## Based on TDTr so dropped levels are dropped in output.
  ## Needs attribute from TD[trial].
  TDOut <- createTD(data = TDTr)
  ## If trial was not a column in TDTr the name of the trial is lost and
  ## replaced by "TDTr". To prevent this readd the name.
  names(TDOut) <- trial
  attr(x = TDOut[[trial]], which = "renamedCols") <-
    attr(x = TDTr, which = "renamedCols")
  spatial <- setNames(rep(FALSE, times = length(traits)), traits)
  sumTab <- setNames(vector(mode = "list", length = length(traits)), traits)
  ## Construct STA object.
  return(list(mRand = if ("random" %in% what) mr else NULL,
              mFix = if ("fixed" %in% what) mf else NULL, TD = TDOut,
              traits = traits, design = design, spatial = spatial,
              engine = "lme4", predicted = "genotype", sumTab = sumTab))
}
