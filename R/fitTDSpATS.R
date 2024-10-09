#' Fit Single Trial Model using SpATS
#'
#' Fit Single Trial Model using SpATS
#'
#' @inheritParams fitTD
#'
#' @seealso \code{\link{fitTD}}
#'
#' @noRd
#' @keywords internal
fitTDSpATS <- function(TD,
                       trial = NULL,
                       traits,
                       what = c("fixed", "random"),
                       covariates = NULL,
                       useCheckId = FALSE,
                       spatial = FALSE,
                       design = "rowcol",
                       control = NULL,
                       checks = TRUE,
                       ...) {
  ## Base check.
  if (missing(TD) || !inherits(TD, "TD")) {
    stop("TD should be a valid object of class TD.\n")
  }
  ## Only perform other checks if explicitely stated.
  ## Provides the opportunity to skip check if already done in fitTD.
  if (checks) {
    ## Checks.
    checkOut <- modelChecks(TD = TD, trial = trial, design = design,
                            traits = traits, what = what,
                            covariates = covariates, spatial = spatial,
                            engine = "SpATS", useCheckId = useCheckId,
                            control = control)
    ## Convert output to variables.
    list2env(x = checkOut, envir = environment())
  }
  TDTr <- droplevels(TD[[trial]])
  if (design == "res.rowcol") {
    ## Assure nesting of rows and columns within replicates.
    rowTab <- table(TDTr[["repId"]], TDTr[["rowCoord"]])
    colTab <- table(TDTr[["repId"]], TDTr[["colCoord"]])
    ## Count the number of non-zero entries per replicate.
    sumNonZero <- function(x) { sum(x != 0) }
    nRowRep <- max(apply(X = rowTab, MARGIN = 1, FUN = sumNonZero))
    nColRep <- max(apply(X = colTab, MARGIN = 1, FUN = sumNonZero))
    ## Make nesting by assuring row/columns values of zero to max number of
    ## non zero entries for each replicate.
    ## If there is no nesting in one of the dimension nothing will happen.
    TDTr[["rowId"]] <- factor(TDTr[["rowCoord"]] %% nRowRep,
                              levels = c(1:nRowRep, 0))
    TDTr[["colId"]] <- factor(TDTr[["colCoord"]] %% nColRep,
                              levels = c(1:nColRep, 0))
  }
  ## Should repId be used as fixed effect in the model.
  useRepIdFix <- design %in% c("res.ibd", "res.rowcol", "rcbd")
  ## Indicate extra random effects.
  if (design %in% c("ibd", "res.ibd")) {
    randEff <- "subBlock"
  } else if (design %in% c("rowcol", "res.rowcol")) {
    randEff <- c("rowId", "colId")
  } else if (design == "rcbd") {
    ## Set to character() so it can still be used when checking length.
    randEff <- character()
  }
  ## Set default value for nestDiv
  nestDiv <- 2
  ## If valid values for nestDiv are provided in control use these instead.
  if ("nestDiv" %in% names(control)) {
    nestDivCt <- control$nestDiv
    if (length(nestDivCt) == 1) {
      nestDivCt <- rep(x = nestDivCt, times = 2)
    }
    if (is.numeric(nestDivCt) && length(nestDivCt) <= 2 && all(nestDivCt >= 1)) {
      nestDiv <- nestDivCt
    } else {
      warning("Invalid value for control parameter nestDiv. ",
              "Using default values instead.\n")
    }
  }
  ## Compute number of segments.
  ## Defaults to number of cols / 2 and number of rows / 2.
  nSeg <- c(ceiling(length(unique(TDTr[["colCoord"]])) / 2),
            ceiling(length(unique(TDTr[["rowCoord"]])) / 2))
  ## If valid values for nSeg are provided in control use these instead.
  if ("nSeg" %in% names(control)) {
    nSegCt <- control$nSeg
    if (length(nSegCt) == 1) {
      nSegCt <- rep(x = nSegCt, times = 2)
    }
    if (is.numeric(nSegCt) && length(nSegCt) <= 2 && all(nSegCt >= 1) &&
        all(nSegCt <= c(length(unique(TDTr[["colCoord"]])),
                        length(unique(TDTr[["rowCoord"]]))))) {
      nSeg <- nSegCt
    } else {
      warning("Invalid value for control parameter nSeg. ",
              "Using default values instead.\n")
    }
  }
  ## Construct formula for fixed part.
  fixedForm <- formula(paste("~",
                             if (useRepIdFix) "repId" else "1",
                             if (useCheckId) "+ checkId",
                             ## Add "" to start with +.
                             if (!is.null(covariates)) paste(c("", covariates),
                                                             collapse = "+")))
  ## Construct formula for random part. Include repId depending on design.
  if (length(randEff) != 0) {
    randomForm <- formula(paste0("~", if (useRepIdFix) "repId:",
                                 "(", paste(randEff, collapse = "+"), ")"))
  } else {
    randomForm <- NULL
  }
  if ("random" %in% what) {
    mr <- sapply(X = traits, FUN = function(trait) {
      ## Fit model with genotype random.
      modTrR <- tryCatchExt({
        if (all(is.na(TDTr[[trait]]))) {
          stop("Only NA values for trait ", trait, " in trial ", trial, ".\n")
        }
        SpATS::SpATS(response = trait, genotype = "genotype",
                     genotype.as.random = TRUE,
                     spatial = ~ SpATS::PSANOVA(colCoord, rowCoord,
                                                nseg = nSeg,
                                                nest.div = nestDiv),
                     fixed = fixedForm, random = randomForm, data = TDTr,
                     control = list(monitoring = 0), ...)
      })
      if (length(modTrR$warning) != 0) {
        modTrR <- wrnToErr(modTrR)
      }
      if (length(modTrR$warning) != 0) {
        warning("Warning in SpATS for genotype random, trait ", trait,
                " in trial ", trial, ":\n", modTrR$warning, "\n", call. = FALSE)
      }
      if (is.null(modTrR$error)) {
        return(modTrR$value)
      } else {
        warning("Error in SpATS for genotype random, trait ", trait,
                " in trial ", trial, ":\n", modTrR$error, "\n", call. = FALSE)
        return(NULL)
      }
    }, simplify = FALSE)
  } else {
    mr <- NULL
  }
  if ("fixed" %in% what) {
    mf <- sapply(X = traits, FUN = function(trait) {
      ## Fit model with genotype fixed.
      modTrF <- tryCatchExt({
        if (all(is.na(TDTr[[trait]]))) {
          stop("Only NA values for trait ", trait, " in trial ", trial, ".\n")
        }
        SpATS::SpATS(response = trait, genotype = "genotype",
                     genotype.as.random = FALSE,
                     spatial = ~ SpATS::PSANOVA(colCoord, rowCoord,
                                                nseg = nSeg,
                                                nest.div = nestDiv),
                     fixed = fixedForm, random = randomForm, data = TDTr,
                     control = list(monitoring = 0), ...)
      })
      if (length(modTrF$warning) != 0) {
        modTrF <- wrnToErr(modTrF)
      }
      if (length(modTrF$warning) != 0) {
        warning("Warning in SpATS for genotype fixed, trait ", trait,
                " in trial ", trial, ":\n", modTrF$warning, "\n", call. = FALSE)
      }
      if (is.null(modTrF$error)) {
        return(modTrF$value)
      } else {
        warning("Error in SpATS for genotype fixed, trait ", trait,
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
  ## Spatial model is always the same.
  ## Still set it for easier use in summary.
  spatial <- setNames(rep("2 dimensional P-splines", times = length(traits)),
                      traits)
  ## sumTab is just an empty list. Needs to be available so it can be found
  ## by other functions when checking.
  sumTab <- setNames(vector(mode = "list", length = length(traits)), traits)
  return(list(mRand = mr, mFix = mf, TD = TDOut, traits = traits,
              design = design, spatial = spatial, engine = "SpATS",
              predicted = "genotype", sumTab = sumTab, useCheckId = useCheckId))
}
