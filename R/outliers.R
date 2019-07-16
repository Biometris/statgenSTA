#' Identifying outliers in objects of class SSA
#'
#' Function to identify observations with standardized residuals exceeding
#' \code{rLimit}. If not provided \code{rLimit} is computed as
#' \code{qnorm(1 - 0.5 / rDf)} where \code{rDf} are the residual degrees
#' of freedom for the model. This value is then restricted to the interval
#' 2..4. Alternatively a custom limit may be provided.\cr
#' If \code{verbose = TRUE} a summary is printed of outliers and observations
#' that have the same value for \code{commonFactors}. The latter ones will be
#' marked as similar to distinguish them from the former ones.
#'
#' @param SSA An object of class \code{\link{SSA}}.
#' @param trials A character vector specifying the trials for which outliers
#' should be identified. If \code{trials = NULL}, all trials are included.
#' @param traits A character vector specifying the names of the traits for
#' which outliers should be identified.
#' @param what A character string indicating whether the outlier should be
#' identified for the fitted model with genotype as fixed or genotype as random
#' factor.
#' @param rLimit A numerical value used for determining when a value is
#' considered an outlier. All observations with standardized residuals
#' exceeding \code{rLimit} will be marked as outliers.
#' @param commonFactors A character vector specifying the names of columns
#' in \code{TD} used for selecting observations that are similar to the
#' outliers. If \code{commonFactors = NULL}, only outliers are reported and
#' no similar observations.
#' @param verbose Should the outliers be printed to the console?
#'
#' @return A list with two components:
#' \itemize{
#' \item{indicator - a list of numeric vectors indicating the location of the
#' outliers in the data}
#' \item{outliers - a data.frame containing the outliers and observations
#' similar to the outliers as defined by \code{commonFactors}}
#' }
#'
#' @examples
#' ## Fit a model using lme4.
#' myModel <- fitTD(TD = TDHeat05, traits = "yield", design = "res.rowcol",
#'                 engine = "lme4")
#' ## Detect outliers in the standardized residuals of the fitted model.
#' outliers <- outlierSSA(SSA = myModel, traits = "yield")
#'
#' @export
outlierSSA <- function(SSA,
                       trials = NULL,
                       traits = NULL,
                       what = c("fixed", "random"),
                       rLimit = NULL,
                       commonFactors = NULL,
                       verbose = TRUE) {
  ## Checks.
  if (missing(SSA) || !inherits(SSA, "SSA")) {
    stop("SSA should be a valid object of class SSA.\n")
  }
  if (!is.null(trials) && (!is.character(trials) ||
                           !all(hasName(x = SSA, name = trials)))) {
    stop("trials has to be a character vector defining trials in SSA.\n")
  }
  if (is.null(trials)) {
    trials <- names(SSA)
  }
  if (!is.null(traits) && !is.character(traits)) {
    stop("traits has to be a character vector.\n")
  }
  if (!is.null(rLimit) && (!is.numeric(rLimit) || length(rLimit) > 1 ||
                           rLimit < 0)) {
    stop("rLimit should be NULL or a positive numerical value.\n")
  }
  what <- match.arg(arg = what, choices = c("fixed", "random"))
  outTot <- sapply(X = trials, FUN = function(trial) {
    if (!is.null(commonFactors) && (!is.character(commonFactors) ||
        !all(hasName(x = SSA[[trial]]$TD[[trial]], name = commonFactors)))) {
      stop("commonFactors has to be a character vector defining columns in TD.\n")
    }
    whatMod <- c("mFix", "mRand")[what == c("fixed", "random")]
    if (is.null(SSA[[trial]][[whatMod]])) {
      warning("Model with genotype ", what, " not available for trial ",
              trial, ".\nOutlier detection skipped.")
      return(NULL)
    }
    ## Check that traits are available for current trial.
    if (!is.null(traits)) {
      traitsTr <- traits[hasName(x = SSA[[trial]][[whatMod]],
                                 name = traits)]
      if (length(traitsTr) == 0) {
        ## Skip with warning if no traits available.
        warning("traits not available for trial ", trial, ".\n",
                "Outlier detection for trial ", trial, " skipped.\n")
        return(NULL)
      }
    } else {
      ## If no trait is given as input extract it from the SSA object.
      traitsTr <- names(SSA[[trial]][[whatMod]])
    }
    whatExt <- ifelse(what == "fixed", "stdRes", "stdResR")
    whatExtDf <- ifelse(what == "fixed", "rDf", "rDfR")
    stdRes <- extract(SSA, trials = trial, traits = traitsTr,
                      what = whatExt)[[trial]][[whatExt]]
    rDf <- extract(SSA, trials = trial, traits = traitsTr,
                   what = whatExtDf)[[trial]][[whatExtDf]]
    ## Create empty data.frame for storing results.
    outTr <- indicatorTr <- setNames(vector(mode = "list",
                                            length = length(traitsTr)),
                                     traitsTr)
    for (trait in traitsTr) {
      stdResTr <- stdRes
      ## Compute limit value for residuals.
      if (is.null(rLimit)) {
        rLimit <- min(max(2, qnorm(p = 1 - 0.5 / rDf[trait])), 4)
      }
      datTr <- SSA[[trial]]$TD[[trial]]
      datTr <- datTr[!colnames(datTr) %in% setdiff(traitsTr, trait)]
      ## Compute outliers.
      ## Set missing values to 0 to prevent problems when comparing to rLimit.
      stdResTr[is.na(stdResTr[[trait]]), trait] <- 0
      outVals <- stdResTr[abs(stdResTr[[trait]]) > rLimit, trait]
      if (length(outVals > 0)) {
        ## Rename column for easier joining.
        colnames(stdResTr)[colnames(stdResTr) == trait] <- "res"
        ## Create data.frame with outliers for current trait.
        outTrt <- cbind(datTr, stdResTr["res"])
        if (!is.null(commonFactors)) {
          ## If commonFactors are given merge to data.
          outTrt <- unique(merge(x = outTrt,
                                 y = outTrt[abs(outTrt$res) > rLimit,
                                            commonFactors, drop = FALSE],
                                 by = commonFactors))
        } else {
          outTrt <- outTrt[abs(outTrt$res) > rLimit, ]
        }
        outTrt$similar <- abs(outTrt$res) <= rLimit
        outTrt$trait <- trait
        ## Rename column trait to value.
        colnames(outTrt)[colnames(outTrt) == trait] <- "value"
        ## Change order of columns to always display most relevant info first.
        firstCols <- c("trial", "trait", "value", "res")
        outTrt <- cbind(outTrt[, firstCols],
                        outTrt[!colnames(outTrt) %in% firstCols])
        outTr[[trait]] <- outTrt
        ## Fill indicator column for current trait.
        indicatorTr[[trait]] <- which(abs(stdResTr[["res"]]) > rLimit)
      }
    }
    ## Create one single outlier data.frame.
    outTotTr <- Reduce(f = rbind, x = outTr)
    return(list(outTotTr, indicatorTr))
  }, simplify = FALSE)
  indicatorTot <- lapply(X = outTot, `[[`, 2)
  outTot <- Reduce(f = rbind, x = lapply(X = outTot, `[[`, 1))
  if (verbose) {
    if (!is.null(outTot)) {
      cat(paste("Large standardized residuals.\n\n"))
      print(format(outTot[c("trial", "genotype", "trait", "value", "res",
                            "similar")], quote = FALSE), row.names = FALSE)
    } else {
      cat("No large standardized residuals.\n")
    }
  }
  return(list(indicator = indicatorTot, outliers = outTot))
}


