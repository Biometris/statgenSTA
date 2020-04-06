#' Identifying outliers in objects of class STA
#'
#' Function to identify observations with standardized residuals exceeding
#' \code{rLimit}. If not provided \code{rLimit} is computed as
#' \code{qnorm(1 - 0.5 / rDf)} where \code{rDf} are the residual degrees
#' of freedom for the model. This value is then restricted to the interval
#' 2..4. Alternatively a custom limit may be provided.\cr
#' If \code{verbose = TRUE} a summary is printed of outliers and observations
#' that have the same value for \code{commonFactors}. The column outlier in the
#' output can be used to distinguish real outliers from observations included
#' because of their commonFactors.
#'
#' @param STA An object of class \code{STA}.
#' @param trials A character vector specifying the trials for which outliers
#' should be identified. If \code{trials = NULL}, all trials are included.
#' @param traits A character vector specifying the names of the traits for
#' which outliers should be identified.
#' @param what A character string indicating whether the outliers should be
#' identified for the fitted model with genotype as fixed
#' (\code{what = "fixed"})or genotype as random (\code{what = "random"}) factor.
#' If \code{STA} contains only one model this model is chosen automatically.
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
#'
#' ## Detect outliers in the standardized residuals of the fitted model.
#' outliers <- outlierSTA(STA = myModel, traits = "yield")
#'
#' @export
outlierSTA <- function(STA,
                       trials = NULL,
                       traits = NULL,
                       what = NULL,
                       rLimit = NULL,
                       commonFactors = NULL,
                       verbose = TRUE) {
  ## Checks.
  if (missing(STA) || !inherits(STA, "STA")) {
    stop("STA should be a valid object of class STA.\n")
  }
  trials <- chkTrials(trials, STA)
  chkChar(traits)
  chkNum(rLimit, min = 0)
  chkChar(commonFactors)
  outTot <- sapply(X = trials, FUN = function(trial) {
    ## Set boolean for detecting if any outlier detection was actually done.
    ## Used for printing output in the end.
    detection <- FALSE
    ## Checks.
    if (!is.null(commonFactors) && (
      !all(hasName(x = STA[[trial]]$TD[[trial]], name = commonFactors)))) {
      stop("commonFactors has to be a character vector defining columns in TD.\n")
    }
    ## Check that traits are available for current trial.
    traitsTr <- chkTraits(traits, trial, STA[[trial]], err = FALSE)
    if (length(traitsTr) == 0) {
      ## Return NULL to be able to rbind everything together in the end.
      return(NULL)
    }
    if (is.null(what)) {
      what <- ifelse(is.null(STA[[trial]]$mFix), "random", "fixed")
    } else {
      what <- match.arg(arg = what, choices = c("fixed", "random"))
    }
    whatMod <- c("mFix", "mRand")[what == c("fixed", "random")]
    if (is.null(STA[[trial]][[whatMod]]) ||
        is.null(unlist(STA[[trial]][[whatMod]], recursive = FALSE))) {
      warning("Model with genotype ", what, " not available for trial ",
              trial, ".\nOutlier detection skipped.", call. = FALSE)
      return(NULL)
    }
    ## At least one combination of what and trait not skipped.
    ## Set detection to TRUE.
    detection <- TRUE
    whatExt <- ifelse(what == "fixed", "stdResF", "stdResR")
    whatExtDf <- ifelse(what == "fixed", "rDfF", "rDfR")
    stdRes <- extract(STA, trials = trial, traits = traitsTr,
                      what = whatExt)[[trial]][[whatExt]]
    rDf <- extract(STA, trials = trial, traits = traitsTr,
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
      datTr <- STA[[trial]]$TD[[trial]]
      ## Compute outliers.
      if (any(abs(na.omit(stdResTr[[trait]])) > rLimit)) {
        ## Rename column for easier joining.
        stdResTr[["res"]] <- stdResTr[[trait]]
        ## Create data.frame with outliers for current trait.
        outTrt <- cbind(datTr, stdResTr["res"])
        if (!is.null(commonFactors)) {
          ## If commonFactors are given merge to data.
          outTrt <- unique(merge(x = outTrt,
                                 y = outTrt[abs(outTrt[["res"]]) > rLimit,
                                            commonFactors, drop = FALSE],
                                 by = commonFactors))
        } else {
          outTrt <- outTrt[!is.na(outTrt[["res"]]) &
                                    abs(outTrt[["res"]]) > rLimit, ]
        }
        ## Add columns outlier and trait to output.
        outTrt[["outlier"]] <- abs(outTrt[["res"]]) > rLimit
        outTrt[["trait"]] <- trait
        ## Add column value with value of trait.
        ## Leave actual trait column as well for ease of judging outliers.
        outTrt[["value"]] <- outTrt[[trait]]
        ## Change order of columns to always display most relevant info first.
        firstCols <- c("trial", "trait", "value", "res")
        outTrt <- outTrt[c(firstCols, setdiff(colnames(outTrt), firstCols))]
        outTr[[trait]] <- outTrt
        ## Fill indicator column for current trait.
        indicatorTr[[trait]] <- which(abs(stdResTr[["res"]]) > rLimit)
      }
    } # End for loop over traits.
    ## Create one single outlier data.frame.
    outTotTr <- do.call(what = rbind, args = outTr)
    return(list(outTotTr, indicatorTr, detection))
  }, simplify = FALSE) # End lapply over trials.
  ## Check if detecting was done for any of the trials.
  detected <- any(unlist(sapply(X = outTot, FUN = `[[`, 3)))
  ## Create a list of indicators per trial.
  indicatorTot <- lapply(X = outTot, `[[`, 2)
  ## Bind outliers for all trials together in a single data.frame.
  outTot <- do.call(what = rbind, args = lapply(X = outTot, FUN = `[[`, 1))
  if (verbose && detected) {
    if (!is.null(outTot)) {
      cat(paste("Large standardized residuals.\n\n"))
      print(format(outTot[c("trial", "genotype", "trait", "value", "res",
                            "outlier")], quote = FALSE), row.names = FALSE)
    } else {
      cat("No large standardized residuals.\n")
    }
  }
  return(list(indicator = indicatorTot, outliers = outTot))
}


