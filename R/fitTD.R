#' Fit single trial mixed model
#'
#' Perform REML analysis given a specific experimental design using either
#' SpATS, lme4 or asreml. SpATS is used as a default method when row
#' coordinates (rowCoord) and column coordinates (colCoord) are present, lme4
#' otherwise.
#' See details for the exact models fitted.
#'
#' The actual model fitted depends on the design. For the supported designs, the
#' following models are used:
#'
#' design | code | model fitted |
#' --------------------------------------- | ------------ | ----------------------------------------- |
#' incomplete block design | ibd | trait = **subBlock** + genotype + \eqn{\epsilon} |
#' resolvable incomplete block design | res.ibd | trait = *repId* + **repId:subBlock** + genotype + \eqn{\epsilon} |
#' randomized complete block design | rcbd | trait = *repId* + genotype + \eqn{\epsilon} |
#' row column design | rowcol | trait = **rowId** + **colId** + genotype + \eqn{\epsilon} |
#' resolvable row column design | res.rowcol | trait = *repId* + **repId:rowId** + **repId:colId** + genotype + \eqn{\epsilon} |
#'
#' In the models above, fixed effects are indicated in *italics* whereas random
#' effects are indicated in **bold**. genotype can be fitted as fixed or as
#' random effect depending on the value of the parameter \code{what}. Extra
#' fixed effects may be fitted using the parameter \code{covariates}.\cr\cr
#' If \code{SpATS} is used as modeling engine, an extra spatial term is always
#' included  in the model. This term is constructed using the function
#' \code{\link[SpATS]{PSANOVA}} from the SpATS package as\cr
#' \code{PSANOVA(colCoord, rowCoord, nseg = nSeg, nest.div = 2)}
#' where\cr \code{nSeg = (number of columns / 2, number of rows / 2)}. nseg and
#' nest.div can be modified using the \code{control} parameter.\cr\cr
#' When \code{asreml} is used for modeling and \code{spatial} is \code{TRUE}
#' six models are fitted with different random terms and covariance structure.
#' The best model is determined based on a goodness-of-fit criterion, either
#' AIC or BIC. This can be set using the control parameter \code{criterion},
#' default is AIC.
#' The fitted random terms depend on the structure of the data. If the design
#' has a regular structure, i.e. all replicates appear the same amount of times
#' in the design, the following combinations of random and spatial terms are
#' fitted:
#'
#' random part | spatial part |
#' --------------------------------------- | ------------ |
#' random effects based on design | none
#' random effects based on design | ar1(rowId):colId |
#' random effects based on design | rowId:ar1(colId) |
#' random effects based on design | ar1(rowId):ar1(colId) |
#' random effects based on design  + nugget | ar1(rowId):colId |
#' random effects based on design + nugget | rowId:ar1(colId) |
#' random effects based on design + nugget | ar1(rowId):ar1(colId) |
#'
#' If the design is not regular the following combinations of random and spatial
#' terms are fitted:
#'
#' random part | spatial part |
#' --------------------------------------- | ------------ |
#' random effects based on design | none
#' random effects based on design | exp(rowCoord):colCoord |
#' random effects based on design | rowCoord:exp(colCoord) |
#' random effects based on design | iexp(rowCoord, colCoord) |
#' random effects based on design  + nugget | exp(rowCoord):colCoord |
#' random effects based on design + nugget | rowCoord:exp(colCoord) |
#' random effects based on design + nugget | iexp(rowCoord,colCoord) |
#'
#' @param TD An object of class \code{\link{TD}}.
#' @param trials A character vector specifying the trials for which the models
#' should be fitted.
#' @param design A character string specifying the experimental design. Either
#' "ibd" (incomplete block design), "res.ibd" (resolvable incomplete block
#' design), "rcbd" (randomized complete block design), "rowcol" (row column
#' design) or "res.rowcol" (resolvable row column design). Can be ignored when
#' the trial design is specified in the meta data (see \code{\link{setMeta}}).
#' @param traits A character vector specifying the traits for which the models
#' should be fitted.
#' @param what A character vector specifying whether "genotype" should
#' be fitted as fixed or random effect. If not specified, both models are
#' fitted.
#' @param covariates A character vector specifying covariates to be fitted as
#' extra fixed effects in the model.
#' @param useCheckId Should checkId be used as a fixed effect in the model?\cr
#' If \code{TRUE}, \code{TD} has to contain a column 'checkId'.
#' @param spatial Should spatial models be tried? Spatial models can
#' only be fitted with SpATS and asreml. If SpATS is used for modeling, only
#' spatial models can be fitted and spatial is always set to \code{TRUE}. If
#' asreml is used, fitting spatial models is optional.
#' @param engine A character string specifying the name of the mixed modeling
#' engine to use, either "SpATS", "lme4" or "asreml." For spatial models,
#' "SpaTS" is used as default, for other models "lme4".
#' @param control An optional list with control parameters to be passed to the
#' actual fitting functions. Currently \code{nSeg} and \code{nestDiv} are valid
#' parameters when fitting a model using SpATS. They pass a value to nseg and
#' nest.div in \code{\link[SpATS]{PSANOVA}} respectively. For \code{nSeg} also a
#' named list can be supplied containing values for nSeg per trial.\cr
#' \code{criterion} is a valid parameter when fitting a spatial model using
#' asreml. It may be used to pass a goodness-of-fit criterion for comparing
#' different spatial models. See also in details. Other parameters are ignored.
#' @param progress Should the progress of the modeling be printed. If
#' \code{TRUE}, for every trial a line is output indicating the traits fitted
#' for the particular trial.
#' @param ... Further arguments to be passed to \code{SpATS}, \code{lme4} or
#' \code{asreml}.
#'
#' @return An object of class \code{STA}, a list containing, per trial
#' that has been analyzed, a list of:
#' \item{mRand}{A list of models with fitted with genotype as random effect.}
#' \item{mFix}{A list of models fitted with genotype as fixed effect.}
#' \item{TD}{An object of class \code{\link{TD}} containing the data on which
#' \code{mRand} and \code{mFix} are based.}
#' \item{traits}{A character vector indicating the traits for which the models
#' are fitted.}
#' \item{design}{A character string containing the design of the trial.
#' (see \code{\link{fitTD}} for the possible designs).}
#' \item{spatial}{A character string indicating the spatial part of the model.
#' \code{FALSE} if no spatial design has been used.}
#' \item{engine}{A character string containing the engine used for the
#' analysis.}
#' \item{predicted}{A character string indicating the variable that has been
#' predicted.}
#' \item{sumTab}{A data.frame with a summary table for the spatial models tried
#' when \code{engine = "asreml"} and \code{spatial = TRUE}}
#'
#' @references
#' Maria Xose Rodriguez-Alvarez, Martin P. Boer, Fred A. van Eeuwijk, Paul H.C.
#' Eilers (2017). Correcting for spatial heterogeneity in plant breeding
#' experiments with P-splines. Spatial Statistics
#' \url{https://doi.org/10.1016/j.spasta.2017.10.003}
#' @references
#' Butler, D. G., et al. (2010). Analysis of Mixed Models for S language
#' environments: ASReml-R reference manual. Brisbane, DPI Publications
#' @references
#' Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker (2015). Fitting
#' Linear Mixed-Effects Models Using lme4. Journal of Statistical Software,
#' 67(1), 1-48. \url{https://www.jstatsoft.org/article/view/v067i01/0}.
#'
#' @examples
#' ## Fit model using lme4.
#' myModel1 <- fitTD(TD = TDHeat05, design = "ibd", traits = "yield",
#'                   what = "fixed", engine = "lme4")
#'
#' ## Summarize results.
#' summary(myModel1)
#'
#' ## Create base plots of the results.
#' plot(myModel1)
#'
#' ## Create a pdf report summarizing results.
#' \donttest{
#' report(myModel1, outfile = tempfile(fileext = ".pdf"), what = "fixed")
#' }
#'
#' ## Fit model using SpATS.
#' myModel2 <- fitTD(TD = TDHeat05, design = "res.rowcol", traits = "yield",
#'                   what = "fixed")
#' summary(myModel2)
#'
#' ## Create spatial plots of the results.
#' plot(myModel2, plotType = "spatial")
#'
#' ## Create a pdf report summarizing results.
#' \donttest{
#' report(myModel2, outfile = tempfile(fileext = ".pdf"), what = "fixed")
#' }
#'
#' ## Fit model using asreml.
#' if (requireNamespace("asreml", quietly = TRUE)) {
#'   myModel3 <- fitTD(TD = TDHeat05, design = "res.rowcol", traits = "yield",
#'                     what = "fixed", engine = "asreml")
#'   summary(myModel3)
#'
#'   \donttest{
#'   report(myModel3, outfile = tempfile(fileext = ".pdf"), what = "fixed")
#'   }
#' }
#' @export
fitTD = function(TD,
                 trials = names(TD),
                 design = NULL,
                 traits,
                 what = c("fixed", "random"),
                 covariates = NULL,
                 useCheckId = FALSE,
                 spatial = FALSE,
                 engine = NA,
                 control = NULL,
                 progress = FALSE,
                 ...) {
  ## Base check.
  if (missing(TD) || !inherits(TD, "TD")) {
    stop("TD should be a valid object of class TD.\n")
  }
  ## Run models depending on engine.
  models <- sapply(X = trials, FUN = function(trial) {
    ## Checks.
    checkOut <- modelChecks(TD = TD, trial = trial, design = design,
                            traits = traits, what = what,
                            covariates = covariates, spatial = spatial,
                            engine = engine, useCheckId = useCheckId,
                            control = control)
    ## Convert output to variables.
    list2env(x = checkOut, envir = environment())
    if (progress) {
      ## Print progress, list of traits + trial.
      cat(paste0("Fitting models for ", paste(traits, collapse = ", "),
                 " in ", trial, ".\n"))
    }
    model <- do.call(what = paste0("fitTD", tools::toTitleCase(engine)),
                     args = list(TD = TD[trial], trial = trial, traits = traits,
                                 what = what, covariates = covariates,
                                 useCheckId = useCheckId,
                                 spatial = spatial, design = design,
                                 control = control, checks = FALSE,
                                 ... = ...))
    return(model)
  }, simplify = FALSE)
  return(createSTA(models = models))
}

#' Helper function for performing checks for single trial modeling.
#'
#' @noRd
#' @keywords internal
modelChecks <- function(TD,
                        trial,
                        design,
                        traits,
                        what = c("fixed", "random"),
                        covariates,
                        spatial,
                        engine,
                        useCheckId,
                        control) {
  designs <- c("ibd", "res.ibd", "rcbd", "rowcol", "res.rowcol")
  engines <- c("SpATS", "lme4", "asreml")
  if (!is.character(trial) || !trial %in% names(TD)) {
    stop("trial should be in TD.\n")
  }
  if ((is.null(design) && (is.null(attr(TD[[trial]], "trDesign")) ||
                           !attr(TD[[trial]], "trDesign") %in% designs)) ||
      (!is.null(design) && (!is.character(design) || length(design) > 1 ||
                            !design %in% designs))) {
    stop("design should either be an attribute of TD or one of ",
         paste(designs, collapse = ", "), ".\n")
  }
  ## Extract design from TD if needed.
  if (is.null(design)) {
    design <- attr(TD[[trial]], "trDesign")
  }
  chkChar(traits)
  if (!all(hasName(x = TD[[trial]], name = traits))) {
    stop("All traits should be columns in ", trial, ".\n")
  }
  what <- match.arg(arg = what, several.ok = TRUE)
  chkChar(covariates)
  if (!all(hasName(x = TD[[trial]], name = covariates))) {
    stop("All covariates should be columns in ", trial, ".\n")
  }
  if (!is.logical(spatial) || length(spatial) > 1) {
    stop("spatial should be a single logical value.\n")
  }
  if (!is.na(engine) && (!is.character(engine) || length(engine) > 1 ||
                         !engine %in% engines)) {
    stop("engine should be one of ", paste(engines, collapse = ", "), ".\n")
  }
  if (is.na(engine)) {
    if (isTRUE(length(unique(TD[[trial]][["rowCoord"]])) > 1) &&
        isTRUE(length(unique(TD[[trial]][["colCoord"]])) > 1)) {
      message("Using SpATS for fitting models.")
      engine <- "SpATS"
    } else {
      message("Using lme4 for fitting models.")
      engine <- "lme4"
    }
  }
  ## Asreml crashing for traits containing spaces with an incomprehensible
  ## error message. Prevent this from happening by checking beforehand.
  if (engine == "asreml" && any(grepl(pattern = "[ \t\r\n]", x = traits))) {
    stop("asreml cannot fit models when trait contains white space.\n",
         "Rename your trait or use SpATS or lme4 instead.\n")
  }
  if (spatial && engine == "lme4") {
    warning("Spatial models can only be fitted using SpATS or asreml.\n",
            "Defaulting to SpATS.", call. = FALSE)
    engine <- "SpATS"
  }
  ## Columns needed depend on design.
  desCols <- c(if (engine == "SpATS") c("rowCoord", "colCoord"),
               if (design %in% c("rowcol", "res.rowcol")) c("rowId", "colId"),
               if (design %in% c("res.ibd", "res.rowcol", "rcbd")) "repId",
               if (design %in% c("ibd", "res.ibd")) "subBlock",
               if (useCheckId) "checkId")
  for (desCol in desCols) {
    if (!hasName(TD[[trial]], desCol)) {
      stop(desCol, " should be a column in ", trial, ".\n")
    }
  }
  ## SpATS and lme4 will crash for the designs with replicates when repId only
  ## contains a single distinct value.
  ## Change design to the corresponding design without replicates if this is the
  ## case.
  if (design %in% c("res.ibd", "res.rowcol") &&
      length(unique(TD[[trial]][["repId"]])) == 1) {
    design <- substring(text = design, first = 5)
    warning(trial, " contains only one distinct value for repId.\n",
            "Design changed to ", design, ".\n", call. = FALSE)
  }
  if (!is.null(control) && !is.list(control)) {
    stop("control has to be NULL or a list.\n")
  }
  ## nSeg should either be a single value or a named list of values with
  ## names corresponding to trials. - only relevant for SpATS.
  if (engine == "SpATS" && !is.null(control$nSeg)) {
    if (is.list(control$nSeg)) {
      if (!hasName(x = control$nSeg, name = trial)) {
        stop(trial, " should be a named item in list of nSeg in control")
      } else {
        control$nSeg <- control$nSeg[[trial]]
      }
    }
  }
  return(list(design = design, what = what, engine = engine, control = control))
}
