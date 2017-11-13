#' Forms predicted means (BLUPS) and produces tables based on a set of mega-environments
#'
#' This function calculates predicted means (BLUPS) and associated standard errors based on a
#' set of mega-environments
#'
#' @inheritParams GE.AMMI
#'
#' @param year A character string specifying years within environments.
#' @param ... Other parameters passed to either \code{asreml()} or \code{lmer()}.
#'
#' @examples
#' myDat <- GE.read.csv(system.file("extdata", "F2maize_pheno.csv", package = "RAP"),
#'                      env = "env!", genotype = "genotype!", trait = "yld")
#' myTD <- createTD(data = myDat, genotype = "genotype!", env = "env!")
#' myTDMegaEnv <- GE.megaEnvironment(TD = myTD, trait = "yld")
#' GE.GxETable(TD = myTDMegaEnv, trait = "yld")
#'
#' @import utils
#' @importFrom methods slot
#'
#' @export
GE.GxETable <- function(TD,
                        trait,
                        year = NULL,
                        ...) {
  if (!trait %in% names(TD)) {
    stop(trait," not found in ", TD)
  }
  if (asremlORlme4() == "asreml") {
    tmp <- tempfile()
    sink(file = tmp)
    if (is.null(year)) {
      mr <- try(asreml::asreml(fixed = as.formula(paste(trait, "~ env")),
                               random = as.formula("~ genotype:us(megaEnv)"),
                               data = TD, ...),
                silent = TRUE)
    } else {
      mr <- try(asreml::asreml(fixed = as.formula(paste(trait, "~ env /", year)),
                               random = as.formula(paste("~ genotype:us(megaEnv) +
                                                         genotype:megaEnv:", year)),
                               data = TD, ...),
                silent = TRUE)
    }
    if (inherits(mr, "try-error")){
      genoLevels <- levels(TD$genotype)
      nGenoUnique <- nlevels(TD$genotype)
      megaEnvLevels <- levels(TD$megaEnv)
      nmegaEnvUnique <- nlevels(TD$megaEnv)
      predVals <- se <-  data.frame(matrix(nrow = nGenoUnique, ncol = nmegaEnvUnique),
                                    row.names = genoLevels, check.names = FALSE)
      names(predVals) <- names(se) <- megaEnvLevels
    } else {
      mr$call$fixed <- eval(mr$call$fixed)
      mr$call$random <- eval(mr$call$random)
      mr$call$rcov <- eval(mr$call$rcov)
      mr$call$R.param <- eval(mr$call$R.param)
      mr <- predict(mr, classify = paste0("genotype:megaEnv"), data = TD)
      predictions <- mr$predictions$pvals
      if (!is.factor(predictions$megaEnv)) {
        predictions$megaEnv <- as.factor(predictions$megaEnv)
      }
      if (!is.factor(predictions$genotype)) {
        predictions$genotype <- as.factor(predictions$genotype)
      }
      predVals <- tapply(X = predictions$predicted.value,
                         INDEX = predictions[, c("genotype", "megaEnv")],
                         FUN = identity)
      se <- tapply(X = predictions$standard.error,
                   INDEX = predictions[, c("genotype", "megaEnv")],
                   FUN = identity)
    }
    sink()
    unlink(tmp)
  } else if (asremlORlme4() == "lme4") {
    if (is.null(year)) {
      mr <- try(lme4::lmer(as.formula(paste(trait, "~ env +
                                            (0 + megaEnv | genotype)")),
                           data = TD, ...),
                silent = TRUE)
    } else {
      mr <- try(lme4::lmer(as.formula(paste(trait, "~ env / year + (0 +
                                            megaEnv | genotype) + (0 + megaEnv
                                            | genotype:", year, ")")),
                           data = TD, ...), silent = TRUE)
    }
    genoLevels <- levels(TD$genotype)
    nGenoUnique <- nlevels(TD$genotype)
    megaEnvLevels <- levels(TD$megaEnv)
    nmegaEnvUnique <- nlevels(TD$megaEnv)
    if (inherits(mr, "try-error")){
      predVals <- se <-  data.frame(matrix(nrow = nGenoUnique, ncol = nmegaEnvUnique),
                                    row.names = genoLevels, check.names = FALSE)
      names(predVals) <- names(se) <- megaEnvLevels
    } else {
      # Extract coeffcients mr
      if (class(mr) == "lmerMod") {
        fixEff = lme4::fixef(mr)
      }
      if(class(mr) == "mer") {
        fixEff = slot(mr, "fixef")
      }
      cr = fixEff[grep("env", names(fixEff))]
      ng = length(unique(slot(mr, "flist")[["genotype"]]))
      ranEff = lme4::ranef(mr, drop = TRUE)[["genotype"]]
      blo = mean(c(cr, 0))
      # Predictions BLUPs
      predVals = fixEff[1] + blo + ranEff
      # Compute seBlups
      if(class(mr) == "lmerMod") {
        seBlups = t(sqrt(apply(X = attr(lme4::ranef(mr, condVar = TRUE)[["genotype"]],
                                        "postVar"),
                               MARGIN = 3, FUN = diag)))
      }
      if(class(mr) == "mer") {
        seBlups = t(sqrt(apply(X = attr(lme4::ranef(mr, postVar = TRUE)[["genotype"]],
                                        "postVar"),
                               MARGIN = 3, FUN = diag)))
      }
      se <- data.frame(seBlups, row.names = rownames(predVals), check.names = FALSE)
      names(se) <- names(predVals) <- megaEnvLevels
    }
  } else {
    stop("Either asreml or lme4 is not loaded correctly.")
  }
  return(list(predictedValue = predVals, standardError = se))
}
