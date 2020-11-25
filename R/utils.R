#' Custom tryCatch to return result, errors and warnings.
#' Copied from http://stackoverflow.com/a/24569739/2271856.
#'
#' @noRd
#' @keywords internal
tryCatchExt <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) {
      err <<- conditionMessage(e)
      NULL
    }), warning = function(w) {
      warn <<- c(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  list(value = value, warning = warn, error = err)
}

#' Helper function for suppressing a single warning message.
#'
#' @noRd
#' @keywords internal
supprWarn <- function(expression,
                      message) {
  withCallingHandlers(expression,
                      warning = function(w) {
                        if (grepl(message, w$message)) {
                          invokeRestart("muffleWarning")
                        }
                      })
}

#' Helper function for checking whether error message about 1% change on
#' last iteration for asreml is worth mentioning as a warning.
#' If the corresponding parameter is close to zero then changes of 1%
#' or more can be expected and are ok.
#'
#' @noRd
#' @keywords internal
chkLastIter <- function(model) {
  wrnMsg <- "changed by more than 1%"
  if (any(grepl(pattern = wrnMsg, x = model$warning))) {
    if (asreml4()) {
      ## EXtract trace df from model object.
      mon <- model$value$trace
      ## Extract values for parameters for last 2 iterations.
      ## First 3 rows give general model info.
      lastIt <- mon[-(1:3), c(ncol(mon) - 1, ncol(mon))]
    } else {
      ## EXtract monitor df from model object.
      mon <- model$value$monitor
      ## Extract values for parameters for last 2 iterations.
      ## First 3 rows give general model info. Last col a summary.
      lastIt <- mon[-(1:3), c(ncol(mon) - 2, ncol(mon) - 1)]
    }
    ## Compute change of parameters in last iteration.
    change <- ifelse(lastIt[, 1] == 0, 0, abs((lastIt[, 2] - lastIt[, 1]) /
                                                lastIt[, 1]) * 100)
    ## Suppress warning if the change was less than 5% or the param value less
    ## than 0.1.
    if (all(change <= 5) || all(lastIt[change > 5, 1] < 0.1)) {
      model$warning <- model$warning[!grepl(pattern = wrnMsg,
                                            x = model$warning)]
    }
  }
  return(model)
}

#' Helper function for converting certain asreml warnings to errors.
#'
#' @noRd
#' @keywords internal
wrnToErr <- function(model) {
  wrns <- c("Abnormal termination", "returning -Inf")
  for (wrn in wrns) {
    if (any(grepl(pattern = wrn, x = model$warning))) {
      ## Remove from warnings and add to errors
      model$error <- c(model$error, model$warning[grepl(pattern = wrn,
                                                        x = model$warning)])
      model$warning <- model$warning[!grepl(pattern = wrn,
                                            x = model$warning)]
    }
  }
  return(model)
}

#' Extended version of asreml.predict
#'
#' Asreml has a bug that may throw a warning message:
#' Abnormal termination
#' Insufficient workspace - (reset workspace or pworkspace arguments)
#' This may be avoided by increasing pworkspace, but this doesn't
#' always work.
#' If this happens pworkspace is increased in 'small' steps.
#'
#' @noRd
#' @keywords internal
predictAsreml <- function(model,
                          classify = "genotype",
                          associate = as.formula("~ NULL"),
                          vcov = TRUE,
                          TD,
                          ...) {
  wrnMsg <- "reset workspace or pworkspace arguments"
  ## Predict using default settings, i.e. pworkspace = 8e6
  modelP <- tryCatchExt(predict(model, classify = classify,
                                vcov = vcov, associate = associate,
                                data = TD, maxiter = 20, trace = FALSE, ...))
  pWorkSpace <- 8e6
  ## While there is a warning, increase pWorkSpace and predict again.
  while (!is.null(modelP$warning) &&
         any(grepl(pattern = wrnMsg, x = modelP$warning))
         && pWorkSpace < 160e6) {
    pWorkSpace <- pWorkSpace + 8e6
    modelP <- tryCatchExt(predict(model, classify = classify,
                                  vcov = vcov, associate = associate, data = TD,
                                  maxiter = 20, pworkspace = pWorkSpace,
                                  trace = FALSE, ...))
  }
  if (!is.null(modelP$warning) && !all(grepl(pattern = wrnMsg,
                                             x = modelP$warning))) {
    modelP <- chkLastIter(modelP)
    if (length(modelP$warning) != 0) {
      warning(modelP$warning, call. = FALSE)
    }
  }
  if ((length(modelP$warning) == 0 ||
       !all(grepl(pattern = wrnMsg, x = modelP$warning))) &&
      is.null(modelP$error)) {
    return(modelP$value)
  } else {
    stop("Error in asreml when running predict. Asreml message:\n",
         modelP$error, "\n",
         modelP$warning, "\n", call. = FALSE)
  }
}

#' Helper function for computing the standard error of the variance.
#'
#' @noRd
#' @keywords internal
seVar <- function(x,
                  na.rm = FALSE) {
  if (inherits(x, c("matrix", "data.frame"))) {
    se <- apply(X = x, MARGIN = 2, FUN = seVar, na.rm = na.rm)
  } else if (is.vector(x)) {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    n <- length(x)
    m1 <- sum(x) / n
    m2 <- sum(x ^ 2) / n
    m3 <- sum(x ^ 3) / n
    m4 <- sum(x ^ 4) / n
    se <- sqrt((n * (m4 - 4 * m1 * m3 + 6 * m1 ^ 2 * m2 - 3 * m1 ^ 4) /
                  (n - 1) - (n * (m2 - m1 ^ 2) / (n - 1)) ^ 2) / n)
  } else {
    se <- seVar(x = as.vector(x), na.rm = na.rm)
  }
  return(se)
}

#' Helper function for computing the skewness.
#' This and following formulas taken from
#' https://brownmath.com/stat/shape.htm#Normal.
#'
#' @noRd
#' @keywords internal
skewness <- function(x,
                     na.rm = FALSE) {
  if (inherits(x, c("matrix", "data.frame"))) {
    skw <- apply(X = x, MARGIN = 2, FUN = skewness, na.rm = na.rm)
  } else if (is.vector(x)) {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    n <- length(x)
    skw <- (sum((x - mean(x)) ^ 3) / n) / (sum((x - mean(x)) ^ 2) / n) ^ (3 / 2)
  } else {
    skw <- skewness(x = as.vector(x), na.rm = na.rm)
  }
  return(skw)
}

#' Helper function for computing the standard error of the skewness.
#'
#' @noRd
#' @keywords internal
seSkewness <- function(n) {
  if (n <= 2) {
    warning("For n less than 2 the standard error of skewness cannot be ",
            "calculated", call. = FALSE)
    return(NA)
  }
  return(sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3))))
}

#' Helper function for computing kurtosis.
#' Rescaled by subtracting 3 from the result to give the normal distribution
#' a kurtosis of 0, so basically the excess kurtosis.
#'
#' @noRd
#' @keywords internal
kurtosis <- function(x,
                     na.rm = FALSE) {
  if (inherits(x, c("matrix", "data.frame"))) {
    kurt <- apply(X = x, MARGIN = 2, FUN = kurtosis, na.rm = na.rm)
  } else if (is.vector(x)) {
    if (na.rm) {
      x <- x[!is.na(x)]
    }
    n <- length(x)
    kurt <- n * sum((x - mean(x)) ^ 4) / (sum((x - mean(x)) ^ 2) ^ 2) - 3
  } else {
    kurt <- kurtosis(x = as.vector(x), na.rm = na.rm)
  }
  return(kurt)
}

#' Helper function for computing the standard error of the kurtosis.
#'
#' @noRd
#' @keywords internal
seKurtosis <- function(n) {
  if (n <= 3) {
    warning("For n less than 2 the standard error of kurtosis cannot be ",
            "calculated", call. = FALSE)
    return(NA)
  }
  return(sqrt((24 * n * (n - 1) ^ 2) / ((n - 2) * (n - 3) * (n + 3) * (n + 5))))
}

#' Base method for creating a report
#'
#' Base method for creating a .pdf and .tex report from an \code{R} object.
#'
#' @param x An \code{R} object
#' @param ... Further arguments to be passed on to specific report functions.
#'
#' @seealso \code{\link{report.STA}}
#'
#' @export
report <- function(x,
                   ...) {
  UseMethod("report")
}

#' Helper function for creating the actual report
#'
#' @noRd
#' @keywords internal
createReport <- function(x,
                         reportName,
                         reportPackage,
                         outfile,
                         ...) {
  ## Check provided outfile
  if (!is.null(outfile)) {
    if (!is.character(outfile) || length(outfile) > 1 ||
        tools::file_ext(outfile) != "pdf") {
      stop("Invalid output filename provided.\n")
    }
    ## Since latex cannot handle spaces in figure paths knitr converts those
    ## to pathnames with _. To prevent this spaces are not allowed.
    if (grepl(pattern = " ", x = outfile)) {
      stop("outfile path cannot contain spaces. Provide a path without spaces or
           a relative path.\n")
    }
  } else {
    ## Create a generic output filenane from the name of the report and
    ## the current date/time. The file will be placed in the current
    ## working directory.
    timeStamp <- format(Sys.time(), "%Y%m%d%H%M%S")
    outfile <- paste0("./" , substring(reportName, first = 1,
                                       last = nchar(reportName) - 4),
                      "_", timeStamp, ".pdf")
  }
  ## Extract output directory from outfile.
  outDir <- dirname(outfile)
  ## If output directory doesn't exist, create it.
  if (!dir.exists(outDir)) {
    dir.create(outDir, recursive = TRUE)
  }
  ## When the output need to be written to a top level directory on windows
  ## there may be an extra / at the end of the filename.
  ## This is removed here so file.path works properly further on.
  if (tolower(Sys.info()[["sysname"]]) == "windows") {
    if (substring(text = outDir,
                  first = nchar(outDir)) == .Platform$file.sep) {
      outDir <- substring(text = outDir, first = 1,
                          last = nchar(outDir) - 1)
    }
  }
  ## Extract the name of the outputfile, so without path and extension.
  outBase <- substring(basename(outfile), first = 1,
                       last = nchar(basename(outfile)) - 3)
  ## Construct the output name of the .tex file
  outTex <- file.path(outDir, paste0(outBase, "tex"))
  ## Get the report file from the directory where the package is installed.
  reportFile <- system.file("reports", reportName, package = reportPackage)
  ## Save knitr options for reset when exiting function.
  knitrOptsOrig <- knitr::opts_chunk$get()
  on.exit(knitr::opts_chunk$set(knitrOptsOrig))
  ## Run knitr with chunk options set to produce proper ppt.
  figPrefix <- paste0(format(Sys.time(), "%m%d%y%H%M%S"), "-")
  knitr::opts_chunk$set(fig.show = "hold",
                        fig.path = file.path(outDir, "figures", figPrefix),
                        fig.process = function(x) {
                          paste0("./figures/", basename(x))
                        })
  knitr::knit(input = reportFile, output = outTex, quiet = TRUE)
  ## Construct shell commands for calling pdf latex.
  ## First only draftmode for speed.
  cmdRun1 <- paste0(Sys.which("pdflatex"), " -interaction=nonstopmode -draftmode ",
                    outBase, "tex")
  cmdRun2 <- paste0(Sys.which("pdflatex"), " -interaction=nonstopmode ",
                    outBase, "tex")
  ## Run shell commands. System doesn't work for windows.
  ## Two runs needed to get references right.
  switch(tolower(Sys.info()[["sysname"]]),
         windows = {
           ## Construct shell command for changing directory.
           ## cd /d is used instead of cd to account for changing drives on windows.
           ## Note that here dirname(outfile) is needed instead of outDir.
           cmdDir <- paste0("cd /d ", dirname(outfile))
           shell(cmd = paste(cmdDir, "&", cmdRun1, "> nul 2>&1"))
           shell(cmd = paste(cmdDir, "&", cmdRun2, "> nul"))
         }, linux = {
           ## Construct shell command for changing directory.
           cmdDir <- paste("cd", outDir)
           system(command = paste(cmdDir, ";", cmdRun1, "> /dev/null 2>&1"))
           system(command = paste(cmdDir, ";", cmdRun2, "> /dev/null"))
         }, darwin = {
           ## Construct shell command for changing directory.
           cmdDir <- paste("cd", outDir)
           system(command = paste(cmdDir, ";", cmdRun1, "> /dev/null 2>&1"))
           system(command = paste(cmdDir, ";", cmdRun2, "> /dev/null"))
         })
  ## Remove extra files generated by pdflatex.
  for (extension in c("aux", "log", "out", "toc", "xwm")) {
    unlink(file.path(outDir, paste0(outBase, extension)))
  }
}

#' Function for escaping special LaTeX characters
#'
#' Taken from knitr package. Copied since it is an internal knitr function.
#'
#' @noRd
#' @keywords internal
escapeLatex = function(x, newlines = FALSE, spaces = FALSE) {
  x = gsub('\\\\', '\\\\textbackslash', x)
  x = gsub('([#$%&_{}])', '\\\\\\1', x)
  x = gsub('\\\\textbackslash', '\\\\textbackslash{}', x)
  x = gsub('~', '\\\\textasciitilde{}', x)
  x = gsub('\\^', '\\\\textasciicircum{}', x)
  if (newlines) x = gsub('(?<!\n)\n(?!\n)', '\\\\\\\\', x, perl = TRUE)
  if (spaces) x = gsub('  ', '\\\\ \\\\ ', x)
  x
}

#' Helper function for renaming rows to a more user readable output.
#'
#' @noRd
#' @keywords internal
renameRows <- function(dat) {
  ## data.frame for renaming columns in varcomp and effdim tables.
  renameVars <-
    matrix(nrow = 2,
           dimnames = list(rownames = c("renameFrom", "renameTo")),
           data =
             c("genotype", "Genotype",
               "repId", "Replicate",
               "rowId", "Row",
               "colId", "Col",
               "subBlock", "Block",
               "repId:rowId", "Row(replicate)",
               "repId:colId", "Col(replicate)",
               "repId:subBlock", "Block(replicate)",
               "colCoord", "Linear trend along cols",
               "rowCoord", "Linear trend along rows",
               "rowCoordcolCoord", "Linear trend along rows and cols",
               "f(colCoord)", "Smooth trend along cols",
               "f(rowCoord)", "Smooth trend along rows",
               "f(colCoord):rowCoord", "Linear trend in rows changing smoothly along cols",
               "colCoord:f(rowCoord)", "Linear trend in cols changing smoothly along rows",
               "f(colCoord):f(rowCoord)", "Smooth-by-smooth interaction trend over rows and cols",
               "Nobs", "Number of observations",
               "rowId:colId", "Residual",
               "R", "Residual",
               "variance", "Residual",
               "pow", "Power",
               "units", "Units"))
  renameVars <- as.data.frame(t(renameVars), stringsAsFactors = FALSE)
  for (i in seq_along(renameVars[["renameFrom"]])) {
    rownames(dat)[rownames(dat) == renameVars[["renameFrom"]][i]] <-
      renameVars[["renameTo"]][i]
  }
  return(dat)
}

#' Function for extracting the table with variance components from a model in
#' a nicely printable format.
#'
#' @noRd
#' @keywords internal
extractVarComp <- function(model,
                           engine) {
  if (engine == "SpATS") {
    ## Extract variance components directly from model since using summary
    ## creates a matrix with values already rounded restricting flexibility.
    varComp <- matrix(data = c(model$var.comp, model$psi[1]),
                      dimnames = list(c(names(model$var.comp), "Residual"),
                                      "Variance"))

  } else if (engine == "lme4") {
    if (inherits(model, "lm")) {
      ## In this case there is only residual variance since there are no
      ## random effects.
      varComp <- matrix(data = summary(model)$sigma ^ 2,
                        dimnames = list("Residual", "Variance"))
    } else {
      varComp <- as.data.frame(lme4::VarCorr(model))
      varComp <- matrix(data = varComp$vcov,
                        dimnames = list(varComp$grp, "Variance"))
    }
  } else if (engine == "asreml") {
    ## asreml provides the SE of the variance components as standard output.
    ## This is included in varComp.
    varComp <- as.matrix(summary(model)$varcomp[c("component", "std.error")])
    ## Remove correlations from output. These are present for spatials models.
    varComp <- varComp[!grepl(pattern = ".cor", x = rownames(varComp)), ,
                       drop = FALSE]
    ## To extract user readable names similar to the other engines from
    ## asreml output split the rownames on "." and "!" Depending on the first
    ## part of the split use the appropriate part as row name.
    rownames(varComp) <- sapply(X = strsplit(x = rownames(varComp),
                                             split = "[!.]+"),
                                FUN = function(split) {
                                  if (split[[1]] == "R") {
                                    return(split[[2]])
                                  } else {
                                    return(split[[1]])
                                  }
                                })
    colnames(varComp) <- c("Variance", "SE")
  }
  ## Rename rows for more user readable output.
  varComp <- renameRows(varComp)
  ## Always put genotype as first row.
  if ("Genotype" %in% rownames(varComp)) {
    varComp <- rbind(varComp["Genotype", , drop = FALSE],
                     varComp[rownames(varComp) != "Genotype", , drop = FALSE])
  }
  ## Add an empty row before residuals if it is not already there.
  ## Only done if there is more than 1 row.
  resRow <- which(rownames(varComp) == "Residual")
  if (length(resRow) && nrow(varComp) > 1 && rownames(varComp)[resRow - 1] != "") {
    varComp <- rbind(varComp[1:(resRow - 1), , drop = FALSE],
                     rep(NA, times = ncol(varComp)),
                     varComp[resRow:nrow(varComp), , drop = FALSE])
  }
  return(varComp)
}

#' Helper function for constructing two data.frames containing the coordinates
#' that can be used for plotting a border around parts of a raster plot using
#' geom_path in ggplot2. This can be used to construct an outside border
#' around each replicate in a plot. ggplot2 itself doesn't have this
#' functionality.
#'
#' @noRd
#' @keywords internal
calcPlotBorders <- function(trDat,
                            bordVar) {
  yMin <- min(trDat$rowCoord)
  yMax <- max(trDat$rowCoord)
  xMin <- min(trDat$colCoord)
  xMax <- max(trDat$colCoord)
  ## Create matrix containing replicates.
  ## First create an empty matrix containing all row/column values
  ## between min and max to assure complete missing rows/columns
  ## are added.
  M <- matrix(nrow = yMax - yMin + 1, ncol = xMax - xMin + 1,
              dimnames = list(yMin:yMax, xMin:xMax))
  for (i in 1:nrow(trDat)) {
    M[as.character(trDat[i, "rowCoord"]),
      as.character(trDat[i, "colCoord"])] <- trDat[i, bordVar]
  }
  ## Create an imputed version of M for plotting borders around NA values.
  MImp <- M
  MImp[is.na(MImp)] <- nlevels(trDat[[bordVar]]) + 1
  has.breaks <- function(x) {
    ncol(x) == 2 & nrow(x) > 0
  }
  ## Create a data.frame with positions where the value of rep in the
  ## data changes in vertical direction.
  vertW <- do.call(rbind.data.frame,
                   Filter(f = has.breaks, x = Map(function(i, x) {
                     cbind(y = i, x = which(diff(c(0, x, 0)) != 0))
                   }, 1:nrow(MImp), split(MImp, 1:nrow(MImp)))))
  ## Remove vertical walls that are on the outside bordering an NA value
  ## to prevent drawing of unneeded lines.
  vertW <- vertW[!(vertW$x == 1 & is.na(M[vertW$y, 1])) &
                   !(vertW$x == ncol(M) + 1 &
                       is.na(M[vertW$y, ncol(M)])), ]
  ## Add min row value for plotting in the correct position.
  vertW$y <- vertW$y + yMin - 1
  vertW$x <- vertW$x + xMin - 1
  ## For horizontal walls follow the same procedure as above.
  horW <- do.call(rbind.data.frame,
                  Filter(f = has.breaks, x = Map(function(i, y) {
                    cbind(x = i, y = which(diff(c(0, y, 0)) != 0))
                  }, 1:ncol(MImp), as.data.frame(MImp))))
  horW <- horW[!(horW$y == 1 & is.na(M[1, horW$x])) &
                 !(horW$y == nrow(M) + 1 & is.na(M[nrow(M), horW$x])), ]
  horW$y <- horW$y + yMin - 1
  horW$x <- horW$x + xMin - 1
  return(list(horW = horW, vertW = vertW))
}

#' This function is a slightly modified copy of map_data from ggplot2 combined
#' with map.fortify also from ggplot2.
#' Using the normal function is not possible because both packages qtl and maps
#' have a class map and when building the vignette this gives an error.
#'
#' @noRd
#' @keywords internal
mapData <- function(xLim,
                    yLim) {
  mapObj <- maps::map("world", exact = FALSE, plot = FALSE,
                      fill = TRUE, xlim = xLim, ylim = yLim)
  df <- data.frame(long = mapObj$x, lat = mapObj$y)
  df$group <- cumsum(is.na(df$long) & is.na(df$lat)) + 1
  df$order <- 1:nrow(df)
  names <- do.call("rbind", lapply(strsplit(mapObj$names, "[:,]"),
                                   "[", 1:2))
  df$region <- names[df$group, 1]
  df$subregion <- names[df$group, 2]
  return(df[stats::complete.cases(df$lat, df$long), ])
}

#' @noRd
#' @keywords internal
extractOptSel <- function(what,
                          fixed,
                          random,
                          engine) {
  models <- c("F", "R")[c(fixed, random)]
  extractSel <- extractOptions[extractOptions[[engine]] == 1 &
                                 extractOptions[["model"]] %in% models, ]
  if (what[1] != "all") {
    what <- match.arg(arg = what, choices = extractSel[["result"]],
                      several.ok = TRUE)
    extractSel <- extractSel[extractSel[["result"]] %in% what, ]
  }
  return(extractSel[["result"]])
}

#' Helper function for detecting the version of asreml installed.
#' This is used wherever the syntax for asreml4 differs from asreml3.
#'
#' @noRd
#' @importFrom utils packageVersion
#' @keywords internal
asreml4 <- function() {
  if (requireNamespace("asreml", quietly = TRUE)) {
    if (packageVersion("asreml")[1] >= 4) {
      ## Calling license status apparently also activates the license if this
      ## was done once before.
      licenceStatus <- asreml::asreml.license.status(quiet = TRUE)
      if (licenceStatus$status != 0) {
        stop("Error checking asreml licence status:\n",
             licenceStatus$statusMessage)
      }
      return(TRUE)
    }
    return(FALSE)
  }
}

#' Helper function for row binding data.frames with different columns.
#'
#' @param dfList A list of data.frames.
#'
#' @noRd
#' @keywords internal
dfBind <- function(dfList) {
  ## Filter empty data.frames from dfList
  dfList <- Filter(f = function(x) nrow(x) > 0, x = dfList)
  if (length(dfList) == 0) {
    return(data.frame())
  }
  ## Get variable names from all data.frames.
  allNms <- unique(unlist(lapply(dfList, names)))
  ## rbind all data.frames setting values for missing columns to NA.
  do.call(rbind,
          c(lapply(X = dfList, FUN = function(x) {
            nwDat <- sapply(X = setdiff(allNms, names(x)), FUN = function(y) {
              NA
            })
            data.frame(c(x, nwDat), check.names = FALSE,
                       stringsAsFactors = FALSE)
          }), make.row.names = FALSE)
  )
}

#' Helper function for defining trial colors in plots.
#'
#' @noRd
#' @keywords internal
defineTrialColors <- function(colors,
                              n,
                              default = "black") {
  if (length(colors) == 0) {
    ## Defaults to specified default color for one color for trials.
    ## For more than one colors from statgen.trialColors are used.
    ## Fall back to topo.colors if number of colors in option is too small.
    if (n == 1) {
      colTrial <- default
    } else if (length(getOption("statgen.trialColors")) >= n) {
      colTrial <- getOption("statgen.trialColors")[seq_len(n)]
    } else {
      colTrial <- topo.colors(n = n, alpha = NULL)
    }
  } else {
    nColTrialArg <- length(colors)
    if (nColTrialArg != n) {
      stop("Number of colors provided doesn't match number of trial ",
           "groups:\n", nColTrialArg, " colors provided, ", n,
           " groups in data.\n")
    } else {
      colTrial <- colors
    }
  }
  return(colTrial)
}


