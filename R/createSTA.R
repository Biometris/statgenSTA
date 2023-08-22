#' S3 class STA
#'
#' Function for creating objects of S3 class Single Site Analysis (STA).\cr
#' \code{\link{summary}}, \code{\link{plot}} and \code{\link{report}}
#' methods are available.
#'
#' @param models A list of trials with for each trial the following elements
#' \itemize{
#' \item{mRand}{A list of models with fitted with genotype as random effect.}
#' \item{mFix}{A list of models fitted with genotype as fixed effect.}
#' \item{TD}{An object of class \code{\link{TD}} containing the data on which
#' \code{mRand} and \code{mFix} are based.}
#' \item{traits}{A character vector indicating the traits for which the analysis
#' is done.}
#' \item{design}{A character string containing the design of the trial.
#' (see \code{\link{fitTD}} for the possible designs).}
#' \item{spatial}{A character string indicating the spatial part of the model.
#' \code{FALSE} if no spatial design has been used.}
#' \item{engine}{A character string containing the engine used for the
#' analysis.}
#' \item{predicted}{A character string indicating the variable that has been
#' predicted.}
#' }
#'
#' @noRd
#' @keywords internal
createSTA <- function(models) {
  STA <- structure(models,
                   class = c("STA", "list"),
                   timestamp = Sys.time())
  return(STA)
}

#' Function for extracting for objects of class STA that keeps class.
#'
#' @noRd
#' @keywords internal
`[.STA` <- function(x, i, ...) {
  r <- NextMethod("[")
  attr(r, "class") <- attr(x, "class")
  attr(r, "timestamp") <- attr(x, "timestamp")
  return(r)
}

#' Summarizing objects of class \code{STA}
#'
#' \code{summary} method for class \code{STA}.
#'
#' @param object An object of class \code{STA}.
#' @param trials A character vector indicating the trial to summarize. If
#' \code{trial = NULL} a summary is made of all trials in the \code{STA} object.
#' If a single trial is selected a full summary for this trial is created. For
#' multiple trials a summary table with the most importantant statistics is
#' returned.
#' @param trait A character string indicating the trait to summarize. If
#' \code{trait = NULL} and only one trait is modeled, this trait is summarized.
#' @param nBest An integer indicating the number of the best genotypes (sorted
#' by either BLUEs or BLUPs) to print. If \code{NA}, all genotypes will be
#' printed.
#' @param sortBy A character string specifying how the genotypes will be sorted.
#' Either \code{"BLUEs"}, \code{"BLUPs"} or \code{NA} (i.e. no sorting).
#' @param naLast Should missing values in the data be put last when sorting?
#' @param decreasing Should the sort order be decreasing?
#' @param ... Further arguments - not used.
#'
#' @examples
#' ## Run a single trait analysis using SpATS.
#' modSp <- fitTD(TD = TDHeat05,
#'                design = "res.rowcol",
#'                traits = "yield")
#'
#' ## Print a summary of the fitted model.
#' summary(modSp)
#'
#' @family functions for STA objects
#'
#' @export
summary.STA <- function(object,
                        trials = NULL,
                        trait = NULL,
                        nBest = 20,
                        sortBy = NULL,
                        naLast = TRUE,
                        decreasing = TRUE,
                        ...) {
  ## Checks.
  trials <- chkTrials(trials, object)
  if (is.null(trait) && length(object[[trials[1]]]$traits) > 1) {
    stop("No trait provided but multiple traits found.\n")
  }
  if (!is.null(trait) && (!is.character(trait) || length(trait) > 1 ||
                          !all(sapply(X = trials, FUN = function(trial) {
                            hasName(x = object[[trial]]$TD[[trial]],
                                    name = trait)
                          })))) {
    stop("trait has to be a single character string defining a column in TD.\n")
  }
  if (is.null(trait)) {
    trait <- object[[trials[1]]]$traits
  }
  ## If sortBy not provided, sort by BLUEs if available, BLUPs otherwise.
  ## If sortBy is provided, but only 1 of genotype fixed/ genotype random is
  ## fitted, ignore sortBy and overrule by available output.
  if (is.null(sortBy) || is.null(object[[trials[1]]]$mFix) ||
      is.null(object[[trials[1]]]$mRand)) {
    sortBy <- ifelse(!is.null(object[[trials[1]]]$mFix), "BLUEs", "BLUPs")
  } else {
    sortBy <- match.arg(sortBy, choices = c("BLUEs", "BLUPs"))
  }
  ## If multiple trials supplied just create a table of BLUEs/BLUPs
  if (length(trials) > 1) {
    predTD <- STAtoTD(object, what = sortBy, traits = trait)
    ## Create summary table with default statistics.
    ## Transpose to get trials in rows, stats in columns.
    sumTab <- t(sapply(X = names(predTD), FUN = function(trial) {
      summary(predTD, trial = trial, traits = trait)[, 1, 1]
    }))
    ## Order by mean in descending order.
    sumTab <- sumTab[order(sumTab[, colnames(sumTab) == "Mean"],
                           decreasing = TRUE), ]
    return(structure(list(sumTab = sumTab, what = sortBy, trait = trait),
                     class = c("summary.STA")))
  } else {
    ## Get summary stats for raw data.
    TD <- object[[trials]]$TD
    if (is.null(trait) || (is.null(object[[trials]]$mFix[[trait]]) &&
                           is.null(object[[trials]]$mRand[[trait]]))) {
      stop("No fitted model found for ", trait, " in ", trials, ".\n")
    }
    stats <- summary.TD(object = TD, traits = trait)
    ## get predicted means (BLUEs + BLUPs).
    extr <- extractSTA(object, trials = trials, traits = trait)[[trials]]
    ## Merge results using a loop to avoid warnings over suffixes caused by
    ## merge when using Reduce.
    joinList <- Filter(f = Negate(f = is.null),
                       x = extr[c("BLUEs", "seBLUEs", "BLUPs", "seBLUPs")])
    meanTab <- joinList[[1]]
    for (i in 2:length(joinList)) {
      meanTab <- merge(meanTab, joinList[[i]], all = TRUE, by = "genotype",
                       suffixes = c(i, i + 1))
    }
    ## Move genotype to rowname for proper printing with printCoefMat.
    rownames(meanTab) <- meanTab$genotype
    meanTab <- meanTab[colnames(meanTab) != "genotype"]
    ## Set colnames. Because of duplicate colname SE no selection on columns can
    ## be done anymore after this.
    colnames(meanTab) <- c(if (!is.null(extr$BLUEs)) c("BLUEs", "SE"),
                           if (!is.null(extr$BLUPs)) c("BLUPs", "SE"))
    meansTxt <- paste(c(if (!is.null(extr$BLUEs)) "BLUEs",
                        if (!is.null(extr$BLUPs)) "BLUPs"), collapse = " & ")
    attr(x = meanTab, which = "title") <- meansTxt
    if (!is.na(sortBy)) {
      ## Sort by sortBy with options from input params.
      oList <- order(meanTab[[sortBy]], na.last = naLast,
                     decreasing = decreasing)
      meanTab <- meanTab[oList, ]
    }
    if (!is.na(nBest)) {
      ## Set nBest to number of rows in meanTab to prevent printing of NA rows.
      nBest <- min(nrow(meanTab), nBest)
      ## Extract the n best genotypes.
      meanTab <- meanTab[1:nBest, ]
      attr(x = meanTab, which = "nBest") <- nBest
    }
    ## Extract selected spatial model and
    ## summary table for fitted spatial models when applicable.
    if (object[[trials]]$engine == "asreml" &&
        is.character(object[[trials]]$spatial[[trait]])) {
      selSpatMod <- object[[trials]]$spatial[[trait]]
      spatSumTab <- object[[trials]]$sumTab[[trait]]
    } else {
      selSpatMod <- NULL
      spatSumTab <- NULL
    }
    return(structure(list(selSpatMod = selSpatMod, stats = stats,
                          meanTab = meanTab, heritability = extr$heritability,
                          sed = data.frame("s.e.d" = extr$sed),
                          lsd = data.frame("l.s.d." = extr$lsd),
                          spatSumTab = spatSumTab),
                     class = c("summary.STA")))
  }
}

#' Printing summarized objects of class STA
#'
#' \code{print} method for object of class summary.STA created by summarizing
#' objects of class STA.
#'
#' @param x An object of class \code{summary.STA}
#' @param digits An integer indicating the number of significant digits for
#' printing.
#' @param ... Further arguments passed to \code{\link[stats]{printCoefmat}}.
#'
#' @noRd
#' @export
print.summary.STA <- function(x,
                              digits = max(getOption("digits") - 2, 3),
                              ...) {
  if (!is.null(x$sumTab)) {
    cat("Summary statistics for", x$what, "of", x$trait, "\n\n")
    print(x$sumTab)
  } else {
    if (!is.null(x$spatSumTab)) {
      cat("Overview of tried spatial models",
          "\n================================\n")
      print(x$spatSumTab, digits = digits)
    }
    if (!is.null(x$selSpatMod)) {
      cat("\nSelected spatial model: ", x$selSpatMod, "\n\n")
    }
    cat("Summary statistics",
        "\n==================\n")
    ## Print stats using printCoefMat for a nicer layout.
    print(x$stats)
    if (!is.null(x$heritability)) {
      cat("\nEstimated heritability",
          "\n======================\n")
      cat("\nHeritability:", x$heritability, "\n")
    }
    cat(paste0("\nPredicted means (", attr(x = x$meanTab, which = "title"), ")"),
        "\n===============================\n")
    if (!is.null(attr(x = x$meanTab, which = "nBest"))) {
      cat("Best", attr(x = x$meanTab, which = "nBest"), "genotypes\n")
    } else {
      cat("\n")
    }
    printCoefmat(x$meanTab, digits = digits, ...)
    if (nrow(x$sed) > 0) {
      cat("\nStandard Error of Difference (genotype modeled as fixed effect)",
          "\n===============================================================\n")
      printCoefmat(x$sed, digits = digits, ...)
    }
    if (nrow(x$lsd) > 0) {
      cat("\nLeast Significant Difference (genotype modeled as fixed effect)",
          "\n===============================================================\n")
      printCoefmat(x$lsd, digits = digits, ...)
    }
  }
}

#' Plot function for class STA
#'
#' This function draws either four base plots:
#' \itemize{
#' \item{A histogram of the residuals}
#' \item{A normal Q-Q plot}
#' \item{A residuals vs fitted values plot}
#' \item{An absolute residuals vs fitted values plot}
#' }
#' or five or six spatial plots:
#' \itemize{
#' \item{A spatial plot of the raw data}
#' \item{A spatial plot of the fitted data}
#' \item{A spatial plot of the residuals}
#' \item{A spatial plot of the estimated spatial trend (SpATS only)}
#' \item{A spatial plot of the BLUEs or BLUPs}
#' \item{A histogram of the BLUEs or BLUPs}
#' }
#' Spatial plots can only be made if the data contains both row and column
#' information.
#'
#' @param x An object of class STA.
#' @param ... Further graphical parameters.
#' @param trials A character vector indicating the trials to plot. If
#' \code{trials = NULL}, all trials are plotted.
#' @param traits A character vector indicating the traits to plot. If
#' \code{traits = NULL}, all traits are plotted.
#' @param what A character string indicating whether the fitted model with
#' genotype as fixed (\code{what = "fixed"}) or genotype as random
#' (\code{what = "random"}) factor should be plotted.
#' If \code{x} contains only one model this model is chosen automatically.
#' @param plotType A character string indicating whether \code{base} plots or
#' \code{spatial} plots should be made.
#' @param spaTrend A character string indicating how the spatial trend should
#' be displayed. Either "raw" (original scale), or "percentage". If
#' "percentage", the estimated spatial trend is scaled (i.e., divided by the
#' average of the observed response variable of interest across the field) and
#' results are shown as a percentage.
#' @param outCols An integer indicating the number of columns to use for
#' displaying the plots. Usually the default of 2 for base plots and 3 for
#' spatial plots will be fine, but decreasing the numbers may help for nicer
#' printing.
#' @param title A character string used a title for the plot. Note that when
#' a title is specified and multiple plots are created, all plots will get the
#' same title.
#' @param output Should the plot be output to the current device? If
#' \code{FALSE} only a list of ggplot objects is invisibly returned.
#'
#' @return A list containing ggplot objects for the selected plots.
#'
#' @examples
#' ## Run a single trait analysis using SpATS.
#' modSp <- fitTD(TD = TDHeat05,
#'                design = "res.rowcol",
#'                traits = "yield")
#'
#' ## Create base plots.
#' plot(modSp,
#'      what = "fixed",
#'      plotType = "base")
#'
#' ## Create spatial plots.
#' plot(modSp,
#'      what = "fixed",
#'      plotType = "spatial")
#'
#' ## Create spatial plots showing the spatial trend as percentage.
#' plot(modSp,
#'      what = "fixed",
#'      plotType = "spatial",
#'      spaTrend = "percentage")
#'
#' @family functions for STA objects
#'
#' @importFrom grDevices topo.colors colorRampPalette
#' @export
plot.STA <- function(x,
                     ...,
                     trials = NULL,
                     traits = NULL,
                     what = NULL,
                     plotType = c("base", "spatial"),
                     spaTrend = c("raw", "percentage"),
                     outCols = ifelse(plotType == "base", 2, 3),
                     title = NULL,
                     output = TRUE) {
  ## Checks.
  trials <- chkTrials(trials, x)
  chkChar(traits)
  plotType <- match.arg(arg = plotType)
  chkNum(outCols, min = 1, null = FALSE, incl = TRUE)
  spaTrend <- match.arg(arg = spaTrend)
  chkChar(title, len = 1)
  dotArgs <- list(...)
  p <- setNames(vector(mode = "list", length = length(trials)), trials)
  for (trial in trials) {
    traitsTr <- chkTraits(traits, trial, x[[trial]], err = FALSE)
    trDat <- x[[trial]]$TD[[trial]]
    if (length(traitsTr) == 0) {
      next
    }
    ## Check row column information in trDat - available, no missings.
    if (plotType == "spatial" && !chkRowCol(trDat)) next
    if (is.null(what)) {
      what <- ifelse(is.null(x[[trial]]$mFix), "random", "fixed")
    } else {
      what <- match.arg(arg = what, choices = c("fixed", "random"))
    }
    if (x[[trial]]$engine == "SpATS" &&
        length(grep(pattern = "checkId",
                    x = deparse(x[[trial]][[ifelse(what == "fixed", "mFix",
                                                   "mRandom")]][[1]]$model$fixed))) > 0) {
      useCheckId <- TRUE
    } else {
      useCheckId <- FALSE
    }
    if (all(hasName(x = trDat, name = c("colCoord", "rowCoord")))) {
      mergeCols <- c("colCoord", "rowCoord")
    } else {
      mergeCols <- NULL
    }
    if (useCheckId) {
      mergeCols <- c(mergeCols, "checkId")
    }
    pTr <- setNames(vector(mode = "list", length = length(traits)), traits)
    for (trait in traitsTr) {
      ## Extract the model to plot from the STA object.
      if (what == "fixed") {
        model <- x[[trial]]$mFix[[trait]]
      } else if (what == "random") {
        model <- x[[trial]]$mRand[[trait]]
      }
      if (is.null(model)) {
        warning("No model with genotype ", what, " for trial ", trial,
                " and trait ", trait, ".\n", "Plots for trial ",
                trial, " and trait ", trait, " skipped.\n")
        next
      }
      predicted <- x[[trial]]$predicted
      ## Extract fitted and predicted values from model.
      fitted <- extractSTA(x, trials = trial, traits = trait,
                           what = ifelse(what == "fixed", "fitted", "rMeans"),
                           keep = mergeCols)
      predType <- ifelse(what == "fixed", "BLUEs", "BLUPs")
      pred <- extractSTA(x, trials = trial, traits = trait,
                         what = predType)[c(predicted, trait)]
      ## Extract raw data and compute residuals.
      response <- trDat[, c(predicted, trait, mergeCols)]
      ## Create plot data by merging extracted data together and renaming some
      ## columns.
      plotDat <- merge(response, fitted, by = c(predicted, mergeCols))
      plotDat <- merge(plotDat, pred, by = predicted)
      plotDat[["response"]] <- plotDat[[paste0(trait, ".x")]]
      plotDat[["fitted"]] <- plotDat[[paste0(trait, ".y")]]
      plotDat[["pred"]] <- plotDat[[trait]]
      plotDat[is.na(plotDat[["fitted"]]), "pred"] <- NA
      plotDat[["residuals"]] <- plotDat[["response"]] - plotDat[["fitted"]]
      ## Create empty list for storing plots.
      plots <- vector(mode = "list")
      ## Create main plot title.
      if (is.null(title)) {
        plotTitle <- paste("Trial:", trial, "Trait:", trait)
      } else {
        plotTitle <- title
      }
      if (plotType == "base") {
        plotDat <- ggplot2::remove_missing(plotDat, na.rm = TRUE)
        ## Plot histogram of residuals.
        plots$p1 <-
          ggplot2::ggplot(data = plotDat,
                          ggplot2::aes(x = .data[["residuals"]],
                                       y = ggplot2::after_stat(count / sum(count)))) +
          ggplot2::geom_histogram(fill = "darkgrey", color = "grey50", bins = 10,
                                  boundary = 0) +
          ggplot2::scale_y_continuous(expand = c(0, 0, 0, 0.05),
                                      labels = function(x) {paste0(100 * x, "%")}) +
          ggplot2::labs(y = "Frequency", x = "Residuals") +
          ggplot2::theme(panel.background = ggplot2::element_blank(),
                         panel.grid = ggplot2::element_blank(),
                         panel.border = ggplot2::element_rect(color = "black",
                                                              fill = NA))
        ## Plot QQ plot of residuals.
        plots$p2 <-
          ggplot2::ggplot(data = plotDat,
                          ggplot2::aes(sample = .data[["residuals"]])) +
          ggplot2::stat_qq(shape = 1, alpha = 0.7) +
          ggplot2::labs(y = "Residuals", x = "Normal quantiles") +
          ggplot2::theme(panel.background = ggplot2::element_blank(),
                         panel.grid = ggplot2::element_blank(),
                         panel.border = ggplot2::element_rect(color = "black",
                                                              fill = NA))
        ## Plot residuals vs fitted values.
        plots$p3 <-
          ggplot2::ggplot(data = plotDat,
                          ggplot2::aes(x = .data[["fitted"]],
                                       y = .data[["residuals"]])) +
          ggplot2::geom_point(shape = 1, alpha = 0.7) +
          ggplot2::geom_smooth(method = "loess", formula = "y ~ x",
                               color = "red") +
          ggplot2::geom_hline(yintercept = 0) +
          ggplot2::labs(y = "Residuals", x = "Fitted values") +
          ggplot2::theme(panel.background = ggplot2::element_blank(),
                         panel.grid = ggplot2::element_blank(),
                         panel.border = ggplot2::element_rect(color = "black",
                                                              fill = NA))
        ## Plot absolute value of residuals vs fitted values.
        plots$p4 <-
          ggplot2::ggplot(data = plotDat,
                          ggplot2::aes(x = .data[["fitted"]],
                                       y = abs(.data[["residuals"]]))) +
          ggplot2::geom_point(shape = 1, alpha = 0.7) +
          ggplot2::geom_smooth(method = "loess", formula = "y ~ x",
                               color = "red") +
          ggplot2::labs(y = "|Residuals|", x = "Fitted values") +
          ggplot2::theme(panel.background = ggplot2::element_blank(),
                         panel.grid = ggplot2::element_blank(),
                         panel.border = ggplot2::element_rect(color = "black",
                                                              fill = NA))
        if (output) {
          ## do.call is needed since grid.arrange doesn't accept lists as input.
          do.call(gridExtra::grid.arrange,
                  args = c(plots, list(ncol = outCols, top = plotTitle)))
        }
        pTr[[trait]] <- plots
      } else if (plotType == "spatial") {
        yMin <- min(plotDat[["rowCoord"]])
        yMax <- max(plotDat[["rowCoord"]])
        xMin <- min(plotDat[["colCoord"]])
        xMax <- max(plotDat[["colCoord"]])
        if (x[[trial]]$engine == "SpATS") {
          ## Execute this part first since it needs plotData without missings
          ## removed.
          ## Code mimickes code from SpATS package but is adapted to create a
          ## data.frame useable by ggplot.
          plotDat <- plotDat[order(plotDat[["colCoord"]],
                                   plotDat[["rowCoord"]]), ]
          nCol <- xMax - xMin + 1
          nRow <- yMax - yMin + 1
          p1 <- 100 %/% nCol + 1
          p2 <- 100 %/% nRow + 1
          ## Get spatial trend from SpATS object.
          spatTr <- SpATS::obtain.spatialtrend(model,
                                               grid = c(nCol * p1, nRow * p2))
          ## spatial trend contains values for all data points, so NA in original
          ## data need to be removed. The kronecker multiplication is needed to
          ## convert the normal row col pattern to the smaller grid extending the
          ## missing values.
          ## First a matrix M is created containing information for all
          ## columns/rows in the field even if they are completely empty.
          M <- matrix(nrow = nRow, ncol = nCol,
                      dimnames = list(yMin:yMax, xMin:xMax))
          for (i in 1:nrow(plotDat)) {
            M[as.character(plotDat[i, "rowCoord"]),
              as.character(plotDat[i, "colCoord"])] <-
              ifelse(is.na(plotDat[i, "response"]), NA, 1)
          }
          spatTrDat <- kronecker(M, matrix(data = 1, ncol = p1, nrow = p2)) *
            spatTr$fit
          ## Melt to get the data in ggplot shape. Rows and columns in the
          ## spatial trend coming from SpATS are swapped so therefore use t()
          spatTrDat <- as.data.frame(t(spatTrDat))
          plotDatSpat <- reshape(spatTrDat, direction = "long",
                                 varying = list(rowCoord = colnames(spatTrDat)),
                                 timevar = "rowCoord", idvar = "colCoord",
                                 v.names = "value")
          ## Add true values for columns and rows for plotting.
          plotDatSpat[["colCoord"]] <- spatTr$col.p
          plotDatSpat[["rowCoord"]] <- rep(x = spatTr$row.p, each = p1 * nCol)
          ## Remove missings from data.
          plotDatSpat <- ggplot2::remove_missing(plotDatSpat, na.rm = TRUE)
        }
        ## Create data.frame with all rows columns in field.
        ## Full missing rows/columns are included.
        ## If not geom_tile just fills the empty columns by expanding the
        ## neighboring colums (or rows).
        fullGrid <- expand.grid(colCoord = xMin:xMax, rowCoord = yMin:yMax)
        plotDat <- merge(fullGrid, plotDat, all.x = TRUE)
        ## Code taken from plot.SpATS and simplified.
        ## Set colors and legends.
        colors <- topo.colors(100)
        legends <- c("Raw data", "Fitted data", "Residuals",
                     "Fitted Spatial Trend",
                     ifelse(what == "fixed", "Genotypic BLUEs",
                            "Genotypic BLUPs"), "Histogram")
        ## Compute range of values in response + fitted data so same scale
        ## can be used over plots.
        zlim <- range(c(plotDat$response, plotDat$fitted), na.rm = TRUE)
        plots$p1 <- fieldPlot(plotDat = plotDat, fillVar = "response",
                              title = legends[1], colors = colors, zlim = zlim)
        plots$p2 <- fieldPlot(plotDat = plotDat, fillVar = "fitted",
                              title = legends[2], colors = colors, zlim = zlim)
        plots$p3 <- fieldPlot(plotDat = plotDat, fillVar = "residuals",
                              title = legends[3], colors = colors)
        if (x[[trial]]$engine == "SpATS") {
          ## Get tickmarks from first plot to be used as ticks.
          ## Spatial plot tends to use different tickmarks by default.
          xTicks <-
            ggplot2::ggplot_build(plots[[1]])$layout$panel_params[[1]]$x$breaks
          if (spaTrend == "raw") {
            plots$p4 <- fieldPlot(plotDat = plotDatSpat, fillVar = "value",
                                  title = legends[4], colors = colors,
                                  xTicks = xTicks)
          } else {
            colorsSp <- colorRampPalette(c("red", "yellow", "blue"),
                                         space = "Lab")(100)
            phenoMean <- mean(plotDat[["response"]], na.rm = TRUE)
            plotDatSpat[["value"]] <- plotDatSpat[["value"]] / phenoMean
            zlim <- c(-1, 1) * max(c(abs(plotDatSpat[["value"]]), 0.1))
            plots$p4 <- fieldPlot(plotDat = plotDatSpat, fillVar = "value",
                                  title = legends[4], zlim = zlim,
                                  colors = colorsSp, spaTrend = "percentage",
                                  xTicks = xTicks)
          }
        }
        plots$p5 <- fieldPlot(plotDat = plotDat, fillVar = "pred",
                              title = legends[5], colors = colors)
        plots$p6 <-
          ggplot2::ggplot(data = plotDat) +
          ggplot2::geom_histogram(ggplot2::aes(x = .data[["pred"]]),
                                  fill = "white", color = "black", bins = 10,
                                  boundary = 0, na.rm = TRUE) +
          ## Remove empty space between ticks and actual plot.
          ggplot2::scale_x_continuous(expand = c(0, 0)) +
          ggplot2::scale_y_continuous(expand = c(0, 0)) +
          ## No background. Center and resize title. Resize axis labels.
          ggplot2::theme(panel.background = ggplot2::element_blank(),
                         plot.title = ggplot2::element_text(hjust = 0.5,
                                                            size = 10),
                         axis.title = ggplot2::element_text(size = 9)) +
          ggplot2::labs(y = "Frequency", x = legends[5]) +
          ggplot2::ggtitle(legends[6])
        if (output) {
          ## do.call is needed since grid.arrange doesn't accept lists as input.
          do.call(gridExtra::grid.arrange,
                  args = c(Filter(f = Negate(f = is.null), x = plots),
                           list(ncol = outCols, top = plotTitle)))
        }
        pTr[[trait]] <- plots
      }# end spatial.
    }# end for traits.
    p[[trial]] <- pTr
  }# end for trials.
  invisible(p)
}

#' Helper function for creating field plots.
#'
#' @noRd
#' @keywords internal
fieldPlot <- function(plotDat,
                      fillVar,
                      title,
                      colors,
                      zlim = range(plotDat[fillVar]),
                      xTicks = ggplot2::waiver(),
                      spaTrend = "raw",
                      ...) {
  p <- ggplot2::ggplot(data = plotDat,
                       ggplot2::aes(x = .data[["colCoord"]],
                                    y = .data[["rowCoord"]],
                                    fill = .data[[fillVar]])) +
    ggplot2::geom_tile(na.rm = TRUE) +
    ## Remove empty space between ticks and actual plot.
    ggplot2::scale_x_continuous(expand = c(0, 0), breaks = xTicks) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ## No background. Center and resize title. Resize axis labels.
    ## Remove legend title and resize legend entries.
    ggplot2::theme(panel.background = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(hjust = 0.5, size = 10),
                   axis.title = ggplot2::element_text(size = 9),
                   legend.text = ggplot2::element_text(size = 8)) +
    ggplot2::ggtitle(title)
  ## Adjust plot colors.
  if (spaTrend == "raw") {
    p <- p + ggplot2::scale_fill_gradientn(limits = zlim, colors = colors,
                                           name = NULL, na.value = "white")
  } else if (spaTrend == "percentage") {
    p <- p + ggplot2::scale_fill_gradientn(limits = zlim, colors = colors,
                                           name = NULL,
                                           labels = scales::percent,
                                           breaks = seq(zlim[1], zlim[2],
                                                        length.out = 5))
  }
  return(p)
}

#' Report method for class STA
#'
#' pdf reports will be created containing a summary of the results of the
#' fitted model(s). For all selected trails and traits a separate pdf file will
#' be generated. Also a .tex file and a folder containing figures will be
#' created for each report to enable easy modifying of the report.
#'
#' This function uses pdflatex to create a pdf report. For it to run correctly
#' an installation of LaTeX is required. Checking for this is done with
#' Sys.which("pdflatex").
#'
#' @param x An object of class STA.
#' @param ... Further arguments passed to the report function.
#' @param trials A character vector indicating the trials to report. If
#' \code{trials = NULL}, all trials are reported.
#' @param traits A character vector indicating the traits to report. If
#' \code{traits = NULL}, all traits are reported.
#' @param descending Should the trait be ordered in descending order? Set to
#' \code{FALSE} if low values of the trait indicate better performance.
#' @param outfile A character string, the name and location of the output .pdf
#' and .tex file for the report. If \code{NULL}, a report with a default name
#' will be created in the current working directory. Trial, trait and
#' the type of model (genotype fixed or random) will be concatenated to the
#' name of the output file.\cr
#' Both knitr and pdflatex don't work well with spaces in file paths and these
#' are therefore disallowed. Relative paths are possible though.
#' @param what A character vector indicating whether the fitted model with
#' genotype as fixed or genotype as random factor should be reported. By
#' default all fitted models in the STA object are reported.
#'
#' @return A pdf report and the .tex file and figures folder that can be used
#' to recreate the report.
#'
#' @examples
#' ## Fit model using lme4.
#' modLme <- fitTD(TD = TDHeat05,
#'                 design = "ibd",
#'                 traits = "yield",
#'                 engine = "lme4")
#'
#' ## Create a pdf report summarizing the results for the model with genotype
#' ## as fixed factor.
#' \donttest{
#' report(modLme,
#'        outfile = tempfile(fileext = ".pdf"),
#'        what = "fixed")
#' }
#'
#' ## Create two pdf report summarizing the results for the model with genotype
#' ## as fixed factor and for the model with genotype as random factor. Order
#' ## the results in ascending order.
#' \donttest{
#' report(modLme,
#'        outfile = tempfile(fileext = ".pdf"),
#'        descending = FALSE)
#' }
#'
#' @family functions for STA objects
#'
#' @export
report.STA <- function(x,
                       ...,
                       trials = NULL,
                       traits = NULL,
                       descending = TRUE,
                       outfile = NULL,
                       what = c("fixed", "random")) {
  ## Checks.
  if (nchar(Sys.which("pdflatex")) == 0) {
    stop("An installation of LaTeX is required to create a pdf report.\n")
  }
  if (!is.null(outfile) && (!is.character(outfile) || length(outfile) > 1 ||
                            tools::file_ext(outfile) != "pdf")) {
    stop("Invalid output filename provided.\n")
  }
  trials <- chkTrials(trials, x)
  chkChar(traits)
  ## Generate a single timestamp for all files.
  timeStamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  what <- match.arg(what, several.ok = TRUE)
  for (trial in trials) {
    for (whatTr in what) {
      outTr <- gsub(pattern = " ", replacement = "_", x = trial)
      modTr <- x[trial]
      whatMod <- c("mFix", "mRand")[whatTr == c("fixed", "random")]
      if (is.null(modTr[[trial]][[whatMod]])) {
        warning("Model with genotype ", whatTr, " not available for trial ",
                trial, ".\nReport skipped.")
        next
      }
      ## Set other model to NULL for easier reporting inside Rnw.
      ## Doing so assures always only one fitted model is available.
      modTr[[trial]][[setdiff(c("mFix", "mRand"), whatMod)]] <- NULL
      ## Check that traits are available for current trial.
      traitsTr <- chkTraits(traits, trial, x[[trial]], err = FALSE)
      if (length(traitsTr) == 0) {
        next
      }
      for (trait in traitsTr) {
        outTrt <- gsub(pattern = " ", replacement = "_", x = trait)
        ## report name has to be adapted.
        if (!is.null(outfile)) {
          ## Add trial and trait info before file extension.
          outExt <- tools::file_ext(outfile)
          outLen <- nchar(outfile)
          outfileTr <- paste0(substring(text = outfile, first = 1,
                                        last = outLen - nchar(outExt) - 1),
                              "_", outTr, "_", outTrt, "_", whatTr, ".", outExt)
        } else {
          outfileTr <- paste0("./modelReport_" , trial, "_", outTr, "_", whatTr,
                              "_", timeStamp, ".pdf")
        }
        createReport(x = modTr, reportName = "modelReport.Rnw",
                     reportPackage = "statgenSTA", outfile = outfileTr, ...,
                     trial = trial, trait = trait, descending = descending)
      } # End loop over traits.
    } # End loop over what.
  } # End loop over trials.
}

#' Convert STA to Cross
#'
#' Convert an STA object to a cross object from package qtl. Genotypic
#' information should be available in a .csv file.\cr
#' The only way to create an object of class cross is by importing both the
#' phenotypic and the genotypic data from external files. Therefore the
#' phenotypic data, either the BLUEs or the BLUPs from the fitted model are
#' first written to a temporary file. The genotypic data has to be available in
#' a .csv file in the correct format as well, see \code{genoFile} for a
#' description of this format. These phenotypic and genotypic files are then
#' imported into a cross object using the read.cross function in the qtl
#' package.
#'
#' @param STA An object of class \code{STA}.
#' @param trial A character string indicating the trial to be exported. If
#' \code{NULL} and \code{STA} contains only one trial, that trial is exported.
#' @param traits A character string containing the traits to be exported. If
#' \code{NULL}, all traits for the selected trial are exported.
#' @param what A character string containing the statistics to be exported as
#' phenotype in the cross object. This can be either \code{BLUEs} or
#' \code{BLUPs}.
#' @param genoFile A character string indicating a filename containing
#' phenotypic data. The data should be in the format required by the
#' qtl package. The first column should contain the individuals, starting
#' from row 4. The following columns contain markers with in the second and
#' third row the chromosome and position on the chromosome and in the
#' following rows the genotypes.
#' @param genotypes A character vector specifying the genotype codes
#' corresponding to AA, AB, BB, not BB and not AA.
#' @param ... Further arguments to be passed to the read.cross function.
#' See \code{\link[qtl]{read.cross}}.
#'
#' @seealso \code{\link[qtl]{read.cross}}
#'
#' @examples
#' ## Fit model using SpATS.
#' modSp <- fitTD(TD = TDHeat05,
#'                design = "res.rowcol",
#'                traits = "yield",
#'                what = "fixed")
#'
#' ## Create cross object with BLUEs from modSp using genotypic information
#' ## from markers.csv in the package.
#' cross <- STAtoCross(modSp,
#'                     genoFile = system.file("extdata", "markers.csv",
#'                                            package = "statgenSTA"))
#'
#' @family functions for STA objects
#'
#' @export
STAtoCross <- function(STA,
                       trial = NULL,
                       traits = NULL,
                       what = c("BLUEs", "BLUPs"),
                       genoFile,
                       genotypes = c("A", "H", "B", "D", "C"),
                       ...) {
  ## Checks
  if (!inherits(STA, "STA")) {
    stop("STA is not a valid object of class STA.\n")
  }
  if (is.null(trial) && length(STA) > 1) {
    stop("No trial provided but multiple trials found in STA object.\n")
  }
  if (!is.null(trial) && (!is.character(trial) || length(trial) > 1 ||
                          !trial %in% names(STA))) {
    stop("trial has to be a single character string defining a trial in STA.\n")
  }
  if (is.null(trial)) {
    trial <- names(STA)
  }
  traits <- chkTraits(traits, trial, STA[[trial]])
  what <- match.arg(what)
  if (!is.character(genoFile) || length(genoFile) > 1 || !file.exists(genoFile)) {
    stop("genoFile is not a valid filename.\n")
  }
  ## Extract predictions from the model.
  pred <- extractSTA(STA, traits = traits, what = what)
  ## Remove trial column.
  pred <- pred[, colnames(pred) != "trial"]
  ## Rename first column to match first column in genoFile.
  colnames(pred)[1] <- colnames(utils::read.csv(genoFile, nrow = 1))[1]
  ## Write predictions to temporary file.
  tmp <- tempfile()
  utils::write.csv(pred, file = tmp, row.names = FALSE)
  ## Read cross from temporary file and supplied genoFile.
  capture.output(cross <- qtl::read.cross(format = "csvs", phefile = tmp,
                                          genfile = genoFile,
                                          genotypes = genotypes, ...),
                 file = tempfile())
  unlink(tmp)
  return(cross)
}

#' Convert STA to TD
#'
#' Convert an STA object to a TD object.\cr
#' To be able to use the output of a single trial analysis in
#' Genotype-by-Environment (GxE) analysis the output first needs to be converted
#' back to an TD object. This function does exactly that. It extracts BLUEs,
#' BLUPs and their standard errors from the STA object and creates a new TD
#' object using these. Also a column "wt" (weight) may also be added. Weights
#' are then calculated as 1/(SE BLUEs) ^ 2.
#'
#' Trial information for the trials in the STA object will be copied from the
#' original TD object on which the modeling was done.
#'
#' @param STA An object of class \code{STA}.
#' @param what A character string containing the statistics to be included as
#' traits in the TD object. Multiple statistics can be included in which case
#' they will appear as \code{statistic_trait} in the output
#' @param traits A character string containing the traits to be included in the
#' TD object. If \code{NULL}, all traits are exported.
#' @param keep Columns from the TD object used as input for the STA model to
#' be copied to the output. see \code{\link{extractSTA}} for possible columns to
#' copy. If if it is available in \code{TD}, the column \code{trial} will always
#' be copied.
#' @param addWt Should a column wt be added to the output? If \code{TRUE}
#' weight is calculated as 1/(SE BLUEs) ^ 2. If multiple traits are included in
#' the output, multiple weight columns will be added, 1 for each trait. These
#' will be named \code{wt_trait}.
#'
#' @examples
#' ## Fit model using SpATS.
#' modSp <- fitTD(TD = TDHeat05,
#'                design = "res.rowcol",
#'                traits = "yield",
#'                what = "fixed")
#'
#' ## Create TD object from the fitted model with BLUEs and standard errors.
#' TDRes <- STAtoTD(modSp,
#'                  what = c("BLUEs", "seBLUEs"))
#'
#' ## Add a weight column in the output.
#' TDResWt <- STAtoTD(modSp,
#'                    what = c("BLUEs", "seBLUEs"),
#'                    addWt = TRUE)
#'
#' @family functions for STA objects
#'
#' @export
STAtoTD <- function(STA,
                    what = c("BLUEs", "seBLUEs", "BLUPs", "seBLUPs"),
                    traits = NULL,
                    keep = NULL,
                    addWt = FALSE) {
  ## Checks.
  if (missing(STA) || !inherits(STA, "STA")) {
    stop("STA is not a valid object of class STA.\n")
  }
  if (!is.null(traits) && (!is.character(traits) ||
                           !all(traits %in% colnames(STA[[1]]$TD[[1]])))) {
    stop("traits has to be a character vector defining columns in TD.\n")
  }
  what <- match.arg(what, several.ok = TRUE)
  if (any(c("BLUEs", "seBLUEs") %in% what) && is.null(STA[[1]]$mFix)) {
    warning("BLUEs and seBLUEs can only be extracted if a model with ",
            "genotype fixed is fitted\nRemoving them from what", call. = FALSE)
    what <- setdiff(what, c("BLUEs", "seBLUEs"))
  }
  if (any(c("BLUPs", "seBLUPs") %in% what) && is.null(STA[[1]]$mRand)) {
    warning("BLUPs and seBLUPs can only be extracted if a model with ",
            "genotype random is fitted\nRemoving them from what", call. = FALSE)
    what <- setdiff(what , c("BLUPs", "seBLUPs"))
  }
  if (length(what) == 0) {
    stop("No statistics left to extract.")
  }
  if (addWt && is.null(STA[[1]]$mFix)) {
    warning("Weights can only be added if a model with genotype fixed is ",
            "fitted.\naddWt set to FALSE", call. = FALSE)
    addWt <- FALSE
  }
  if (addWt && !"seBLUEs" %in% what) {
    warning("Weights can only be added together with seBLUEs.\n",
            "seBLUEs added to what", call. = FALSE)
    what <- c(what, "seBLUEs")
  }
  if (is.null(traits)) {
    traits <- STA[[1]]$traits
  }
  if (!"trial" %in% keep && hasName(x = STA[[1]]$TD[[1]], name = "trial")) {
    keep <- c(keep, "trial")
  }
  ## Create a list of data.frames with all statistics per trial.
  predTrTot <- lapply(X = names(STA), FUN = function(trial) {
    ## Extract predictions from the model.
    predLst <- lapply(X = traits, FUN = function(trait) {
      extractSTA(STA, trials = trial, traits = trait, what = what, keep = keep)
    })
    if (length(what) > 1) {
      predLst <- unlist(predLst, recursive = FALSE)
    }
    if (length(what) + addWt > 1) {
      ## Rename columns if more than one column per trait will appear in the
      ## output. Add the name of the statistic as prefix to the traits.
      predLst <- lapply(X = predLst, FUN = function(pred) {
        for (ext in names(pred)) {
          colNames <- colnames(pred[[ext]])
          colnames(pred[[ext]])[colNames %in% traits] <-
            paste0(ext, "_", colNames[colNames %in% traits])
        }
        return(pred)
      })
    }
    ## Merge all statistics together. Because of the renaming above there is
    ## never a problem with duplicate columns and merging is done on all other
    ## columns than the traits.
    if (length(what) > 1) {
      predTr <- Reduce(f = merge, x = unlist(predLst, recursive = FALSE))
    } else {
      predTr <- Reduce(f = merge, x = predLst)
    }
    traitsTr <- traits[!sapply(X = predLst, FUN = is.null)]
    if (addWt && "seBLUEs" %in% what) {
      ## Add a wt column.
      for (trait in traitsTr) {
        ## Naming is based on all traits since different trials may have
        ## different numbers of traits but we want to keep the naming pattern
        ## consistent.
        wtName <- ifelse(length(traits) == 1, "wt", paste0("wt_", trait))
        predTr[[wtName]] <- 1 / predTr[[paste0("seBLUEs_", trait)]] ^ 2
      }
    }
    return(predTr)
  })
  ## Remove NULL data.frames from predTrTot.
  predTrTot <- Filter(f = Negate(f = is.null), x = predTrTot)
  if (length(predTrTot) == 0) {
    ## Only NULL data.frames.
    stop("No valid data available.\n")
  }
  ## Add data.frame one-by-one to create a full TD dataset.
  predTD <- Reduce(f = addTD, x = predTrTot[-1],
                   init = createTD(data = predTrTot[[1]]))
  return(predTD)
}
