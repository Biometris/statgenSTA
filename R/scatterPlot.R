#' Helper function for creating a scatter plot
#'
#' Helper function for creating a scatter plot for an object of class TD.
#'
#' @importFrom grDevices topo.colors
#' @noRd
#' @keywords internal
scatterPlot <- function(x,
                        trials,
                        traits,
                        title,
                        output,
                        ...) {
  dotArgs <- list(...)
  colorGenoBy <- dotArgs$colorGenoBy
  colGeno <- dotArgs$colGeno
  colorTrialBy <- dotArgs$colorTrialBy
  colTrial <- dotArgs$colTrial
  if (length(trials) == 1) {
    stop("At least two trials are requiered for a scatter plot.\n")
  }
  chkChar(traits, null = FALSE)
  ## Checks for colorGenoBy.
  if (!is.null(colorGenoBy)) {
    chkChar(colorGenoBy, len = 1, null = FALSE)
    if (!all(vapply(X = x, FUN = hasName, FUN.VALUE = logical(1),
                    name = colorGenoBy))) {
      stop("colorGenoBy should be a column in TD.\n")
    }
    chkChar(colGeno)
  }
  ## Checks for colorTrialBy.
  if (!is.null(colorTrialBy)) {
    chkChar(colorTrialBy, len = 1, null = FALSE)
    if (!all(vapply(X = x, FUN = hasName, FUN.VALUE = logical(1),
                    name = colorTrialBy))) {
      stop("colorTrialBy should be a column in TD.\n")
    }
    chkChar(colTrial)
  }
  ## Checks for trialOrder.
  trialOrder <- dotArgs$trialOrder
  if (!is.null(trialOrder) &&
      (!all(trialOrder %in% trials) || !all(trials %in% trialOrder))) {
    stop("trials and trialOrder should contain exactly the same trials.\n")
  }
  ## Checks for addCorr.
  addCorr <- dotArgs$addCorr
  if (!is.null(addCorr)) {
    addCorr <- match.arg(addCorr, choices = c("tl", "bl", "tr", "br"))
  }
  ## Create list of colors for histograms.
  ## Outside trait loop to assure identical coloring of trials.
  if (!is.null(colorTrialBy)) {
    colorTrialDat <- unique(do.call(rbind, lapply(X = x, FUN = `[`,
                                                  c("trial", colorTrialBy))))
  } else {
    colorTrialDat <- unique(do.call(rbind, lapply(X = x, FUN = `[`, "trial")))
    colorTrialDat[[".colorTrialBy"]] <- factor(1)
    colorTrialBy <- ".colorTrialBy"
  }
  if (!is.factor(colorTrialDat[[2]])) {
    colorTrialDat[[2]] <- as.factor(colorTrialDat[[2]])
  }
  ## droplevels is needed to assure number of colors matches actual number of
  ## trials in data.
  colorTrialDat <- droplevels(colorTrialDat)
  colorTrialGroups <- levels(colorTrialDat[[2]])
  ## Get the number of colors needed for coloring the trials.
  nColTrial <- length(colorTrialGroups)
  colTrial <- defineTrialColors(colors = colTrial, n = nColTrial,
                                default = "grey50")
  colorTrialColors <- setNames(colTrial, colorTrialGroups)
  histCols <- setNames(colorTrialColors[match(colorTrialDat[[2]],
                                              colorTrialGroups)],
                       make.names(paste0("t", colorTrialDat[[1]])))
  p <- setNames(vector(mode = "list", length = length(traits)), traits)
  for (trait in traits) {
    ## Create plot title.
    if (is.null(title)) {
      plotTitle <- paste("Scatterplots of trials for", trait)
    } else {
      plotTitle <- title
    }
    ## Create a single data.frame from x with only columns genotype, trial
    ## and trait and optionally colorGenoBy.
    ## trials where trait is not measured/available are removed by setting
    ## them to NULL.
    plotDat <- Reduce(f = rbind, x = lapply(X = x, FUN = function(trial) {
      if (!hasName(x = trial, name = trait) || all(is.na(trial[[trait]]))) {
        NULL
      } else {
        trial[c("genotype", "trial", trait, colorGenoBy)]
      }
    }))
    if (is.null(plotDat) || nlevels(plotDat[["trial"]]) < 2) {
      warning(trait, " has no valid observations for a least two trials.\n",
              "Plot skipped.\n", call. = FALSE)
      next
    }
    plotDat <- droplevels(plotDat)
    ## colorGenoBy is ignored in plot if it is not a factor.
    ## Therefore convert it to factor if it wasn't already.
    if (!is.null(colorGenoBy) && !is.factor(plotDat[[colorGenoBy]])) {
      plotDat[[colorGenoBy]] <- as.factor(plotDat[[colorGenoBy]])
    }
    if (!is.null(trialOrder)) {
      ## Reorder trials.
      ## First restrict reordering to trials left after removing NA trials.
      trialOrderTr <- trialOrder[trialOrder %in% levels(plotDat[["trial"]])]
      plotDat[["trial"]] <- factor(plotDat[["trial"]], levels = trialOrderTr)
    }
    ## Create table with values trait per genotype per trial.
    ## If TD already contains BLUEs/BLUPs taking means doesn't do anything
    ## but it is needed for raw data where there can be replicates.
    plotTab <- as.data.frame(tapply(plotDat[[trait]],
                                    INDEX = list(plotDat[["genotype"]],
                                                 plotDat[["trial"]]),
                                    FUN = mean, na.rm = TRUE))
    ## Get range of values for trait for determining position of annotation.
    plotRange <- range(unlist(plotTab), na.rm = TRUE)
    if (!is.null(addCorr)) {
      ## Compute correlations for annotation.
      corMat <- cor(plotTab, use = "pairwise.complete.obs")
      ## Convert corMat to data.frame to prevent crash when reshaping.
      corMat <- as.data.frame(corMat)
      ## Convert correlation matrix to long format for ggplot.
      meltedCorMat <- reshape(corMat, direction = "long",
                              varying = list(genotype = colnames(corMat)),
                              ids = rownames(corMat), idvar = "trial1",
                              times = colnames(corMat), timevar = "trial2",
                              v.names = "cor")
      ## Reshape converts trial columns to character.
      ## This gives problems with plotting, so reconvert them to factor.
      meltedCorMat[["trial1"]] <- factor(meltedCorMat[["trial1"]],
                                         levels = rownames(corMat))
      meltedCorMat[["trial2"]] <- factor(meltedCorMat[["trial2"]],
                                         levels = rownames(corMat))
      ## Set position for annotation.
      minPos <- plotRange[1] + 0.03 * plotRange[1] * sign(plotRange[1])
      maxPos <- plotRange[2] - 0.03 * plotRange[2] * sign(plotRange[2])
      meltedCorMat[["x"]] <- ifelse(addCorr %in% c("tl", "bl"), minPos, maxPos)
      meltedCorMat[["y"]] <- ifelse(addCorr %in% c("br", "bl"), minPos, maxPos)
      colnames(meltedCorMat)[colnames(meltedCorMat) == "trial1"] <- "trial.x"
      colnames(meltedCorMat)[colnames(meltedCorMat) == "trial2"] <- "trial.y"
    }
    ## Create plots containing histograms.
    ## Used further on to replace diagonal plot in plot matrix.
    histVars <- make.names(paste0("t", colnames(plotTab)))
    histPlots <- lapply(X = histVars, FUN = function(trial) {
      colnames(plotTab) <- make.names(paste0("t", colnames(plotTab)))
      binWidth <- diff(range(plotTab[[trial]], na.rm = TRUE)) / 10
      ggplot2::ggplot(plotTab,
                      ggplot2::aes(x = .data[[trial]],
                                   y = ggplot2::after_stat(count / sum(count)))) +
        ggplot2::geom_histogram(na.rm = TRUE, binwidth = binWidth,
                                boundary = 0, fill = histCols[trial],
                                color = histCols[trial]) +
        ggplot2::scale_x_continuous(limits = range(plotTab, na.rm = TRUE)) +
        ggplot2::theme(panel.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = "black",
                                                            fill = NA))
    })
    ## Y-axis should be the same for all histograms.
    ## Build histograms and extract axis information.
    yMax <- max(vapply(X = histPlots, FUN = function(hp) {
      max(ggplot2::ggplot_build(hp)$data[[1]][["ymax"]])
    }, FUN.VALUE = numeric(1)))
    ## Add scaling for y-axis to histograms
    ## Convert to grobs for easier use later on.
    histGrobs <- lapply(X = histPlots, FUN = function(hp) {
      hp <- hp + ggplot2::scale_y_continuous(expand = c(0, 0, 0, 0.05),
                                             labels = function(x) {
                                               paste0(100 * x, "%")
                                             }, limits = c(0, yMax))
      ggplot2::ggplotGrob(hp)
    })
    ## Reshape to get data in format suitable for ggplot.
    plotTab <- reshape(plotTab, direction = "long",
                       varying = colnames(plotTab),
                       timevar = "trial", times = colnames(plotTab),
                       idvar = "genotype", ids = rownames(plotTab),
                       v.names = trait)
    ## Reshaping loses factor levels. Reset them for trial so trial order
    ## matches that from histograms.
    plotTab[["trial"]] <- factor(plotTab[["trial"]],
                                 levels = levels(plotDat[["trial"]]))
    if (!is.null(colorGenoBy)) {
      plotTab <- merge(plotTab, unique(plotDat[c("genotype", colorGenoBy)]))
    } else {
      plotTab[[".colorGenoBy"]] <- factor(1)
      colorGenoBy <- ".colorGenoBy"
    }
    ## Merge to itself to create a full data set.
    plotTab <- merge(plotTab, plotTab, by = c("genotype", colorGenoBy))
    if (colorTrialBy != ".colorTrialBy") {
      legendDat <- merge(colorTrialDat, colorTrialColors, by.x = colorTrialBy,
                         by.y = "row.names")
      plotTab <- merge(plotTab, legendDat, by.x = "trial.y", by.y = "trial")
      colnames(plotTab)[colnames(plotTab) == colnames(legendDat)[2]] <-
        colorTrialBy
    }
    nColGeno <- nlevels(plotTab[[colorGenoBy]])
    if (length(colGeno) == 0) {
      ## Defaults to "darkgrey" for one color for genotypes.
      ## For more than one colors from statgen.genoColors are used.
      ## Fall back to topo.colors if number of colors in option is too small.
      if (nColGeno == 1) {
        colGeno <- "darkgrey"
      } else if (length(getOption("statgen.genoColors")) >= nColGeno) {
        colGeno <- getOption("statgen.genoColors")[seq_len(nColGeno)]
      } else {
        colGeno <- topo.colors(n = nColGeno, alpha = NULL)
      }
    } else {
      nColGenoArg <- length(colGeno)
      if (nColGenoArg != nColGeno) {
        stop("Number of colors provided doesn't match number of genotype groups:",
             "\n", nColGenoArg, " colors provided, ", nColGeno,
             " groups in data.\n")
      }
    }
    ## Create a facet plot containing only scatter plots.
    scatterBase <-
      ggplot2::ggplot(data = plotTab,
                      ggplot2::aes(x = .data[[paste0(trait, ".x")]],
                                   y = .data[[paste0(trait, ".y")]],
                                   color = .data[[colorGenoBy]])) +
      ggplot2::geom_point(na.rm = TRUE, shape = 1,
                          show.legend = colorGenoBy != ".colorGenoBy") +
      ggplot2::scale_color_manual(values = colGeno) +
      ggplot2::scale_x_continuous(breaks = scales::breaks_extended(n = 3)) +
      ggplot2::scale_y_continuous(breaks = scales::breaks_extended(n = 3)) +
      ggplot2::facet_grid(facets = c("trial.y", "trial.x")) +
      ggplot2::labs(title = plotTitle, x = "", y = "") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     aspect.ratio = 1,
                     panel.background = ggplot2::element_rect(fill = "white"),
                     panel.grid = ggplot2::element_blank(),
                     panel.border = ggplot2::element_rect(color = "black",
                                                          fill = NA))
    if (colorTrialBy != ".colorTrialBy") {
      scatterBase <- scatterBase +
        ggplot2::geom_point(ggplot2::aes(fill = .data[[colorTrialBy]]),
                            color = NA, na.rm = TRUE) +
        ggplot2::scale_fill_discrete(labels = names(colorTrialColors)) +
        ggplot2::guides(fill = ggplot2::guide_legend(override.aes =
                                                       list(color = colorTrialColors)))
    }
    if (!is.null(addCorr)) {
      ## Add correlation annotated in the corner of the plot.
      scatterBase <- scatterBase +
        ggplot2::geom_text(data = meltedCorMat,
                           ggplot2::aes(x = .data[["x"]], y = .data[["y"]],
                                        label = paste("rho ==",
                                                      round(.data[["cor"]], 2))),
                           color = "red", hjust = "inward", vjust = "inward",
                           parse = TRUE, inherit.aes = FALSE)
    }
    ## Convert to grobs to enable modifying.
    scatterGrob <- ggplot2::ggplotGrob(scatterBase)
    ## Get grobs containing plot panels.
    panels <- scatterGrob$layout$name[grepl(pattern = "panel",
                                            x = scatterGrob$layout$name)]
    splitPanels <- strsplit(x = panels, split = "-")
    ## Upper right panels need to be set to zeroGrob to make them empty.
    nullPanels <- panels[vapply(X = splitPanels, FUN = function(pan) {
      as.numeric(pan[2]) < as.numeric(pan[3])
    }, FUN.VALUE = logical(1))]
    for (np in nullPanels) {
      scatterGrob$grobs[[which(scatterGrob$layout$name == np)]] <-
        ggplot2::zeroGrob()
    }
    ## Set diagonal panels to histograms calculated before.
    histPanels <- panels[vapply(X = splitPanels, FUN = function(pan) {
      as.numeric(pan[2]) == as.numeric(pan[3])
    }, FUN.VALUE = logical(1))]
    for (i in seq_along(histPanels)) {
      hg <- histGrobs[[i]]
      ## Replace grob by panel grob from histogram.
      scatterGrob$grobs[[which(scatterGrob$layout$name == histPanels[i])]] <-
        hg$grobs[[which(hg$layout$name == "panel")]]
    }
    ## Replace top left axis in the matrix by y axis from the first histogram.
    scatterGrob$grobs[[which(scatterGrob$layout$name == "axis-l-1")]] <-
      histGrobs[[1]]$grobs[[which(histGrobs[[1]]$layout$name == "axis-l")]]
    p[[trait]] <- scatterGrob
    if (output) {
      grid::grid.newpage()
      grid::grid.draw(scatterGrob)
    }
  }
  invisible(p)
}
