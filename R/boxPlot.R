#' Helper function for creating a map plot
#'
#' Helper function for creating a map plot for an object of class TD.
#'
#' @keywords internal
boxPlot <- function(x,
                    trials,
                    traits,
                    title,
                    output,
                    ...) {
  dotArgs <- list(...)
  chkChar(traits, null = FALSE)
  groupBy <- dotArgs$groupBy
  ## Checks for groupBy.
  if (!is.null(groupBy)) {
    chkChar(groupBy, len = 1, null = FALSE)
  }
  if (!is.null(groupBy) &&
      !all(vapply(X = x, FUN = hasName, FUN.VALUE = logical(1),
                  name = groupBy))) {
    stop("groupBy should be a column in TD.\n")
  }
  ## Checks for colorTrialBy.
  colorTrialBy <- dotArgs$colorTrialBy
  if (!is.null(colorTrialBy)) {
    chkChar(colorTrialBy, len = 1, null = FALSE)
  }
  if (!is.null(colorTrialBy) &&
      !all(vapply(X = x, FUN = hasName, FUN.VALUE = logical(1),
                  name = colorTrialBy))) {
    stop("colorTrialBy should be a column in TD.\n")
  }
  colTrial <- dotArgs$colTrial
  chkChar(colTrial)
  ## Checks for orderBy.
  orderBy <- dotArgs$orderBy
  if (!is.null(orderBy)) {
    orderBy <- match.arg(orderBy,
                         choices = c("alphabetic", "ascending", "descending"))
  } else {
    orderBy <- "alphabetic"
  }
  ## Create vector for outputs.
  p <- setNames(vector(mode = "list", length = length(traits)), traits)
  ## Save value of colorTrialBy
  colorTrialByIn <- colorTrialBy
  for (trait in traits) {
    ## Create a single data.frame from x with only columns trial, trait and
    ## genotype.
    ## Genotype is needed to be able to display hovering info (in GUI).
    ## trials where trait is not measured/available are removed by setting
    ## them to NULL.
    xVar <- if (is.null(groupBy)) "trial" else groupBy
    plotDat <- Reduce(f = rbind, x = lapply(X = x[trials], function(trial) {
      if (!hasName(x = trial, name = trait)) {
        NULL
      } else {
        if (!hasName(x = trial, name = "trial")) {
          trial[["trial"]] <- names(x)
        }
        trial[c(trait, "genotype", xVar,
                if (!is.null(colorTrialBy)) colorTrialBy)]
      }
    }))
    ## If trait is not measured in any of the trials skip plotting for
    ## that trait.
    if (is.null(plotDat)) {
      warning(trait, " isn't a column in any of the trials.\n",
              "Plot skipped.\n", call. = FALSE)
      next
    }
    ## If colorTrailBy is not user specified add a default value to avoid
    ## splitting cases later on.
    if (is.null(colorTrialBy)) {
      plotDat[[".colorTrialBy"]] <- factor(1)
      colorTrialBy <- ".colorTrialBy"
    }
    ## colorTrialBy is ignored in plot if it is not a factor.
    ## Therefore convert it to factor if it wasn't already.
    if (!is.null(colorTrialBy) && !is.factor(plotDat[colorTrialBy])) {
      plotDat[colorTrialBy] <- factor(plotDat[[colorTrialBy]])
    }
    ## Get the number of colors needed for coloring the trials.
    nColTrial <- nlevels(plotDat[[colorTrialBy]])
    colTrial <- defineTrialColors(colors = colTrial, n = nColTrial,
                                  default = "darkgrey")
    if (orderBy != "alphabetic") {
      ## Reorder levels in trial so plotting is done according to orderBy.
      ## do.call needed since order doesn't accept a vector as input.
      levNw <- reorder(x = plotDat[[xVar]], X = plotDat[[trait]],
                       FUN = mean, na.rm = TRUE, order = TRUE)
      if (orderBy == "ascending") {
        plotDat[xVar] <- factor(plotDat[[xVar]], levels = levels(levNw))
      } else {
        plotDat[xVar] <- factor(plotDat[[xVar]], levels = rev(levels(levNw)))
      }
    }
    ## Define trait specific title.
    if (is.null(title)) {
      plotTitle <- paste("Boxplot for", trait)
    } else {
      plotTitle <- title
    }
    ## Create boxplot.
    pTr <- ggplot2::ggplot(plotDat,
                           ggplot2::aes_string(x = paste0("`", xVar, "`"),
                                               y = paste0("`", trait, "`"),
                                               fill = colorTrialBy)) +
      ggplot2::geom_boxplot(na.rm = TRUE,
                            show.legend = colorTrialBy != ".colorTrialBy") +
      ggplot2::scale_fill_manual(values = colTrial) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     axis.text.x = ggplot2::element_text(angle = 90,
                                                         vjust = 0.5,
                                                         hjust = 1),
                     panel.background = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     panel.border = ggplot2::element_rect(color = "black",
                                                          fill = NA)) +
      ggplot2::labs(x = xVar, y = trait, title = plotTitle)
    ## Add plot to output.
    p[[trait]] <- pTr
    if (output) {
      plot(pTr)
    }
    ## Reset colorTrialBy to input value.
    colorTrialBy <- colorTrialByIn
  }
  invisible(p)
}
