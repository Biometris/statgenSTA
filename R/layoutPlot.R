#' Helper function for creating a layout plot
#'
#' Helper function for creating a layout plot for an object of class TD.
#'
#' @keywords internal
layoutPlot <- function(x,
                       trials,
                       traits,
                       title,
                       output,
                       ...) {
  dotArgs <- list(...)
  chkChar(traits, len = 1, null = TRUE)
  showGeno <- isTRUE(dotArgs$showGeno)
  highlight <- dotArgs$highlight
  colorSubBlock <- isTRUE(dotArgs$colorSubBlock)
  if (!is.null(highlight)) {
    chkChar(highlight, null = FALSE)
  }
  p <- setNames(vector(mode = "list", length = length(trials)), trials)
  for (trial in trials) {
    trDat <- x[[trial]]
    if (!chkRowCol(trDat)) next
    if (length(highlight) > 0) {
      trDat[["highlight."]] <- ifelse(trDat[["genotype"]] %in% highlight,
                                      as.character(trDat[["genotype"]]), NA)
    }
    trLoc <- attr(trDat, "trLocation")
    plotRep <- hasName(x = trDat, name = "repId")
    plotSubBlock <- hasName(x = trDat, name = "subBlock")
    plotTrait <- !is.null(traits) && hasName(x = trDat, name = traits)
    ## Compute min and max for row and column coordinates.
    yMin <- min(trDat[["rowCoord"]])
    yMax <- max(trDat[["rowCoord"]])
    xMin <- min(trDat[["colCoord"]])
    xMax <- max(trDat[["colCoord"]])
    ## Create data.frame with all rows columns in field.
    ## Full missing rows/columns are included.
    ## If not geom_tile just fills the empty columns by expanding the
    ## neighboring columns (or rows).
    fullGrid <- expand.grid(colCoord = xMin:xMax, rowCoord = yMin:yMax)
    trDat <- merge(fullGrid, trDat, all.x = TRUE)
    trDat[!is.na(trDat[["rowId"]]), "color."] <- "grey85"
    ## Compute aspect for proper depiction of field size. If no information
    ## is available plots are assumed to be square.
    ylen <- attr(trDat, "trPlLength")
    xlen <- attr(trDat, "trPlWidth")
    if (is.null(ylen) || is.null(xlen)) {
      aspect <- length(unique(trDat[["colCoord"]])) /
        length(unique(trDat[["rowCoord"]]))
    } else {
      aspect <- ylen / xlen
    }
    ## Create data for lines between replicates.
    if (plotRep) {
      repBord <- calcPlotBorders(trDat = trDat, bordVar = "repId")
    }
    if (is.null(title)) {
      plotTitle <- trLoc
    } else {
      plotTitle <- title
    }
    ## Create base plot.
    pTr <-
      ggplot2::ggplot(data = trDat,
                      ggplot2::aes_string(x = "colCoord", y = "rowCoord")) +
      ggplot2::coord_fixed(ratio = aspect,
                           xlim = range(trDat[["colCoord"]]) + c(-0.5, 0.5),
                           ylim = range(trDat[["rowCoord"]]) + c(-0.5, 0.5),
                           clip = "off") +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ## Move ticks to edge of the plot.
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(),
                                  expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                  expand = c(0, 0)) +
      ggplot2::ggtitle(plotTitle)
    if (sum(!is.na(trDat[["highlight."]])) > 0) {
      ## Genotypes to be highlighted get a color.
      ## Everything else the NA color.
      pTr <- pTr +
        ggplot2::geom_tile(ggplot2::aes_string(fill = "highlight.",
                                               color = "color.")) +
        ggplot2::scale_color_manual(values = "grey85", na.translate = FALSE,
                                    na.value = "transparant") +
        ## Remove NA from scale.
        ggplot2::scale_fill_discrete(na.translate = FALSE) +
        ggplot2::labs(fill = "Highlighted") +
        ggplot2::guides(color = "none")
    } else if (plotSubBlock && colorSubBlock) {
      ## Color tiles by subblock.
      pTr <- pTr +
        ggplot2::geom_tile(ggplot2::aes_string(fill = "subBlock",
                                               color = "color.")) +
        ggplot2::scale_color_manual(values = "grey85", na.translate = FALSE,
                                    na.value = "transparant") +
        ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3), color = "none")
    } else if (plotTrait) {
      ## Color tiles by trait value.
      pTr <- pTr +
        ggplot2::geom_tile(ggplot2::aes_string(fill = traits),
                           color = "grey85") +
        ## Adjust plot colors.
        ggplot2::scale_fill_gradientn(colors = topo.colors(n = 100),
                                      na.value = "white")
    } else {
      ## No subblocks and no highlights so just a single fill color.
      pTr <- pTr +
        ggplot2::geom_tile(ggplot2::aes_string(color = "color."),
                           fill = "white") +
        ggplot2::scale_color_manual(values = "grey85", na.translate = FALSE,
                                    na.value = "transparant") +
        ggplot2::guides(color = "none")
    }
    ## Create data for lines between subBlocks.
    if (plotSubBlock) {
      subBlockBord <- calcPlotBorders(trDat = trDat, bordVar = "subBlock")
      ## Add horizontal and vertical lines as segment.
      ## adding/subtracting 0.5 assures plotting at the borders of
      ## the tiles.
      pTr <- pTr +
        ggplot2::geom_segment(ggplot2::aes_string(x = "x - 0.5",
                                                  xend = "x - 0.5",
                                                  y = "y - 0.5",
                                                  yend = "y + 0.5",
                                                  linetype = "'subBlocks'"),
                              data = subBlockBord[["vertW"]], size = 0.6,
                              color = "blue") +
        ggplot2::geom_segment(ggplot2::aes_string(x = "x - 0.5",
                                                  xend = "x + 0.5",
                                                  y = "y - 0.5",
                                                  yend = "y - 0.5"),
                              data = subBlockBord[["horW"]], size = 0.6,
                              color = "blue")
    }
    if (showGeno) {
      ## Add names of genotypes to the center of the tiles.
      pTr <- pTr +
        ggplot2::geom_text(ggplot2::aes_string(label = "genotype"),
                           size = 2, check_overlap = TRUE, na.rm = TRUE)
    }
    if (plotRep) {
      ## Add lines for replicates.
      ## Add horizontal and vertical lines as segment.
      ## adding/subtracting 0.5 assures plotting at the borders of
      ## the tiles.
      pTr <- pTr +
        ggplot2::geom_segment(ggplot2::aes_string(x = "x - 0.5",
                                                  xend = "x - 0.5",
                                                  y = "y - 0.5",
                                                  yend = "y + 0.5",
                                                  linetype = "'replicates'"),
                              data = repBord[["vertW"]], size = 1) +
        ggplot2::geom_segment(ggplot2::aes_string(x = "x - 0.5",
                                                  xend = "x + 0.5",
                                                  y = "y - 0.5",
                                                  yend = "y - 0.5"),
                              data = repBord[["horW"]], size = 1)
    }
    if (plotSubBlock || plotRep) {
      shwVals <- c(plotRep, plotSubBlock)
      newAes <- list(size = c(1, 0.6)[shwVals],
                     color = c("black", "blue")[shwVals])
      pTr <- pTr +
        ## Add a legend entry for replicates and subBlocks.
        ggplot2::scale_linetype_manual(c("replicates", "subBlocks")[shwVals],
                                       values = c("replicates" = "solid",
                                                  "subBlocks" = "solid")[shwVals],
                                       name = ggplot2::element_blank()) +
        ggplot2::guides(linetype = ggplot2::guide_legend(override.aes = newAes))
    }
    p[[trial]] <- pTr
    if (output) {
      plot(pTr)
    }
  }
  invisible(p)
}
