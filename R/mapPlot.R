#' Helper function for creating a map plot
#'
#' Helper function for creating a map plot for an object of class TD.
#'
#' @keywords internal
mapPlot <- function(x,
                    title = "Trial locations",
                    output = TRUE,
                    ...) {
  dotArgs <- list(...)
  colorTrialBy <- dotArgs$colorTrialBy
  colTrial <- dotArgs$colTrial
  printTrialNames <- dotArgs$printTrialNames
  ## Checks for colTrial.
  chkChar(colTrial)
  if (!is.null(colorTrialBy)) {
    ## Checks for colorTrialBy.
    chkChar(colorTrialBy, len = 1, null = FALSE)
    if (!all(vapply(X = x, FUN = hasName, FUN.VALUE = logical(1),
                    name = colorTrialBy))) {
      stop("colorTrialBy should be a column in TD.\n")
    }
    colorTrialDat <- do.call(rbind, lapply(X = x, FUN = function(trial) {
      ## Assure that coloring by trial is possible by using unique within
      ## column selection. Not doing so results in a column named trial.1
      ## causing problems when referring to trial later on.
      colorTrial <- unique(trial[, unique(c("trial", colorTrialBy)),
                                 drop = FALSE])
      if (nrow(colorTrial) != 1) {
        stop("colorTrialBy should be unique within each trial.\n")
      }
      return(colorTrial)
    }))
  } else {
    ## Create a dummy data.frame colorTrialDat to assure later on no
    ## split has to be made between cases.
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
  ## Get the number of colors needed for coloring the trials.
  nColTrial <- nlevels(colorTrialDat[[colorTrialBy]])
  colTrial <- defineTrialColors(colors = colTrial, n = nColTrial,
                                default = "red")
  ## Check for latitude and longitude.
  minLatRange <- dotArgs$minLatRange
  minLongRange <- dotArgs$minLongRange
  chkNum(minLatRange, null = TRUE)
  chkNum(minLongRange, null = TRUE)
  if (is.null(minLatRange)) {
    minLatRange <- 10
  }
  if (is.null(minLongRange)) {
    minLongRange <- 5
  }
  ## Create a data.frame for plotting trials.
  locs <- setNames(getMeta(x)[c("trLocation", "trLat", "trLong")],
                   c("name", "lat", "long"))
  locs[["trial"]] <- rownames(locs)
  ## Merge groups for coloring text.
  locs <- merge(locs, colorTrialDat, by.x = "row.names", by.y = "trial")
  if (any(table(unique(locs[c("name", colorTrialBy)])) > 1)) {
    stop("colorTrialBy should be unique within locations.\n")
  }
  ## Deduplicate locations to plot based on latitude and longitude.
  ## This avoids plotting identical locations multiple times
  ## (for e.g multiple years.)
  ## Only keep columns required for the actual plot.
  locs <- unique(locs[!is.na(locs[["lat"]]) & !is.na(locs[["long"]]),
                      c("name", "lat", "long", colorTrialBy)])
  if (nrow(locs) == 0) {
    stop("At least one trial should have latitude and longitude ",
         "for plotting on map.\n")
  }
  ## Set minimum range for latitude and longitude.
  latR <- range(locs[["lat"]])
  latR <- latR +
    (diff(latR) < minLatRange) * c(-1, 1) * (minLatRange - diff(latR)) / 2
  longR <- range(locs[["long"]])
  longR <- longR +
    (diff(longR) < minLongRange) * c(-1, 1) * (minLongRange - diff(longR)) / 2
  ## Add 10% to edges of the map so locations are not on the absolute edge.
  longR <- longR + c(-0.1, 0.1) * diff(longR)
  latR <- latR + c(-0.1, 0.1) * diff(latR)
  ## Create data usable by ggplot geom_polygon.
  mapDat <- mapData(xLim = longR, yLim = latR)
  p <- ggplot2::ggplot(mapDat, ggplot2::aes_string(x = "long", y = "lat")) +
    ggplot2::geom_polygon(ggplot2::aes_string(group = "group"), fill = "white",
                          color = "black") +
    ## Add a proper map projection.
    ggplot2::coord_map(clip = "on", xlim = longR, ylim = latR) +
    ## Add trial locations.
    ggplot2::geom_point(data = locs,
                        ggplot2::aes_string(color = colorTrialBy),
                        show.legend = isFALSE(printTrialNames) &
                          colorTrialBy != ".colorTrialBy") +
    ggplot2::scale_color_manual(values = colTrial) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   ## Empty space left represents water areas. Color blue.
                   panel.background =
                     ggplot2::element_rect(fill = "steelblue2")) +
    ggplot2::ggtitle(title)
  if (!isFALSE(printTrialNames)) {
    p <- p +
      ggrepel::geom_text_repel(mapping =
                                 ggplot2::aes_string(label = "name",
                                                     color = colorTrialBy),
                               data = locs, size = 3,
                               nudge_x = 0.01 * diff(longR),
                               nudge_y = 0.04 * diff(latR),
                               show.legend = colorTrialBy != ".colorTrialBy")
  }
  if (output) {
    plot(p)
  }
  invisible(p)
}
