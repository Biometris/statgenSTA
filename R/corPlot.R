#' Helper function for creating a correlation plot
#'
#' Helper function for creating a correlation plot for an object of class TD.
#'
#' @keywords internal
corPlot <- function(x,
                    trials,
                    traits,
                    title,
                    output,
                    ...) {
  if (length(trials) == 1) {
    stop("At least two trials requiered for a correlation plot.\n")
  }
  chkChar(traits, null = FALSE)
  p <- setNames(vector(mode = "list", length = length(traits)), traits)
  for (trait in traits) {
    ## Create a single data.frame from x with only columns genotype, trial
    ## and trait.
    ## trials where trait is not measured/available are removed by setting
    ## them to NULL.
    plotDat <- Reduce(f = rbind,
                      x = lapply(X = x[trials], FUN = function(trial) {
                        if (!hasName(x = trial, name = trait)) {
                          NULL
                        } else {
                          trial[c("genotype", "trial", trait)]
                        }
                      }))
    if (is.null(plotDat)) {
      warning(trait, " isn't a column in any of the trials.\n",
              "Plot skipped.\n", call. = FALSE)
      next
    }
    plotDat <- droplevels(plotDat)
    ## Create table with values trait per genotype per trial.
    ## If TD already contains BLUEs/BLUPs taking means doesn't do anything
    ## but it is needed for raw data where there can be replicates.
    plotTab <- tapply(plotDat[[trait]],
                      INDEX = list(plotDat$genotype, plotDat$trial),
                      FUN = function(x) {
                        meanGT <- mean(x, na.rm = TRUE)
                        ifelse(is.nan(meanGT), NA, meanGT)
                      })
    ## Get number of observations on which correlation will be based.
    corBase <- as.data.frame(t(combn(x = levels(plotDat[["trial"]]), m = 2)))
    corBase <- cbind(corBase,
                     combn(x = levels(plotDat[["trial"]]), m = 2,
                           FUN = function(trials) {
                             sum(!is.na(rowSums(plotTab[, trials])))
                           }))
    ## Warn if number of observations below 10 for combinations of trials.
    ## Results get unreliable.
    corWarn <- corBase[corBase[[3]] < 11, ]
    nWarn <- nrow(corWarn)
    if (nWarn > 10) {
      warning("The correlation between ", nWarn, " sets of trials was ",
              "calculated with less than 10 genotypes.\n", call. = FALSE)
    } else if (nWarn > 0) {
      warning(vapply(X = seq_len(nWarn), FUN = function(i) {
        paste("The correlation between trials", corWarn[i, 1], "and",
              corWarn[i, 2], "was calculated with only", corWarn[i, 3],
              "genoypes.\n")
      }, FUN.VALUE = character(1)), call. = FALSE)
    }
    ## Create a correlation matrix.
    corMat <- tryCatchExt(cor(plotTab, use = "pairwise.complete.obs"))
    if (!is.null(corMat$error)) {
      stop(corMat$error)
    } else if (!is.null(supprWarn(corMat$warning, "deviation is zero"))) {
      warning(corMat$warning)
    }
    corMat <- corMat$value
    ## hclust doesn't allow missing values.
    if (any(is.na(corMat))) {
      stop("There are trials with no common genotypes. ",
           "Clustering impossible.\n")
    }
    ## Determine ordering according to clustering of trials.
    corClust <- hclust(as.dist(1 - corMat), method = "ward.D2")
    ordClust <- order.dendrogram(as.dendrogram(corClust))
    ## Reorder according to clusters.
    corMat <- corMat[ordClust, ordClust]
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
    ## Select bottom right triangle for correlations.
    meltedCorMatLow <- meltedCorMat[as.numeric(meltedCorMat[["trial1"]]) >
                                      as.numeric(meltedCorMat[["trial2"]]), ]
    if (is.null(title)) {
      plotTitle <- paste("Correlations of trials for", trait)
    } else {
      plotTitle <- title
    }
    ## Create plot.
    pTr <- ggplot2::ggplot(data = meltedCorMatLow,
                           ggplot2::aes_string("trial1", "trial2")) +
      ggplot2::geom_tile(ggplot2::aes_string(fill = "cor"),
                         color = "grey50") +
      ## Create a gradient scale.
      ggplot2::scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                                    na.value = "grey", limit = c(-1, 1)) +
      ## Move y-axis to the right for easier reading.
      ggplot2::scale_y_discrete(position = "right") +
      ## Remove grid behind empty bit of triangle.
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(angle = 45,
                                                         vjust = 1, size = 6,
                                                         hjust = 1),
                     axis.text.y = ggplot2::element_text(size = 6),
                     ## Center title.
                     plot.title = ggplot2::element_text(hjust = 0.5)) +
      ## No axis and legend titles.
      ggplot2::labs(title = plotTitle, x = "", y = "", fill = "") +
      ## Equal coordinates to get a square sized plot.
      ggplot2::coord_equal()
    p[[trait]] <- pTr
    if (output) {
      plot(pTr)
    }
  }
  invisible(p)
}
