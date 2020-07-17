## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>",
fig.dim = c(7, 4)
)
library(statgenSTA)

## ----createTD-----------------------------------------------------------------
## Create a TD object containing the data from Santa Rosa.
data("wheatChl")
wheatTD <- createTD(data = wheatChl[wheatChl$trial != "C_SWS_12", ], 
                    genotype = "trt", repId = "rep", subBlock = "bl", 
                    rowCoord = "row", colCoord = "col")

## ----getMeta------------------------------------------------------------------
## Extract meta data from the TD object. 
(wheatMeta <- getMeta(TD = wheatTD))

## ----setMeta------------------------------------------------------------------
## Fill in meta data and add back to the TD object.
wheatMeta$trLocation <- "Santa Rosa"
wheatMeta$trDate <- as.Date(rep(c("310811", "310812"), times = 2), "%d%m%y")
wheatMeta$trLat <- -36.32
wheatMeta$trLong <- -71.55
wheatMeta$trPlWidth = 2
wheatMeta$trPlLength = 1
wheatTD <- setMeta(TD = wheatTD, meta = wheatMeta)

## ----addTD, R.options=list(width=90)----------------------------------------------------
## Add the data for Cauquenes to the TD object.
wheatTD <- addTD(TD = wheatTD, data = wheatChl[wheatChl$trial == "C_SWS_12", ], 
                 genotype = "trt", repId = "rep", subBlock = "bl", 
                 rowCoord = "row", colCoord = "col", trLocation = "Cauquenes", 
                 trDate = as.Date("070912", "%d%m%y"), trLat = -35.58,
                 trLong = -72.17, trPlWidth = 2, trPlLength = 1)
## Inspect the meta data after the extra trial was added.
getMeta(TD = wheatTD)

## ----TDsum--------------------------------------------------------------------
## Create a summary for grain yield in SR_FI_11.
summary(wheatTD, trial = "SR_FI_11", traits = "GY")

## ----TDsumGroup---------------------------------------------------------------
## Create a summary for the two replicates in SR_FI_11.
summary(wheatTD, trial = "SR_FI_11", traits = "GY", groupBy = "repId")

## ----colorOpts, eval=FALSE----------------------------------------------------
#  ## Set default colors for genotypes and trials.
#  options("statgen.genoColors" = c("blue", "green", "yellow"))
#  options("statgen.trialColors" = c("red", "brown", "purple"))

## ----layoutPlot---------------------------------------------------------------
plot(wheatTD, trials = "SR_FI_11")

## ----layoutPlotHL-------------------------------------------------------------
## Plot the layout for SR_FI_11 with genotypes G278 and G279 highlighted.
plot(wheatTD, trials = "SR_FI_11", highlight = c("G278", "G279"))

## ----layoutPlotSB, fig.dim = c(7, 5)------------------------------------------
## Plot the layout for SR_FI_11, color subBlocks.
plot(wheatTD, trials = "SR_FI_11", colorSubBlock = TRUE)

## ----layoutPlotSG, fig.dim = c(7, 5)------------------------------------------
## Plot the layout for SR_FI_11, color subBlocks.
plot(wheatTD, trials = "SR_FI_11", showGeno = TRUE)

## ----mapPlot------------------------------------------------------------------
## Plot the locations of the trials on a map.
plot(wheatTD, plotType = "map")

## ----boxPlot------------------------------------------------------------------
## Create a boxplot for grain yield.
plot(wheatTD, plotType = "box", traits = "GY")

## ----boxPlotGR----------------------------------------------------------------
## Create a boxplot for grain yield with boxes grouped by year and repIds within
## years colored.
plot(wheatTD, plotType = "box", traits = "GY", groupBy = "year", 
     colorTrialBy = "repId", orderBy = "descending")

## ----corPlot------------------------------------------------------------------
## Create a correlation plot for grain yield.
plot(wheatTD, plotType = "cor", traits = "GY")

## ----scatPlot, fig.dim = c(7, 7)----------------------------------------------
## Create a scatter plot matrix for grain yield.
## Add correlations between trials in top left of scatter plots.
plot(wheatTD, plotType = "scatter", traits = "GY", addCorr = "tl")

## ----fitSp, message=FALSE-----------------------------------------------------
## Fit a single trial model using a model based on a resovable row column design.
modWheatSp <- fitTD(TD = wheatTD, trials = "SR_FI_11", traits = "GY", design = "res.rowcol")

## ----fitSpSm, message=FALSE---------------------------------------------------
## Fit a single trial model with genotype as random effect.
modWheatSp2 <- fitTD(TD = wheatTD, trials = "SR_FI_11", traits = "GY", 
                     what = "random", design = "res.rowcol")

## ----fitSpCtr, message=FALSE--------------------------------------------------
## Fit a spatial single trial model using SpATS. 
## Manually specify the number of segments for rows and columns.
modWheatSp3 <- fitTD(TD = wheatTD, trials = "SR_FI_11", traits = "GY", 
                     design = "res.rowcol", control = list(nSeg = c(20, 20)))

## ----fitAs, message=FALSE, results='hide'-------------------------------------
if (requireNamespace("asreml", quietly = TRUE)) {
  ## Fit a spatial single trial model using asreml.
  modWheatAs <- fitTD(TD = wheatTD, trials = "SR_FI_11", traits = "GY", 
                      design = "res.rowcol", spatial = TRUE, engine = "asreml",
                      control = list(criterion = "BIC"))
}

## ----spatCh, R.options=list(width=90)---------------------------------------------------
if (exists("modWheatAs")) {
  ## Overview of fitted models.
  print(modWheatAs$SR_FI_11$sumTab$GY, digits = 2, row.names = FALSE)
}  

## ----fitSum, message=FALSE----------------------------------------------------
## Set nBest to 5 to decrease size of output.
summary(modWheatSp, nBest = 5)

## ----basePlot-----------------------------------------------------------------
## Base plots for the model with genotype fitted as random effect.
plot(modWheatSp, plotType = "base", what = "random")

## ----spatPlot-----------------------------------------------------------------
## Spatial plot for the model with genotype fitted as fixed effect.
plot(modWheatSp, plotType = "spatial")

## ----spatPlotPerc-------------------------------------------------------------
## Spatial plot for the model with genotype fitted as fixed effect.
## Display the spatial trend as a percentage.
plot(modWheatSp, plotType = "spatial", spaTrend = "percentage")

## ----outDet-------------------------------------------------------------------
## Outlier detection for the model with genotype fitted as random.
outliers <- outlierSTA(modWheatSp, traits = "GY", what = "random")

## ----outDetCom----------------------------------------------------------------
## Outlier detection for the model with genotype fitted as random.
## A custom limit is used and commonFactors set to genotype.
outliers <- outlierSTA(modWheatSp, traits = "GY", what = "random",
                       rLimit = 3.2, commonFactors = "genotype")

## ----modRep, eval=FALSE-------------------------------------------------------
#  ## Create a report in the current working directory
#  report(modWheatSp)
#  ## Create a report for the model with genotype fitted as random.
#  report(modWheatSp, outfile = "./myReports/wheatReport.pdf", what = "random")

## ----extractOpts, results="as.is", echo=FALSE, out.width = "\\textwidth"------
## Generate table of options for extract from internal data.
optsTab <- statgenSTA:::extractOptions[, c("result", "model", "description")]
optsTab <- optsTab[order(optsTab[["model"]]), ]
knitr::kable(optsTab, align = c("llll"), row.names = FALSE)

## ----extBLUEs-----------------------------------------------------------------
## Extract BLUEs
BLUEsWheat <- extract(STA = modWheatSp, what = "BLUEs")
## Extract BLUEs and BLUPs
predWheat <- extract(STA = modWheatSp, what = c("BLUEs", "BLUPs"))

## ----extBLUEsKeep-------------------------------------------------------------
## Extract BLUEs from the fitted model.
BLUEsWheat2 <- extract(STA = modWheatSp, what = "BLUEs", keep = "trial")
head(BLUEsWheat2[["SR_FI_11"]]$BLUEs)

## ----extFit-------------------------------------------------------------------
## Extract fitted values from the model.
fitVals <- extract(STA = modWheatSp, what = "fitted", keep = c("trial", "repId"))
head(fitVals[["SR_FI_11"]]$fitted)

## ----STAtoTD, message=FALSE---------------------------------------------------
## Fit a model for all trials with genotype as fixed factor.
modWheatSpTot <- fitTD(TD = wheatTD, traits = "GY", what = "fixed", 
                       design = "res.rowcol")
## Create a TD object containing BLUEs and standard errors of BLUEs.
TDGxE <- STAtoTD(STA = modWheatSpTot, what = c("BLUEs", "seBLUEs"))
## Add weights to the output.
TDGxE2 <- STAtoTD(STA = modWheatSpTot, what = c("BLUEs", "seBLUEs"), addWt = TRUE)

