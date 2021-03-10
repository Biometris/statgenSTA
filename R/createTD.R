#' S3 class TD
#'
#' \code{createTD}\cr
#' Function for creating objects of S3 class TD (Trial Data). The function
#' converts a data.frame to an object of class TD in the following steps:
#' \itemize{
#' \item{Check input data}
#' \item{Rename columns to default column names - default column names:
#' genotype, trial, loc, year, repId, subBlock, rowCoord, colCoord, rowId,
#' colId, checkId}
#' \item{Convert column types to default column types - rowCoord and colCoord
#' are converted to numeric columns, all other renamed columns to factor
#' columns. Columns other than the default columns, e.g. traits or other
#' covariates will be included in the output unchanged.}
#' \item{Split input data by trial - each trial in the input data will become
#' a list item in the output.}
#' \item{Add meta data - the trial meta data are added as attributes to the
#' different output items. The function parameters starting with "tr" provide
#' the meta data. Their values will be recycled if needed, so by setting a
#' single "trDesign", all trials will get the same design. For trLat, trLong,
#' trDesign and trDate a column in \code{data} that contains the information
#' can be specified as well. The meta data can be changed later on using
#' \code{getMeta} and \code{setMeta}}
#' }
#' \code{addTD}\cr
#' Function for adding extra trial data to an existing object of class TD. The
#' data for the new trials will be added after the data for existing trials. It
#' is possible to add data for an already existing trial, but this will cause
#' multiple items in the output with identical names, which might cause problems
#' later on in the analysis. Therefore a warning will be issued in this
#' case.\cr\cr
#' \code{dropTD}\cr
#' Function for removing data for selected trials from an existing object of
#' class TD.\cr\cr
#' \code{\link{summary.TD}} and \code{\link{plot.TD}} methods are available.
#'
#' @param data A data.frame containing trial data with at least a column for
#' genotype. The data.frame should be in a wide format, i.e. all available
#' phenotypic data should be in a separate column within the data.frame.
#' @param genotype An optional character string indicating the column in
#' \code{data} that contains genotypes.
#' @param trial An optional character string indicating the column in
#' \code{data} that contains trials.
#' @param loc An optional character string indicating the column in
#' \code{data} that contains trial locations.
#' @param year An optional character string indicating the column in \code{data}
#' that contains years.
#' @param repId An optional character string indicating the column in
#' \code{data} that contains replicates.
#' @param subBlock An optional character string indicating the column in
#' \code{data} that contains sub blocks.
#' @param plotId An optional character string indicating the column in
#' \code{data} that contains plots. This column will be combined with trial
#' to a single output factor.
#' @param rowCoord An optional character string indicating the column in
#' \code{data} that contains the row coordinates.
#' @param colCoord An optional character string indicating the column in
#' \code{data} that contains the column coordinates.
#' @param rowId An optional character string indicating the column in
#' \code{data} that contains field rows. If not supplied, this is assumed to
#' be the same as rowCoord.
#' @param colId An optional character string indicating the column in
#' \code{data} that contains field columns. If not supplied, this is assumed to
#' be the same as colCoord.
#' @param checkId An optional character string indicating the column in
#' \code{data} that contains the check IDs.
#' @param trLocation An optional character vector indicating the locations of
#' the trials. This will be used as default names when creating plots and
#' summaries. If no locations are provided, first the column loc is considered.
#' If this contains one unique value for a trial this is used as trLocation.
#' Otherwise the trial name is used.
#' @param trDate An optional character string indicating the column in
#' \code{data} that contains the date of the trial or a date vector indicating
#' the dates of the trials.
#' @param trDesign An optional character string indicating the column in
#' \code{data} that contains the design of the trial or a character vector
#' indicating the designs of the trials. Either "none" (no (known) design), "
#' ibd" (incomplete-block design), "res.ibd" (resolvable incomplete-block
#' design), "rcbd" (randomized complete block design), "rowcol" (row-column
#' design) or "res.rowcol" (resolvable row-column design).
#' @param trLat An optional character string indicating the column in \code{data}
#' that contains the latitude of the trial or a numerical vector indicating the
#' latitudes of the trials on a scale of -90 to 90.
#' @param trLong An optional character string indicating the column in
#' \code{data} that contains the latitude of the trial or a numerical vector
#' indicating the longitudes of the trials on a scale of -180 to 180.
#' @param trPlWidth An optional positive numerical vector indicating the
#' widths of the plots.
#' @param trPlLength An optional positive numerical vector indicating the
#' lengths of the plots.
#'
#' @return An object of class TD, a list of data.frames with renamed columns
#' and an attribute \code{renamedCols} containing an overview of renamed
#' columns. For each unique value of trial, the output has a data.frame in
#' the list with the same name as the trial. These data.frames have attributes
#' containing the metadata for the corresponding trial. If there is no column
#' for trial, the list will contain one item named after the input data.
#'
#' @examples
#' ## Create a data.frame to be converted to TD object.
#' ## The data consists of genotype, trial, row and column information and
#' ## two traits, yield and flowering time.
#' datT1 <- data.frame(geno = paste0("G", 1:10), tr = "T1",
#'                     row = rep(1:5, each = 2), col = rep(1:2, times = 5),
#'                     yield = 1:10, flowering = 3:12)
#'
#' ## Convert data.frame to TD object.
#' TDT1 <- createTD(data = datT1, genotype = "geno", trial = "tr",
#'                  rowCoord = "row", colCoord = "col")
#'
#' ## Create a second data.frame similar to the first with data for a second trial.
#' datT2 <- data.frame(geno = paste0("G", 1:10), tr = "T2",
#'                     row = rep(1:2, each = 5), col = rep(1:5, times = 2),
#'                     yield = 10:1, flowering = 12:3)
#'
#' ## Add this data to the TD object created above.
#' TDTot <- addTD(TD = TDT1, data = datT2, genotype = "geno", trial = "tr",
#'                rowCoord = "row", colCoord = "col")
#'
#' ## Drop the data for the first trial from the object.
#' TDT2 <- dropTD(TD = TDTot, rmTrials = "T1")
#'
#' @family functions for TD objects
#'
#' @importFrom utils hasName
#'
#' @name TD
NULL

#' @rdname TD
#' @export
createTD <- function(data,
                     genotype = NULL,
                     trial = NULL,
                     loc = NULL,
                     year = NULL,
                     repId = NULL,
                     subBlock = NULL,
                     plotId = NULL,
                     rowCoord = NULL,
                     colCoord = NULL,
                     rowId = rowCoord,
                     colId = colCoord,
                     checkId = NULL,
                     trLocation = NULL,
                     trDate = NULL,
                     trDesign = NULL,
                     trLat = NULL,
                     trLong = NULL,
                     trPlWidth = NULL,
                     trPlLength = NULL) {
  ## Save name of original data for naming output.
  dataName <- deparse(substitute(data))
  if (length(dataName) > 1) {
    dataName <- "dat"
  }
  ## Checks.
  if (missing(data) || !is.data.frame(data)) {
    stop("data has to be a data.frame.\n")
  }
  ## Convert input to data.frame. This needs to be done to be able to handle
  ## tibbles and possibly other data structures in the future.
  data <- as.data.frame(data)
  cols <- colnames(data)
  for (param in c(genotype, trial, loc, year, repId, subBlock, plotId,
                  rowId, colId, rowCoord, colCoord, checkId)) {
    if (!is.null(param) && (!is.character(param) || length(param) > 1 ||
                            !hasName(data, param))) {
      stop(deparse(param), " has to be NULL or a column in data.\n")
    }
  }
  checkTDMeta(trPlWidth = trPlWidth, trPlLength = trPlLength)
  ## Create list of reserved column names for renaming columns.
  renameCols <- c("genotype", "trial", "loc", "year", "repId", "plotId",
                  "subBlock", "rowId", "colId", "rowCoord", "colCoord",
                  "checkId")
  ## First rename duplicate columns and add duplicated columns to data.
  renameFrom <- as.character(sapply(X = renameCols, FUN = function(x) {
    get(x)
  }))
  ## Create a data.frame with renamed cols to add to TD as an attribute.
  renamed <- data.frame(orig = renameFrom[renameFrom != "NULL"],
                        new = renameCols[renameFrom != "NULL"],
                        stringsAsFactors = FALSE)
  ## Get duplicate columns.
  dupCols <- which(duplicated(renameFrom) & renameFrom != "NULL")
  for (dupCol in dupCols) {
    ## Copy original column as extra column in data for each duplicate.
    tempName <- paste0(".temp", dupCol)
    data[tempName] <- data[, colnames(data) == renameFrom[dupCol]]
    ## Add new replacementname to cols and renameFrom.
    cols[length(cols) + 1] <- tempName
    renameFrom[dupCol] <- tempName
  }
  ## Rename columns.
  for (i in 1:length(renameCols)) {
    cols[cols == renameFrom[i]] <- renameCols[i]
  }
  ## Check for duplicates in cols.
  ## When there are renaming is attempted to an existing column.
  dupCols <- cols[duplicated(cols)]
  if (length(dupCols) > 0) {
    stop("The following columns already exist in the input data:\n",
         paste(dupCols, collapse = ","), "\n",
         "Renaming another column to one of these is impossible.\n")
  }
  colnames(data) <- cols
  ## Convert columns to factor if neccessary.
  factorCols <-  c("genotype", "trial", "loc", "year", "repId", "subBlock",
                   "plotId", "rowId", "colId", "checkId")
  for (factorCol in factorCols) {
    if (hasName(data, factorCol) && !is.factor(data[[factorCol]])) {
      data[cols == factorCol] <- as.factor(data[, cols == factorCol])
    }
  }
  ## Combine plotId and trial into a single factor if both are available.
  ## If trial is not available plotId itself was converted to factor in the
  ## previous step.
  if (all(hasName(data, c("trial", "plotId")))) {
    data$plotId <- interaction(data$trial, data$plotId, sep = "_")
  }
  ## Convert columns to numeric if neccessary.
  numCols <- c("rowCoord", "colCoord")
  for (numCol in numCols) {
    if (hasName(data, numCol) && !is.numeric(data[cols == numCol])) {
      data[cols == numCol] <- as.numeric(data[, cols == numCol])
    }
  }
  ## Sort data by rowCoord and colCoord.
  ## This is only needed for spatial modeling with asreml but it doesn't harm
  ## always doing so.
  if (all(hasName(data, c("rowCoord", "colCoord")))) {
    data <- data[order(data[["rowCoord"]], data[["colCoord"]]), ]
    ## Check that row column combinations are unique within trials.
    if (hasName(data, "trial")) {
      rowColTab <- table(data[["trial"]], data[["rowCoord"]],
                         data[["colCoord"]])
      if (any(rowColTab > 1)) {
        warning("Combinations of row and column coordinates should be unique ",
                "within trials.\n")
      }
    } else {
      rowColTab <- table(data[["rowCoord"]], data[["colCoord"]])
      if (any(rowColTab > 1)) {
        warning("Combinations of row and column coordinates should be unique.\n")
      }
    }
  }
  if (hasName(data, "trial")) {
    listData <- split(x = data, f = data[["trial"]], drop = TRUE)
  } else {
    listData <- setNames(list(data), dataName)
  }
  ## Define meta data to set from input variables.
  ## trLat, trLong, trDate and trDesign might be defined from variables so
  ## are treated differently.
  meta <- c("trLocation", "trPlWidth", "trPlLength")
  ## Expand input values for meta variables to number of trials.
  metaVals <- sapply(X = meta, FUN = function(m) {
    if (!is.null(get(m))) {
      metaVal <- rep(x = get(m), length.out = length(listData))
      if (is.null(names(metaVal)) || !all(hasName(listData, names(metaVal)))) {
        names(metaVal) <- names(listData)
      }
      return(metaVal)
    } else {
      NULL
    }
  }, simplify = FALSE)
  ## Set meta for all trials in data.
  for (tr in names(listData)) {
    for (m in meta) {
      ## Set meta data. Set to NULL if not in input so meta variable is
      ## left out meta data. This to avoid a list of NULL.
      attr(x = listData[[tr]], which = m) <- unname(metaVals[[m]][tr])
    }
    ## Location should always be filled since it is used in plot titles as
    ## well. Use trial name as default value.
    if (is.null(trLocation)) {
      if (hasName(x = listData[[tr]], name = "loc") &&
          length(unique(listData[[tr]][["loc"]])) == 1) {
        attr(x = listData[[tr]],
             which = "trLocation") <- as.character(listData[[tr]][["loc"]][1])
      } else {
        attr(x = listData[[tr]], which = "trLocation") <- tr
      }
    }
    ## If trLat or trLong are specifying a column in the data set meta data
    ## to the value for this column. To do so the value has to be unique within
    ## each trial.
    trLatDat <- trLat
    if (!is.null(trLat) && hasName(x = listData[[tr]], name = trLat)) {
      ## Set trLatDat to value within trial and check for uniqueness.
      trLatDat <- unique(listData[[tr]][[trLat]])
      if (!length(trLatDat) == 1) {
        stop("trLat not unique for ", tr, ".\n")
      }
    }
    trLongDat <- trLong
    if (!is.null(trLong) && hasName(x = listData[[tr]], name = trLong)) {
      ## Set trLongDat to value within trial and check for uniqueness.
      trLongDat <- unique(listData[[tr]][[trLong]])
      if (!length(trLongDat) == 1) {
        stop("trLong not unique for ", tr, ".\n")
      }
    }
    ## Check that combination of latitude and longitude set is valid.
    chkLatLong(trLatDat, trLongDat)
    attr(x = listData[[tr]], which = "trLat") <- trLatDat
    attr(x = listData[[tr]], which = "trLong") <- trLongDat
    ## If trDate is specifying a column in the data set meta data to the value
    ## for this column. To do so the value has to be unique within each trial.
    trDateDat <- trDate
    if (!is.null(trDate) && hasName(x = listData[[tr]], name = trDate)) {
      ## Set trDateDat to value within trial and check for uniqueness.
      trDateDat <- unique(listData[[tr]][[trDate]])
      if (!length(trDateDat) == 1) {
        stop("trDate not unique for ", tr, ".\n")
      }
    }
    attr(x = listData[[tr]], which = "trDate") <- trDateDat
    ## If trDesign is specifying a column in the data set meta data to the value
    ## for this column. To do so the value has to be unique within each trial.
    trDesignDat <- trDesign
    if (!is.null(trDesign) && hasName(x = listData[[tr]], name = trDesign)) {
      ## Set trDateDat to value within trial and check for uniqueness.
      trDesignDat <- unique(listData[[tr]][[trDesign]])
      if (!length(trDesignDat) == 1) {
        stop("trDesign not unique for ", tr, ".\n")
      }
    }
    chkDesign(trDesignDat)
    attr(x = listData[[tr]], which = "trDesign") <- trDesignDat
    ## Add a list of columns that have been renamed as attribute to TD.
    attr(x = listData[[tr]], which = "renamedCols") <-
      if (nrow(renamed) > 0) renamed else NULL
  }
  TD <- structure(listData,
                  class = c("TD", "list"))
  return(TD)
}

#' @param TD An object of class TD which should be modified.
#'
#' @rdname TD
#' @export
addTD <- function(TD,
                  data,
                  genotype = NULL,
                  trial = NULL,
                  loc = NULL,
                  year = NULL,
                  repId = NULL,
                  subBlock = NULL,
                  plotId = NULL,
                  rowCoord = NULL,
                  colCoord = NULL,
                  rowId = rowCoord,
                  colId = colCoord,
                  checkId = NULL,
                  trLocation = NULL,
                  trDate = NULL,
                  trDesign = NULL,
                  trLat = NULL,
                  trLong = NULL,
                  trPlWidth = NULL,
                  trPlLength = NULL) {
  TDNw <- createTD(data = data, genotype = genotype, trial = trial,
                   loc = loc, year = year, repId = repId,
                   subBlock = subBlock, plotId = plotId, rowCoord = rowCoord,
                   colCoord = colCoord, rowId = rowId, colId = colId,
                   checkId = checkId, trLocation = trLocation, trDate = trDate,
                   trDesign = trDesign, trLat = trLat, trLong = trLong,
                   trPlWidth = trPlWidth, trPlLength = trPlLength)
  dupTrials <- names(TDNw)[names(TDNw) %in% names(TD)]
  if (length(dupTrials) > 0) {
    warning("The following trials already existed in TD and will be added ",
            "again: ", paste(dupTrials, collapse = ", "), ".\n", call. = FALSE)
  }
  TDTot <- c(TD, TDNw)
  class(TDTot) <- c("TD", "list")
  return(TDTot)
}

#' @param rmTrials A character vector of trials that should be removed.
#'
#' @rdname TD
#' @export
dropTD <- function(TD,
                   rmTrials) {
  naTrials <- rmTrials[!rmTrials %in% names(TD)]
  if (length(naTrials) > 0) {
    warning("The following trials are not in TD: ",
            paste(naTrials, collapse = ", "), ".\n", call. = FALSE)
  }
  leftTrials <- names(TD)[!names(TD) %in% rmTrials]
  if (length(leftTrials) == 0) {
    warning("All trials have been removed from TD.\n", call. = FALSE)
  }
  return(TD[!names(TD) %in% rmTrials])
}

#' Summarizing objects of class \code{TD}
#'
#' \code{summary} method for class \code{TD}.
#'
#' @param object An object of class TD.
#' @param ... Further arguments - currently not used.
#' @param trial A character string specifying the trial to be summarized.
#' @param traits A character vector specifying the traits to be summarized.
#' @param groupBy A character string specifying a column in TD by which the
#' summary should be grouped. If \code{NULL}, no grouping is done.
#' @param what A character vector indicating which summary statistics should be
#' computed. If \code{what = "all"}, all available statistics are computed.\cr
#' Possible options are:
#' \describe{
#' \item{nVals}{The number of values, i.e. non-missing + missing values.}
#' \item{nObs}{The number of non-missing observations.}
#' \item{nMiss}{The number of missing values.}
#' \item{mean}{The mean.}
#' \item{median}{The median.}
#' \item{min}{The minimum.}
#' \item{max}{The maximum.}
#' \item{range}{The range (maximum - minimum).}
#' \item{firstQ}{The first (25pct) quantile.}
#' \item{thirdQ}{The third (75pct) quantile.}
#' \item{sd}{The standard deviation.}
#' \item{seMean}{The standard error of mean.}
#' \item{var}{The variance.}
#' \item{seVar}{The standard error of variance.}
#' \item{CV}{The coefficient of variation.}
#' \item{sum}{The sum.}
#' \item{sumSq}{The sum of squares.}
#' \item{uncorSumSq}{The uncorrected sum of squares.}
#' \item{skew}{The skewness.}
#' \item{seSkew}{The standard error of the skewness.}
#' \item{kurt}{The kurtosis.}
#' \item{seKurt}{The standard error of the kurtosis.}
#' \item{all}{All summary statistics.}
#' }
#'
#' @return A table containing the selected summary statistics.
#'
#' @family functions for TD objects
#'
#' @examples
#' ## Summarize TDHeat05.
#' summary(TDHeat05, traits = "yield")
#'
#' ## Summarize TDHeat05 grouping by repId.
#' summary(TDHeat05, traits = "yield", groupBy = "repId")
#'
#' @export
summary.TD <- function(object,
                       ...,
                       trial = names(object),
                       traits,
                       groupBy = NULL,
                       what = if (!is.null(groupBy)) {
                         c("nObs", "mean", "sd")
                       } else {
                         c("nObs", "nMiss", "mean", "median", "min", "max",
                           "firstQ", "thirdQ", "var")
                       }) {
  allStat <- data.frame(stat = c("nVals", "nObs", "nMiss", "mean", "median",
                                 "min","max", "range", "firstQ", "thirdQ", "sd",
                                 "seMean", "var", "seVar", "CV", "sum", "sumSq",
                                 "uncorSumSq", "skew", "seSkew", "kurt",
                                 "seKurt"),
                        name = c("Number of values", "Number of observations",
                                 "Number of missing values", "Mean", "Median",
                                 "Min", "Max", "Range", "First quantile",
                                 "Third quantile", "Standard deviation",
                                 "Standard error of mean", "Variance",
                                 "Standard error of variance",
                                 "Coefficient of variation", "Sum of values",
                                 "Sum of squares", "Uncorrected sum of squares",
                                 "Skewness", "Standard Error of Skewness",
                                 "Kurtosis", "Standard Error of Kurtosis"),
                        stringsAsFactors = FALSE)
  ## Checks.
  if (!is.character(trial) || length(trial) > 1 ||
      !hasName(x = object, name = trial)) {
    stop("trial should be a single character string in ",
         deparse(substitute(object)), ".\n")
  }
  trDat <- object[[trial]]
  if (!is.character(traits) || !all(hasName(x = trDat, name = traits))) {
    stop("All traits should be columns in trial.\n")
  }
  if (!is.null(groupBy) && (!is.character(groupBy) || length(groupBy) > 1 ||
                            !hasName(x = trDat, name = groupBy))) {
    stop("groupBy should be a single character string indicating ",
         "a column in trial")
  }
  if (what[[1]] == "all") {
    what <- allStat[["stat"]]
  }
  if (!is.character(what) || all(!what %in% allStat[["stat"]])) {
    stop("At least one statistic should be chosen.\n")
  }
  whichStat <- which(allStat[["stat"]] %in% what)
  what <- allStat[whichStat, "stat"]
  if (!is.null(groupBy)) {
    groups <- unique(trDat[[groupBy]])
  } else {
    trDat[[".tot"]] <- 1
    groupBy <- ".tot"
    groups <- 1
  }
  ## Create an array to store the values.
  stats <- array(dim = c(length(what), length(traits), length(groups)),
                 dimnames = list(what, traits, groups))
  for (i in seq_along(traits)) {
    for (j in seq_along(groups)) {
      trDatGr <- trDat[trDat[[groupBy]] == groups[j], traits[i]]
      if ("nVals" %in% what) {
        stats["nVals", i, j] <- length(trDatGr)
      }
      if ("nObs" %in% what) {
        stats["nObs", i, j] <- length(na.omit(trDatGr))
      }
      if ("nMiss" %in% what) {
        stats["nMiss", i, j] <- sum(is.na(trDatGr))
      }
      if ("mean" %in% what) {
        stats["mean", i, j] <- mean(trDatGr, na.rm = TRUE)
      }
      if ("median" %in% what) {
        stats["median", i, j] <- median(trDatGr, na.rm = TRUE)
      }
      if ("min" %in% what) {
        stats["min", i, j] <- min(trDatGr, na.rm = TRUE)
      }
      if ("max" %in% what) {
        stats["max", i, j] <- max(trDatGr, na.rm = TRUE)
      }
      if ("range" %in% what) {
        stats["range", i, j] <- diff(range(trDatGr, na.rm = TRUE))
      }
      if ("firstQ" %in% what) {
        stats["firstQ", i, j] <- quantile(trDatGr, prob = .25, na.rm = TRUE)
      }
      if ("thirdQ" %in% what) {
        stats["thirdQ", i, j] <- quantile(trDatGr, prob = .75, na.rm = TRUE)
      }
      if ("sd" %in% what) {
        stats["sd", i, j] <- sd(trDatGr, na.rm = TRUE)
      }
      if ("seMean" %in% what) {
        stats["seMean", i, j] <- sd(trDatGr, na.rm = TRUE) /
          sqrt(length(na.omit(trDatGr)))
      }
      if ("var" %in% what) {
        stats["var", i, j] <- var(trDatGr, na.rm = TRUE)
      }
      if ("seVar" %in% what) {
        stats["seVar", i, j] <- seVar(trDatGr, na.rm = TRUE)
      }
      if ("CV" %in% what) {
        stats["CV", i, j] <- 100 * sd(trDatGr, na.rm = TRUE) /
          mean(trDatGr, na.rm = TRUE)
      }
      if ("sum" %in% what) {
        stats["sum", i, j] <- sum(trDatGr, na.rm = TRUE)
      }
      if ("sumSq" %in% what) {
        stats["sumSq", i, j] <- sum((na.omit(trDatGr) -
                                       mean(trDatGr, na.rm = TRUE)) ^ 2)
      }
      if ("uncorSumSq" %in% what) {
        stats["uncorSumSq", i, j] <- sum(trDatGr ^ 2, na.rm = TRUE)
      }
      if ("skew" %in% what) {
        stats["skew", i, j] <- skewness(trDatGr, na.rm = TRUE)
      }
      if ("seSkew" %in% what) {
        stats["seSkew", i, j] <- seSkewness(length(na.omit(trDatGr)))
      }
      if ("kurt" %in% what) {
        stats["kurt", i, j] <- kurtosis(trDatGr, na.rm = TRUE)
      }
      if ("seKurt" %in% what) {
        stats["seKurt", i, j] <- seKurtosis(length(na.omit(trDatGr)))
      }
    }
  }
  rownames(stats) <- allStat[whichStat, "name"]
  attr(x = stats, which = "whichStat") <- whichStat
  return(structure(stats,
                   class = c("summary.TD", "array"),
                   trial = trial,
                   group = if (groupBy != ".tot") groupBy else NULL))
}

#' @export
print.summary.TD <- function(x,
                             ...) {
  whichStat <- attr(x, "whichStat")
  groupBy  <- attr(x, "group")
  decimals <- c(rep(x = 0, times = 3), rep(x = 2, times = 7),
                rep(x = 3, times = 5), rep(x = 2, times = 3),
                rep(x = 3, times = 4))[whichStat]
  xPrint <- x
  for (i in seq_along(decimals)) {
    xPrint[i, , ] <- format(x[i, , ], digits = decimals[i],
                            nsmall = decimals[i])
  }
  for (i in 1:ncol(xPrint)) {
    trait <- colnames(xPrint)[i]
    cat(paste("\nSummary statistics for", trait, "in", attr(x, "trial"),
              if (!is.null(groupBy)) paste("grouped by", groupBy), "\n\n"))
    if (dim(xPrint)[3] > 1) {
      xPrintM <- matrix(nrow = nrow(xPrint), ncol = dim(xPrint)[3])
      for (j in 1:nrow(xPrint)) {
        xPrintM[j, ] <- xPrint[j, i, ]
      }
      dimnames(xPrintM) <- list(rownames(xPrint), dimnames(xPrint)[[3]])
      print(xPrintM, quote = FALSE, right = TRUE)
    } else {
      xPrintM <- as.matrix(xPrint[, i , 1])
      dimnames(xPrintM) <- list(rownames(xPrint), trait)
      print(xPrintM, quote = FALSE, right = TRUE)
    }
  }
  cat("\n")
}

#' Plot function for class TD
#'
#' Plotting function for objects of class TD. Plots either the layout of the
#' different trials within the TD object or locates the trials on a map. Also a
#' boxplot can be made for selected traits and trials, a plot of correlations
#' between trials and a scatter plot matrix. A detailed description and optional
#' extra parameters of the different plots is given in the sections below.
#'
#' @section Layout Plot:
#' Plots the layout of the selected trials.
#' This plot can only be made for trials that contain both row (\code{rowCoord})
#' and column (\code{colCoord}) information. If either one of those is missing
#' the trial is skipped with a warning. If blocks (\code{subBlock}) are
#' available for a trial these can be colored in different colors per block by
#' setting \code{colorSubBlock = TRUE}. If replicates (\code{repId}) are
#' available a black line is plotted between different replicates. Missing plots
#' are indicated in white. These can either be single plots in a trial or
#' complete missing columns or rows.\cr
#' Extra parameter options:
#' \describe{
#' \item{showGeno}{Should individual genotypes be indicated as text in the plot?
#' Defaults to \code{FALSE}}
#' \item{sizeGeno}{The text size for indicating individual genotypes. Defaults
#' to 2. Ignored if \code{showGeno = FALSE}.}
#' \item{highlight}{A character vector of genotypes to be highlighted in the
#' plot.}
#' \item{colorSubBlock}{Should blocks be colored with a different color per
#' subBlock? Defaults to \code{FALSE}. \code{colorSubBlock} is ignored when
#' highlight is used to highlight genotypes.}
#' }
#'
#' @section Map Plot:
#' A map is plotted with the locations of the trials in the TD object.
#' Mapping the trials is done based on latitude and longitude that can be
#' added when creating an object of class TD. Trials for which either latitude
#' or longitude is not available are skipped with a warning message.
#' The countries in which the trials are located will be plotted on a single
#' map and the location of the trials will be indicated on this map. The
#' actual plot is made using ggplot, but for getting the data for the borders
#' of the countries the maps package is needed.\cr
#' Extra parameter options:
#' \describe{
#' \item{colorTrialBy}{A character string indicating a column in \code{TD} by
#' which the trials on the map are colored.}
#' \item{colTrial}{A character vector with plot colors for the trials. A
#' single color when \code{colorTrialBy = NULL}, a vector of colors otherwise.}
#' \item{printTrialNames}{Should trial names be printed. Defaults to
#' \code{TRUE}. Setting this to \code{FALSE} can be useful if there are many
#' trials.}
#' \item{minLatRange}{A positive numerical value indicating the minimum range
#' (in degrees) for the latitude on the plotted map. Defaults to 10.}
#' \item{minLongRange}{A positive numerical value indicating the minimum range
#' (in degrees) for the longitude on the plotted map. Defaults to 5.}
#' }
#'
#' @section Box Plot:
#' Creates a boxplot per selected trait grouped by trial. Extra parameter
#' options:
#' \describe{
#' \item{groupBy}{A character string indicating a column in \code{TD} by which
#' the boxes in the plot should be grouped. By default the boxes are grouped
#' per trial.}
#' \item{colorTrialBy}{A character string indicating a column in \code{TD} by
#' which the boxes are colored. Coloring will be done within the groups
#' indicated by the \code{groupBy} parameter.}
#' \item{colTrial}{A character vector with plot colors for the trials. A
#' single color when \code{colorTrialBy = NULL}, a vector of colors otherwise.}
#' \item{orderBy}{A character string indicating the way the boxes should be
#' ordered. Either "alphabetic" for alphabetical ordering of the groups,
#' "ascending" for ordering by ascending mean, or "descending" for ordering by
#' descending mean. Default boxes are ordered alphabetically.}
#' }
#'
#' @section Correlation Plot:
#' Draws a heat map of correlations between trials per selected trait. If
#' genotypes are replicated within trials genotypic means are taken before
#' computing correlations. The order of the trials in the heat map is determined
#' by clustering them. Closely related trials will be plotted close to each
#' other.
#'
#' @section Scatter Plot:
#' Draws a scatter plot matrix per selected trait. If genotypes are replicated
#' within trials genotypic means are taken before plotting. The lower left of
#' the matrix contains scatter plots between trials. The diagonal contains
#' histograms of the data per trial.\cr
#' Extra parameter options:
#' \describe{
#' \item{colorGenoBy}{A character string indicating a column in \code{TD} by
#' which the genotypes in the scatter plots are colored.}
#' \item{colGeno}{A character vector with plot colors for the genotypes. A
#' single color when \code{colorGenoBy = NULL}, a vector of colors otherwise.}
#' \item{colorTrialBy}{A character string indicating a column in \code{TD} by
#' which the trials in the histograms are colored.}
#' \item{colTrial}{A character vector with plot colors for the trials. A
#' single color when \code{colorTrialBy = NULL}, a vector of colors otherwise.}
#' \item{trialOrder}{A character vector indicating the order of the trials in
#' the plot matrix (left to right and top to bottom). This vector should be a
#' permutation of all trials plotted.}
#' \item{addCorr}{A character string indicating the position of the correlation
#' between trials displayed in each plot, either "tl" for top left, "tr", for
#' top right, "bl" for bottom left or "br" for bottom right. If \code{NULL},
#' the default, then no correlation is added to the plot.}
#' }
#'
#' @param x An object of class TD.
#' @param ... Extra plot options. Described per plotType in their respective
#' section.
#' @param plotType A single character string indicating which plot should be
#' made. See the sections below for a detailed explanation of the plots.
#' @param trials A character vector indicating which trials to include in the
#' plot.
#' @param traits A character vector indicating for which traits a plot should
#' be made. Ignored if \code{plotType} = "map".
#' @param title A character string used a title for the plot. Note that when
#' a title is specified and multiple plots are created, all plots will get the
#' same title.
#' @param output Should the plot be output to the current device? If
#' \code{FALSE} only a list of ggplot objects is invisibly returned.
#'
#' @family functions for TD objects
#'
#' @examples
#' data("dropsRaw")
#'
#' ## Create a TD object.
#' dropsTD <- createTD(data = dropsRaw[dropsRaw$year == 2012, ],
#'                     genotype = "Variety_ID", trial = "Experiment",
#'                     loc = "Site", repId = "Replicate", subBlock = "block",
#'                     rowCoord = "Row", colCoord = "Column",
#'                     trLat = "Lat", trLong = "Long")
#' ### Layout plot.
#'
#' \donttest{
#' ## Plot the layout of one of the trials.
#' plot(dropsTD, trials = "Kar12W")
#'
#' ## Highlight some of the genotypes in the layout.
#' plot(dropsTD, trials = "Kar12W", highlight = c("A3", "11430"))
#'
#' ### Map plot.
#'
#' ## Plot the location of the trials on the map.
#' plot(dropsTD, plotType = "map")
#'
#' ### Box plot.
#'
#' ## Create a box plot for grain.yield.
#' plot(dropsTD, plotType = "box", traits = "grain.yield")
#'
#' ## Add coloring by scenarioFull to the boxes.
#' plot(dropsTD, plotType = "box", traits = "grain.yield",
#'      colorTrialBy = "scenarioFull")
#'
#' ## Sort the boxes in descending order.
#' plot(dropsTD, plotType = "box", traits = "grain.yield",
#'      orderBy = "descending")
#'
#' ### Correlation plot.
#'
#' ## Plot the correlations between trials for grain.yield.
#' plot(dropsTD, plotType = "cor", traits = "grain.yield")
#'
#' ### Scatter plot
#'
#' ## Plot scatter plot for grain.yield.
#' plot(dropsTD, plotType = "scatter", traits = "grain.yield")
#'
#' ## Create a scatter plot matrix for grain yield.
#' ## Color trials by scenario and genotypes by family.
#' plot(dropsTD, plotType = "scatter", traits = "grain.yield",
#'      colorTrialBy = "scenarioFull", colorGenoBy = "geneticGroup")
#' }
#'
#' @export
plot.TD <- function(x,
                    ...,
                    plotType = c("layout", "map", "box", "cor", "scatter"),
                    trials = names(x),
                    traits = NULL,
                    title = NULL,
                    output = TRUE) {
  ## Checks.
  trials <- chkTrials(trials, x)
  plotType <- match.arg(plotType)
  chkChar(title, len = 1)
  dotArgs <- list(...)
  ## Restrict x to trials.
  x <- dropTD(x, names(x)[!names(x) %in% trials])
  if (plotType == "layout") {
    p <- layoutPlot(x = x, trials = trials, traits = traits, title = title,
                    output = output, ...)
  } else if (plotType == "map") {
    p <- mapPlot(x = x, title = title, output = output,  ...)
  } else if (plotType == "box") {
    p <- boxPlot(x = x, trials = trials, traits = traits, title = title,
                 output = output, ...)
  } else if (plotType == "cor") {
    p <- corPlot(x = x, trials = trials, traits = traits, title = title,
                 output = output, ...)
  } else if (plotType == "scatter") {
    p <- scatterPlot(x = x, trials = trials, traits = traits, title = title,
                     output = output, ...)
  }
  invisible(p)
}

#' Get and set metadata for TD objects
#'
#' Functions for extracting and adding metadata for objects of class TD.\cr\cr
#' \code{getMeta} extracts a data.frame with location, date, design, latitude,
#' longitude, plot width and plot length for all trials in TD.\cr\cr
#' \code{setMeta} adds metadata from a data.frame to an object of class TD. See
#' details for the specifications of the data.frame.\cr\cr
#' The most common use case is extracting metadata from a TD object, modifying
#' the content and then adding it back to the TD object.\cr
#' Information in the metadata of a TD object is used in plotting functions
#' (e.g. latitude and longitude for a map plot) and when fitting models on the
#' data (the trial design).
#'
#' When setting metadata, metadata has to be a data.frame with rownames
#' corresponding to the trials in \code{TD}. The data.frame should contain one
#' or more of the following columns:
#' \describe{
#' \item{trLocation}{The location of the trial. Used as default name when
#' creating plots and summaries.}
#' \item{trDate}{The date of the trial.}
#' \item{trDesign}{The design of the trial. One of "none" (no (known) design),
#' "ibd" (incomplete-block design), "res.ibd" (resolvable incomplete-block
#' design), "rcbd" (randomized complete block design),
#' "rowcol" (row-column design) or "res.rowcol" (resolvable row-column design).
#' Used when fitting models.}
#' \item{trLat}{The latitude of the trial on a scale of -90 to 90. Used when
#' plotting the trials on a map.}
#' \item{trLong}{The longitude of the trial on a scale of -180 to 180. Used
#' when plotting the trials on a map.}
#' \item{trPlWidth}{The width of the plot. Used in combination with trPlLength
#' to determine the size of the plots in a layout plot of a trial.}
#' \item{trPlLength}{The length of the plot. Used in combination with
#' trPlWidth to determine the size of the plots in a layout plot of a trial.}
#' }
#' The values of the metadata of TD will be set to the values in the
#' corresponding column in \code{meta}. Existing values will be overwritten,
#' but \code{NA} will be ignored so setting a value to \code{NA} won't result
#' in accidentally removing it.
#'
#' @param TD An object of class TD.
#'
#' @family functions for TD objects
#'
#' @examples
#' data("dropsRaw")
#'
#' ## Create a TD object.
#' dropsTD <- createTD(data = dropsRaw[dropsRaw$year == 2012, ],
#'                     genotype = "Variety_ID", trial = "Experiment",
#'                     loc = "Site", repId = "Replicate", subBlock = "block",
#'                     rowCoord = "Row", colCoord = "Column",
#'                     trLat = "Lat", trLong = "Long")
#'
#' ## Get meta data from dropsTD.
#' (dropsMeta <- getMeta(dropsTD))
#'
#' ## Add trial date to meta data.
#' dropsMeta$trDate <- as.Date(rep("010112", times = 5), "%d%m%y")
#'
#' ## Add back meta data to wheatTD.
#' dropsTD <- setMeta(dropsTD, dropsMeta)
#'
#' @export
getMeta <- function(TD) {
  if (missing(TD) || !inherits(TD, "TD")) {
    stop("TD should be an object of class TD.\n")
  }
  metaVars <- c("trLocation", "trDate", "trDesign", "trLat", "trLong",
                "trPlWidth", "trPlLength")
  meta <- as.data.frame(matrix(nrow = length(TD), ncol = length(metaVars),
                               dimnames = list(names(TD), metaVars)))
  for (mv in metaVars) {
    meta[mv] <- sapply(X = TD, FUN = function(tr) {
      mvTr <- attr(tr, which = mv)
      ## Replace NULL by NA to ensure correct output format for inserting in df.
      if (!is.null(mvTr)) {
        return(mvTr)
      } else {
        return(NA)
      }
    })
  }
  class(meta$trDate) <- "Date"
  return(meta)
}

#' @param TD An object of class TD.
#' @param meta A data.frame containing metadata.
#'
#' @rdname getMeta
#' @export
setMeta <- function(TD,
                    meta) {
  if (missing(TD) || !inherits(TD, "TD")) {
    stop("TD should be an object of class TD.\n")
  }
  if (missing(meta) || !inherits(meta, "data.frame")) {
    stop("meta should be a data.frame.\n")
  }
  naTr <- rownames(meta)[!rownames(meta) %in% names(TD)]
  if (length(naTr) > 0) {
    warning("The following trials in meta are not in TD: ",
            paste(naTr, collapse = ", "), ".\n", call. = FALSE)
  }
  metaVars <- c("trLocation", "trDate", "trDesign", "trLat", "trLong",
                "trPlWidth", "trPlLength")
  ## Set metadata for trials in meta that are also in TD.
  for (tr in rownames(meta)[rownames(meta) %in% names(TD)]) {
    for (mv in metaVars) {
      mvTr <- meta[tr, mv]
      if (!is.na(mvTr)) {
        chk <- try(do.call(what = checkTDMeta, args = setNames(list(mvTr), mv)),
                   silent = TRUE)
        if (inherits(chk, "try-error")) {
          ## Get message from check function but remove first 8 chars to
          ## prevent having an error text with 3x error in it.
          stop("\nError for ", tr, ":\n", substring(text = chk, first = 9))
        }
        attr(TD[[tr]], which = mv) <- mvTr
      }
    }
  }
  return(TD)
}

#' Function for extracting objects of class TD that keeps class.
#'
#' @noRd
#' @keywords internal
`[.TD` <- function(x, i, ...) {
  r <- NextMethod("[")
  attr(r, "class") <- attr(x, "class")
  return(r)
}

#' Function for concatenating objects of class TD that keeps class.
#'
#' @noRd
#' @keywords internal
c.TD <- function(...) {
  args <- list(...)
  args <- lapply(X = args, FUN = unclass)
  argNames <- unique(unlist(lapply(X = args, FUN = names)))
  r <- do.call("c", args)
  r <- lapply(X = r, FUN = function(trial) {
    trial[["trial"]] <- factor(trial[["trial"]], levels = argNames)
    return(trial)
  })
  class(r) <- c("TD", "list")
  return(r)
}


#' Helper function for checking metadata structure for TD objects.
#'
#' @noRd
#' @keywords internal
checkTDMeta <- function(trLocation = NULL,
                        trDate = NULL,
                        trDesign = NULL,
                        trLat = NULL,
                        trLong = NULL,
                        trPlWidth = NULL,
                        trPlLength = NULL) {
  chkDesign(trDesign)
  chkLatLong(trLat, trLong)
  if (!is.null(trPlWidth) && (!is.numeric(trPlWidth) || any(trPlWidth < 0))) {
    stop("trPlWidth should be a positive numerical vector.\n", call. = FALSE)
  }
  if (!is.null(trPlLength) && (!is.numeric(trPlLength) || any(trPlLength < 0))) {
    stop("trPlLength should be a positive numerical vector.\n", call. = FALSE)
  }
}

#' Helper function for checking design.
#'
#' @noRd
#' @keywords internal
chkDesign <- function(design) {
  match.arg(design, choices = c("none", "ibd", "res.ibd", "rcbd", "rowcol",
                                "res.rowcol"),
            several.ok = TRUE)
}

#' Helper function for checking latitude and longitude.
#'
#' @noRd
#' @keywords internal
chkLatLong <- function(lat,
                       long) {
  if (!is.null(lat) && !is.na(lat) &&
      (!is.numeric(lat) || any(abs(lat) > 90))) {
    stop("lat should be a numerical vector with values between -90 and 90.\n",
         call. = FALSE)
  }
  if (!is.null(long) && !is.na(long) &&
      (!is.numeric(long) || any(abs(long) > 180))) {
    stop("long should be a numerical vector with values between -180 and 180.\n",
         call. = FALSE)
  }
  if (!is.null(lat) && !is.na(lat) && !is.null(long) && !is.na(long)) {
    locLen <- max(length(lat), length(long))
    ## Check that coordinates point to a proper location so plotting can be done.
    loc <- maps::map.where(x = rep(x = long, length.out = locLen),
                           y = rep(x = lat, length.out = locLen))
    if (length(loc) > 0 && anyNA(loc)) {
      warning("Values for latitude and longitude should all match a known ",
              "land location.\n", call. = FALSE)
    }
  }
}

