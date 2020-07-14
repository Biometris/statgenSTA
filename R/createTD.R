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
#' Otherwise the trialname is used.
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
#' indicating the longitudes of the trials on a scale of -180 to 180. If
#' \code{trLong} is not provided longitude will be taken from \code{long}.
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
#' ## Create a data.frame with to be converted to TD object.
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
#' datT2<- data.frame(geno = paste0("G", 1:10), tr = "T2",
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
  checkTDMeta(trDesign = trDesign, trPlWidth = trPlWidth,
              trPlLength = trPlLength)
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
         "Renaming another column to one of these is imposseble.\n")
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
#' @param trial A character string specifying the trial to be summarised.
#' @param traits A character vector specifying the traits to be summarised.
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
#' \item{firstQ}{The first (25\%) quantile.}
#' \item{thirdQ}{The third (75\%) quantile.}
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
print.summary.TD <- function(x, ...) {
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
#' Plots the layout of the selected trials (all available trials by default).
#' This plot can only be made for trials that contain both row (\code{rowCoord})
#' and column (\code{colCoord}) information. If either one of those is missing
#' the trial is skipped with a warning. If blocks (\code{subBlock}) are
#' available for a trial these are indicated in different colors per block,
#' otherwise all plots are colored in grey. If replicates (\code{repId}) are
#' available a black line is plotted between diffent replicates. Missing plots
#' are indicated in white. This can either be single plots in a trial or
#' complete missing columns or rows.\cr
#' Extra parameter options:
#' \describe{
#' \item{showGeno}{Should individual genotypes be indicated in the plot?
#' Defaults to \code{FALSE}}
#' \item{highlight}{A character vector of genotypes to be highlighted in the
#' plot.}
#' \item{colorSubBlock}{Should subBlocks be colored with a different color per
#' subBlock? Defaults to \code{FALSE}. \code{colorSubBlock} is ignored when
#' highlight is used to highlight genotypes.}
#' }
#'
#' @section Map Plot:
#' A map is plotted with the locations of the trials in the TD object.
#' Mapping the trials is done based on lattitude and longitude that can be
#' added when creating an object of class TD. Trials without latitude and/or
#' longitude available are skipped with a warning message. The countries in
#' which the trials are located will be plotted on a single map and the
#' location of the trials will be indicated on this map. The actual plot is
#' made using ggplot, but for getting the data for the borders of the countries
#' the maps package is needed.\cr
#' Extra parameter options:
#' \describe{
#' \item{colorTrialBy}{A character string indicating a column in \code{TD} by
#' which the trials on the map are colored.}
#' \item{colTrial}{A character vector with plot colors for the trials. A
#' single color when \code{colorTrialBy = NULL}, a vector of colors otherwise.}
#' \item{minLatRange}{A positive numerical value indicating the minimum range
#' (in degrees) for the latitude on the plotted map. Defaults to 10.}
#' \item{minLongRange}{A positive numerical value indicating the minimum range
#' (in degrees) for the longitud on the plotted map. Defaults to 5.}
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
#' Draws a heatmap of correlations between trials per selected trait. If
#' genotypes are replicated within trials genotypic means are taken before
#' computing correlations. The order of the trials in the heatmap is determined
#' by clustering them.
#'
#' @section Scatter Plot:
#' Draws a scatter plot matrix per selected trait. If genotypes are replicated
#' within trials genotypic means are taken before plotting. The lower left of
#' the matix contains scatter plots between trials. The diagonal contains
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
#' diplayed in each plot, either "tl" for top left, "tr", for top right, "bl"
#' for bottom left or "br" for bottom right. If \code{NULL}, the default, then
#' no correlation is added to the plot.}
#' }
#'
#' @param x An object of class TD.
#' @param ... Extra plot options. Described per plotType in their respective
#' section.
#' @param plotType A single character string indicating which plot should be
#' made. See the sections below for a detailed explanation of the plots.
#' @param trials A character vector indicating the trials to be plotted when
#' plotting field layouts. Only used if \code{plotType} = "layout" or "box".
#' @param traits A character vector indicating the traits to be plotted in
#' a boxplot. Only used if \code{plotType} = "box" or "cor".
#' @param title A character string used a title for the plot. Note that when
#' a title is specified and multiple plots are created, all plots will get the
#' same title.
#' @param output Should the plot be output to the current device? If
#' \code{FALSE} only a list of ggplot objects is invisibly returned.
#'
#' @family functions for TD objects
#'
#' @examples
#' data("wheatChl")
#'
#' ## Create a TD object.
#' wheatTD <- createTD(data = wheatChl, genotype = "trt", repId = "rep",
#'                     subBlock = "bl", rowCoord = "row", colCoord = "col")
#'
#' ## Add meta data to be able to plot locations on a map.
#' wheatMeta <- getMeta(wheatTD)
#' wheatMeta$trLocation <- c("Cauquenes", rep("Santa Rosa", times = 4))
#' wheatMeta$trLat <- c(-35.58, rep(-36.32, times = 4))
#' wheatMeta$trLong <- c(-72.17, rep(-71.55, times = 4))
#' wheatTD <- setMeta(wheatTD, wheatMeta)
#'
#' ### Layout plot.
#'
#' ## Plot the layout of one of the trials.
#' plot(wheatTD, trials = "C_SWS_12")
#'
#' ## Highlight some of the genotypes in the layout.
#' plot(wheatTD, trials = "C_SWS_12", highlight = c("G001", "G002"))
#'
#' ### Map plot.
#'
#' ## Plot the location of the trials on the map.
#' plot(wheatTD, plotType = "map")
#'
#' ### Box plot.
#'
#' ## Create a box plot for GY.
#' plot(wheatTD, plotType = "box", traits = "GY")
#'
#' ## Add coloring by repId to the boxes.
#' plot(wheatTD, plotType = "box", traits = "GY", colorTrialBy = "repId")
#'
#' ## Sort the boxes in descending order.
#' plot(wheatTD, plotType = "box", traits = "GY", orderBy = "descending")
#'
#' ### Correlation plot.
#'
#' ## Plot the correlations between trials for GY.
#' plot(wheatTD, plotType = "cor", traits = "GY")
#'
#' ### Scatter plot
#'
#' ## Plot scatter plot for GY.
#' plot(wheatTD, plotType = "scatter", traits = "GY")
#'
#' ## Add correlations to top left corner of plots.
#' plot(wheatTD, plotType = "scatter", traits = "GY", addCorr = "tl")
#'
#' @importFrom grDevices hcl.colors hcl.pals
#' @importFrom utils combn
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
      ## Compute min and max for row and column coordinates.
      yMin <- min(trDat[["rowCoord"]])
      yMax <- max(trDat[["rowCoord"]])
      xMin <- min(trDat[["colCoord"]])
      xMax <- max(trDat[["colCoord"]])
      ## Create data.frame with all rows columns in field.
      ## Full missing rows/columns are included.
      ## If not geom_tile just fills the empty columns by expanding the
      ## neighbouring colums (or rows).
      fullGrid <- expand.grid(colCoord = xMin:xMax, rowCoord = yMin:yMax)
      trDat <- merge(fullGrid, trDat, all.x = TRUE)
      trDat[!is.na(trDat[["rowId"]]), "color."] <- "grey75"
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
        title <- trLoc
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
        ggplot2::ggtitle(title)
      if (sum(!is.na(trDat[["highlight."]])) > 0) {
        ## Genotypes to be highlighted get a color.
        ## Everything else the NA color.
        pTr <- pTr +
          ggplot2::geom_tile(ggplot2::aes_string(fill = "highlight.",
                                                 color = "color.")) +
          ggplot2::scale_color_manual(values = "grey75", na.translate = FALSE,
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
          ggplot2::scale_color_manual(values = "grey75", na.translate = FALSE,
                                      na.value = "transparant") +
          ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3), color = "none")
      } else {
        ## No subblocks and no hightlights so just a single fill color.
        pTr <- pTr +
          ggplot2::geom_tile(ggplot2::aes_string(color = "color."),
                             fill = "white") +
          ggplot2::scale_color_manual(values = "grey75", na.translate = FALSE,
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
                                data = subBlockBord$vertW, size = 0.4) +
          ggplot2::geom_segment(ggplot2::aes_string(x = "x - 0.5",
                                                    xend = "x + 0.5",
                                                    y = "y - 0.5",
                                                    yend = "y - 0.5"),
                                data = subBlockBord$horW, size = 0.4)
      }
      if (showGeno) {
        ## Add names of genotypes to the center of the tiles.
        pTr <- pTr +
          ggplot2::geom_text(ggplot2::aes_string(label = "genotype"),
                             size = 2, check_overlap = TRUE)
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
                                data = repBord$vertW, size = 1) +
          ggplot2::geom_segment(ggplot2::aes_string(x = "x - 0.5",
                                                    xend = "x + 0.5",
                                                    y = "y - 0.5",
                                                    yend = "y - 0.5"),
                                data = repBord$horW, size = 1)
      }
      if (plotSubBlock || plotRep) {
        shwVals <- c(plotRep, plotSubBlock)
        pTr <- pTr +
          ## Add a legend entry for replicates and subBlocks.
          ggplot2::scale_linetype_manual(c("replicates", "subBlocks")[shwVals],
                                         values = c("replicates" = "solid",
                                                    "subBlocks" = "solid")[shwVals],
                                         name = ggplot2::element_blank()) +
          ggplot2::guides(linetype = ggplot2::guide_legend(override.aes =
                                                             list(size = c(1, 0.4)[shwVals])))
      }
      p[[trial]] <- pTr
      if (output) {
        plot(pTr)
      }
    }
  } else if (plotType == "map") {
    ## Checks for colorTrialBy.
    colorTrialBy <- dotArgs$colorTrialBy
    colTrial <- dotArgs$colTrial
    chkChar(colTrial)
    if (!is.null(colorTrialBy)) {
      chkChar(colorTrialBy, len = 1, null = FALSE)
      if (!all(sapply(X = x, FUN = function(trial) {
        hasName(x = trial, name = colorTrialBy)
      }))) {
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
      colorTrialDat <- unique(do.call(rbind, lapply(X = x, FUN = `[`, "trial")))
      colorTrialDat[[".colorTrialBy"]] <- factor(1)
      colorTrialBy <- ".colorTrialBy"
    }
    colorTrialDat <- droplevels(colorTrialDat)
    nColTrial <- nlevels(colorTrialDat[[colorTrialBy]])
    if (length(colTrial) == 0) {
      ## Defaults to black for one color for trials.
      ## For more than one colors from statgen.trialColors are used.
      ## Fall back to topo.colors if number of colors in option is too small.
      if (nColTrial == 1) {
        colTrial <- "red"
      } else if (length(getOption("statgen.trialColors")) >= nColTrial) {
        colTrial <- getOption("statgen.trialColors")[1:nColTrial]
      } else {
        colTrial <- topo.colors(nColTrial)
      }
    } else {
      nColTrialArg <- length(colTrial)
      if (nColTrialArg != nColTrial) {
        stop("Number of colors provided doesn't match number of trial ",
             "groups:\n", nColTrialArg, " colors provided, ", nColTrial,
             " groups in data.\n")
      }
    }
    ## Check for latitude and longitude.
    minLatRange <- dotArgs$minLatRange
    minLongRange <- dotArgs$minLongRange
    if (!is.null(minLatRange) && (!is.numeric(minLatRange) ||
                                  length(minLatRange) > 1)) {
      stop("minLatRange should be a single numerical value.\n")
    }
    if (!is.null(minLongRange) && (!is.numeric(minLongRange) ||
                                   length(minLongRange) > 1)) {
      stop("minLongRange should be a single numerical value.\n")
    }
    if (is.null(minLatRange)) {
      minLatRange <- 10
    }
    if (is.null(minLongRange)) {
      minLongRange <- 5
    }
    ## Create a data.frame for plotting trials.
    ## Population has a random value but if left out nothing is plotted.
    locs <- setNames(getMeta(x)[c("trLocation", "trLat", "trLong")],
                     c("name", "lat", "long"))
    locs[["trial"]] <- rownames(locs)
    ## Merge groups for coloring text.
    locs <- merge(locs, colorTrialDat, by.x = "row.names", by.y = "trial")
    if (any(table(unique(locs[c("name", colorTrialBy)])) > 1)) {
      stop("colorTrialBy should be unique within locations.\n")
    }
    ## Drop Row.names column created when merging.
    locs <- unique(locs[!is.na(locs[["lat"]]) & !is.na(locs[["long"]]),
                        c("name", "lat", "long", colorTrialBy)])
    if (nrow(locs) == 0) {
      stop("At least one trial should have latitude and longitude ",
           "for plotting on map.\n")
    }
    ## Set minimum range for latitude and longitude.
    latR <- range(locs$lat)
    latR <- latR +
      (diff(latR) < minLatRange) * c(-1, 1) * (minLatRange - diff(latR)) / 2
    longR <- range(locs$long)
    longR <- longR +
      (diff(longR) < minLongRange) * c(-1, 1) * (minLongRange - diff(longR)) / 2
    ## Add 10% to edges of map so locations are not on the absolute edge.
    longR <- longR + c(-0.1, 0.1) * diff(longR)
    latR <- latR + c(-0.1, 0.1) * diff(latR)
    ## Create data usable by ggplot geom_polygon.
    mapDat <- mapData(xLim = longR, yLim = latR)
    ## Set text options in a list to be able to specify color groups and
    ## red as default color.
    textArgs <- list(mapping = ggplot2::aes_string(label = "name",
                                                   color = colorTrialBy),
                     data = locs, size = 3, nudge_x = 0.01 * diff(longR),
                     nudge_y = 0.04 * diff(latR))
    if (is.null(title)) {
      title <- "Trial locations"
    }
    p <- ggplot2::ggplot(mapDat, ggplot2::aes_string(x = "long", y = "lat")) +
      ggplot2::geom_polygon(ggplot2::aes_string(group = "group"), fill = "white",
                            color = "black") +
      ## Add a proper map projection.
      ggplot2::coord_map(clip = "on", xlim = longR, ylim = latR) +
      ## Add trial locations.
      ggplot2::geom_point(data = locs) +
      ggrepel::geom_text_repel(mapping = ggplot2::aes_string(label = "name",
                                                             color = colorTrialBy),
                               data = locs, size = 3,
                               nudge_x = 0.01 * diff(longR),
                               nudge_y = 0.04 * diff(latR),
                               show.legend = colorTrialBy != ".colorTrialBy") +
      ggplot2::scale_color_manual(values = colTrial) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     ## Empty space left represents water areas. Color blue.
                     panel.background = ggplot2::element_rect(fill = "steelblue2")) +
      ggplot2::ggtitle(title)
    if (output) {
      plot(p)
    }
  } else if (plotType == "box") {
    chkChar(traits, null = FALSE)
    groupBy <- dotArgs$groupBy
    if (!is.null(groupBy)) {
      chkChar(groupBy, len = 1, null = FALSE)
    }
    if (!is.null(groupBy) && !all(sapply(X = x, FUN = function(trial) {
      hasName(x = trial, name = groupBy)
    }))) {
      stop("groupBy should be a column in TD.\n")
    }
    colorTrialBy <- dotArgs$colorTrialBy
    if (!is.null(colorTrialBy)) {
      chkChar(colorTrialBy, len = 1, null = FALSE)
    }
    if (!is.null(colorTrialBy) && !all(sapply(X = x, FUN = function(trial) {
      hasName(x = trial, name = colorTrialBy)
    }))) {
      stop("colorTrialBy should be a column in TD.\n")
    }
    colTrial <- dotArgs$colTrial
    chkChar(colTrial)
    orderBy <- dotArgs$orderBy
    if (!is.null(orderBy)) {
      orderBy <- match.arg(orderBy, choices = c("alphabetic", "ascending",
                                                "descending"))
    } else {
      orderBy <- "alphabetic"
    }
    p <- setNames(vector(mode = "list", length = length(traits)), traits)
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
      if (is.null(plotDat)) {
        warning(trait, " isn't a column in any of the trials.\n",
                "Plot skipped.\n", call. = FALSE)
        next
      }
      if (is.null(colorTrialBy)) {
        plotDat[[".colorTrialBy"]] <- factor(1)
        colorTrialBy <- ".colorTrialBy"
      }
      ## colorTrialBy is ignored in plot if it is not a factor.
      if (!is.null(colorTrialBy) && !is.factor(plotDat[colorTrialBy])) {
        plotDat[colorTrialBy] <- factor(plotDat[[colorTrialBy]])
      }
      nColTrial <- nlevels(plotDat[[colorTrialBy]])
      if (length(colTrial) == 0) {
        ## Defaults to darkgrey for one color for trials.
        ## For more than one colors from statgen.trialColors are used.
        ## Fall back to topo.colors if number of colors in option is too small.
        if (nColTrial == 1) {
          colTrial <- "darkgrey"
        } else if (length(getOption("statgen.trialColors")) >= nColTrial) {
          colTrial <- getOption("statgen.trialColors")[1:nColTrial]
        } else {
          colTrial <- topo.colors(nColTrial)
        }
      } else {
        nColTrialArg <- length(colTrial)
        if (nColTrialArg != nColTrial) {
          stop("Number of colors provided doesn't match number of trial ",
               "groups:\n", nColTrialArg, " colors provided, ", nColTrial,
               " groups in data.\n")
        }
      }
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
      if (is.null(title)) {
        title <- paste("Boxplot for", trait)
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
        ggplot2::labs(x = xVar, y = trait, title = title)
      p[[trait]] <- pTr
      if (output) {
        plot(pTr)
      }
    }
  } else if (plotType == "cor") {
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
      plotDat <- Reduce(f = rbind, x = lapply(X = x, FUN = function(trial) {
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
      ## Get number of observation on which correlation will be based.
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
        warning(sapply(X = 1:nWarn, FUN = function(i) {
          paste("The correlation between trials", corWarn[i, 1], "and",
                corWarn[i, 2], "was calculated with only", corWarn[i, 3],
                "genoypes.\n")
        }), call. = FALSE)
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
        stop("There are trials with no common genotypes. Clustering impossible.\n")
      }
      ## Remove rows and columns with only NA.
      corKeep <- sapply(X = 1:ncol(corMat), FUN = function(i) {
        any(!is.na(corMat[, i]))
      })
      corMat <- corMat[corKeep, corKeep, drop = FALSE]
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
      ## Select bottom right triangle for correlations and top for variances.
      meltedCorMatLow <- meltedCorMat[as.numeric(meltedCorMat[["trial1"]]) >
                                        as.numeric(meltedCorMat[["trial2"]]), ]
      if (is.null(title)) {
        title <- paste("Correlations of trials for", trait)
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
        ggplot2::labs(title = title, x = "", y = "", fill = "") +
        ## Equal coordinates to get a square sized plot.
        ggplot2::coord_equal()
      p[[trait]] <- pTr
      if (output) {
        plot(pTr)
      }
    }
  } else if (plotType == "scatter") {
    if (length(trials) == 1) {
      stop("At least two trials are requiered for a scatter plot.\n")
    }
    chkChar(traits, null = FALSE)
    colorGenoBy <- dotArgs$colorGenoBy
    if (!is.null(colorGenoBy)) {
      chkChar(colorGenoBy, len = 1, null = FALSE)
    }
    if (!is.null(colorGenoBy) && !all(sapply(X = x, FUN = function(trial) {
      hasName(x = trial, name = colorGenoBy)
    }))) {
      stop("colorGenoBy should be a column in TD.\n")
    }
    colGeno <- dotArgs$colGeno
    chkChar(colGeno)
    colorTrialBy <- dotArgs$colorTrialBy
    if (!is.null(colorTrialBy)) {
      chkChar(colorTrialBy, len = 1, null = FALSE)
    }
    if (!is.null(colorTrialBy) && !all(sapply(X = x, FUN = function(trial) {
      hasName(x = trial, name = colorTrialBy)
    }))) {
      stop("colorTrialBy should be a column in TD.\n")
    }
    colTrial <- dotArgs$colTrial
    chkChar(colTrial)
    trialOrder <- dotArgs$trialOrder
    if (!is.null(trialOrder) &&
        (!all(trialOrder %in% trials) || !all(trials %in% trialOrder))) {
      stop("trials and trialOrder should contain exactly the same trials.\n")
    }
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
    colorTrialGroups <- levels(colorTrialDat[[2]])
    nColTrial <- length(colorTrialGroups)
    if (length(colTrial) == 0) {
      ## Defaults to black for one color for trials.
      ## For more than one colors from statgen.trialColors are used.
      ## Fall back to topo.colors if number of colors in option is too small.
      if (nColTrial == 1) {
        colTrial <- "grey50"
      } else if (length(getOption("statgen.trialColors")) >= nColTrial) {
        colTrial <- getOption("statgen.trialColors")[1:nColTrial]
      } else {
        colTrial <- topo.colors(nColTrial)
      }
    } else {
      nColTrialArg <- length(colTrial)
      if (nColTrialArg != nColTrial) {
        stop("Number of colors provided doesn't match number of trial ",
             "groups:\n", nColTrialArg, " colors provided, ", nColTrial,
             " groups in data.\n")
      }
    }
    colorTrialColors <- setNames(colTrial, colorTrialGroups)
    histCols <- setNames(colorTrialColors[match(colorTrialDat[[2]],
                                                colorTrialGroups)],
                         make.names(paste0("t", colorTrialDat[[1]])))
    p <- setNames(vector(mode = "list", length = length(traits)), traits)
    for (trait in traits) {
      ## Create plot title.
      if (is.null(title)) {
        title <- paste("Scatterplots of trials for", trait)
      }
      ## Create a single data.frame from x with only columns genotype, trial
      ## and trait.
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
      if (!is.null(colorGenoBy) && !is.factor(plotDat[[colorGenoBy]])) {
        plotDat[[colorGenoBy]] <- as.factor(plotDat[[colorGenoBy]])
      }
      if (!is.null(trialOrder)) {
        ## Reorder trials.
        ## First restrict reordering to trials left after removing NA trials.
        trialOrderTr <- trialOrder[trialOrder %in% levels(plotDat[["trial"]])]
        plotDat[["trial"]] <- factor(plotDat[["trial"]], trialOrderTr)
      }
      ## Create table with values trait per genotype per trial.
      ## If TD already contains BLUEs/BLUPs taking means doesn't do anything
      ## but it is needed for raw data where there can be replicates.
      plotTab <- as.data.frame(tapply(plotDat[[trait]],
                                      INDEX = list(plotDat[["genotype"]],
                                                   plotDat[["trial"]]),
                                      FUN = mean, na.rm = TRUE))
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
        ## Using Inf and -Inf for positions independent of scale.
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
                        ggplot2::aes_string(x = trial,
                                            y = "(..count..)/sum(..count..)")) +
          ggplot2::geom_histogram(na.rm = TRUE, binwidth = binWidth,
                                  boundary = 0, fill = histCols[trial],
                                  color = "grey50") +
          ggplot2::scale_x_continuous(limits = range(plotTab, na.rm = TRUE)) +
          ggplot2::theme(panel.background = ggplot2::element_blank(),
                         panel.grid = ggplot2::element_blank(),
                         panel.border = ggplot2::element_rect(color = "black",
                                                              fill = NA))
      })
      ## Y-axis should be the same for all histograms.
      ## Build histograms and extract axis information.
      yMax <- max(sapply(X = histPlots, FUN = function(hp) {
        max(ggplot2::ggplot_build(hp)$data[[1]][["ymax"]])
      }))
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
      ## Create a facet plot containing only scatter plots.
      nColGeno <- nlevels(plotTab[[colorGenoBy]])
      if (length(colGeno) == 0) {
        ## Defaults to darkgrey for one color for genotypes.
        ## For more than one colors from statgen.genoColors are used.
        ## Fall back to topo.colors if number of colors in option is too small.
        if (nColGeno == 1) {
          colGeno <- "darkgrey"
        } else if (length(getOption("statgen.genoColors")) >= nColGeno) {
          colGeno <- getOption("statgen.genoColors")[1:nColGeno]
        } else {
          colGeno <- topo.colors(nColGeno)
        }
      } else {
        nColGenoArg <- length(colGeno)
        if (nColGenoArg != nColGeno) {
          stop("Number of colors provided doesn't match number of genotype groups:",
               "\n", nColGenoArg, " colors provided, ", nColGeno,
               " groups in data.\n")
        }
      }
      scatterBase <-
        ggplot2::ggplot(data = plotTab,
                        ggplot2::aes_string(x = paste0(trait, ".x"),
                                            y = paste0(trait, ".y"),
                                            color = paste0("`", colorGenoBy, "`"))) +
        ggplot2::geom_point(na.rm = TRUE, shape = 1,
                            show.legend = colorGenoBy != ".colorGenoBy") +
        ggplot2::scale_color_manual(values = colGeno) +
        ggplot2::scale_x_continuous(breaks = scales::breaks_extended(n = 3)) +
        ggplot2::scale_y_continuous(breaks = scales::breaks_extended(n = 3)) +
        ggplot2::facet_grid(facets = c("trial.y", "trial.x")) +
        ggplot2::labs(title = title, x = "", y = "") +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       aspect.ratio = 1,
                       panel.background = ggplot2::element_rect(fill = "white"),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = "black",
                                                            fill = NA))
      if (colorTrialBy != ".colorTrialBy") {
        scatterBase <- scatterBase +
          ggplot2::geom_point(ggplot2::aes_string(fill = colorTrialBy),
                              color = NA, na.rm = TRUE) +
          ggplot2::scale_fill_discrete(labels = names(colorTrialColors)) +
          ggplot2::guides(fill = ggplot2::guide_legend(override.aes =
                                                         list(color = colorTrialColors)))
      }
      if (!is.null(addCorr)) {
        ## Add correlation annotated in the corner of the plot.
        scatterBase <- scatterBase +
          ggplot2::geom_text(data = meltedCorMat,
                             ggplot2::aes_string(x = "x", y = "y",
                                                 label = "paste('rho ==', round(cor, 2))"),
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
      nullPanels <- panels[sapply(X = splitPanels, FUN = function(pan) {
        as.numeric(pan[2]) < as.numeric(pan[3])
      })]
      for (np in nullPanels) {
        scatterGrob$grobs[[which(scatterGrob$layout$name == np)]] <-
          ggplot2::zeroGrob()
      }
      ## Set diagonal panels to histograms calculated before.
      histPanels <- panels[sapply(X = splitPanels, FUN = function(pan) {
        as.numeric(pan[2]) == as.numeric(pan[3])
      })]
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
  }
  invisible(p)
}

#' Get and set metadata for TD objects
#'
#' Functions for extracting and adding metadata for objects of class TD.\cr
#' \code{getMeta} extracts a data.frame with location, date, design, latitude,
#' longitude, plotWidth and plotLength for all trials in TD.\cr\cr
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
#' data("wheatChl")
#'
#' ## Create a TD object.
#' wheatTD <- createTD(data = wheatChl, genotype = "trt", repId = "rep",
#'                     subBlock = "bl", rowCoord = "row", colCoord = "col")
#'
#' ## Get meta data from wheatTD
#' (wheatMeta <- getMeta(wheatTD))
#'
#' ## Add location names and latitude/longitude to meta data.
#' wheatMeta$trLocation <- c("Cauquenes", rep("Santa Rosa", times = 4))
#' wheatMeta$trLat <- c(-35.58, rep(-36.32, times = 4))
#' wheatMeta$trLong <- c(-72.17, rep(-71.55, times = 4))
#'
#' ## Add back meta data to wheatTD.
#' wheatTD <- setMeta(wheatTD, wheatMeta)
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

#' Function for extracting for objects of class TD that keeps class.
#'
#' @noRd
#' @keywords internal
`[.TD` <- function(x, i, ...) {
  r <- NextMethod("[")
  attr(r, "class") <- attr(x, "class")
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
  if (!is.null(lat) && (!is.numeric(lat) || any(abs(lat) > 90))) {
    stop("lat should be a numerical vector with values between -90 and 90.\n",
         call. = FALSE)
  }
  if (!is.null(long) && (!is.numeric(long) || any(abs(long) > 180))) {
    stop("long should be a numerical vector with values between -180 and 180.\n",
         call. = FALSE)
  }
  if (!is.null(lat) && !is.null(long)) {
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

