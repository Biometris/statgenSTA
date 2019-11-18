#' Check that x is numerical within a given range.
#'
#' @noRd
#' @keywords internal
chkNum <- function(x,
                   min = NULL,
                   max = NULL,
                   null = TRUE,
                   incl = FALSE) {
  if ((is.null(x) && !null) ||
      (!is.null(x) && (length(x) > 1 || !is.numeric(x) ||
                       isTRUE(incl & x < min) ||
                       isTRUE(!incl & x <= min) ||
                       isTRUE(incl & x > max) ||
                       isTRUE(!incl & x >= max)))) {
    if (null) {
      nullTxt <- " NULL or "
    } else {
      nullTxt <- " "
    }
    if (!is.null(min) && !is.null(max)) {
      txt <- paste(" between", min, "and", max)
    } else if (!is.null(min)) {
      txt <- paste0(" greater than ", if (incl) "or equal to ", min)
    } else if (!is.null(max)) {
      txt <- paste0(" smaller than ", if (incl) "or equal to ", max)
    } else {
      txt <- ""
    }
    stop(match.call()$x, " should be", nullTxt, "a single numerical value",
         txt, ".\n")
  }
}

#' Check that x is a character vector of given length.
#'
#' @noRd
#' @keywords internal
chkChar <- function(x,
                    len = NULL,
                    null = TRUE) {
  if ((is.null(x) && !null) ||
      (!is.null(x) && (!is.character(x) || isTRUE(length(x) > len)))) {
    if (null) {
      nullTxt <- " NULL or "
    } else {
      nullTxt <- " "
    }
    if (is.null(len)) {
      txt <- "vector"
    } else {
      txt <- "string"
    }
    stop(match.call()$x, " should be", nullTxt, "a character ", txt, ".\n")
  }
}

#' Check that trials are within an object.
#'
#' @noRd
#' @keywords internal
chkTrials <- function(trials,
                      obj) {
  if (is.null(trials)) {
    trials <- names(obj)
  } else if (!is.character(trials) || !all(hasName(x = obj, name = trials))) {
    stop("trials has to be a character vector defining trials in ",
         deparse(do.call(substitute, list(expr = substitute(obj),
                                          env = parent.frame()))), ".\n")
  }
  return(trials)
}

#' Check that traits are within an object for a given trial.
#' Either throw an error or give a warning depending on err.
#'
#' @noRd
#' @keywords internal
chkTraits <- function(traits,
                      trial,
                      obj,
                      err = TRUE) {
  if (is.null(traits)) {
    traits <- obj$traits
  } else {
    errTraits <- traits[!traits %in% obj$traits]
    if (length(errTraits) > 0) {
      if (err) {
        stop("The following traits are not modeled for ", trial, ": ",
             paste(errTraits, collapse = ","), "\n")
      } else {
        warning("The following traits are not modeled for ", trial, ": ",
                paste(errTraits, collapse = ","), "\n", trial, " skipped.",
                call. = FALSE)
        traits <- NULL
      }
    }
  }
  return(traits)
}

#' Check that row and column information is available in data.frame.
#' Columns should be present and cannot contain NA values.
#'
#' @noRd
#' @keywords internal
chkRowCol <- function(dat) {
  trial <- dat[["trial"]][1]
  rowCol <- TRUE
  if (!"rowCoord" %in% colnames(dat)) {
    rowCol <- FALSE
    warning("rowCoord should be a column in ", trial, ".\n",
            "Plot skipped.\n", call. = FALSE)
  } else if (sum(is.na(dat[["rowCoord"]])) > 0) {
    rowCol <- FALSE
    warning("rowCoord contains missing values for ", trial, ".\n",
            "Plot skipped.\n", call. = FALSE)
  }
  if (!"colCoord" %in% colnames(dat)) {
    rowCol <- FALSE
    warning("colCoord should be a column in ", trial, ".\n",
            "Plot skipped.\n", call. = FALSE)
  } else if (sum(is.na(dat[["colCoord"]])) > 0) {
    rowCol <- FALSE
    warning("colCoord contains missing values for ", trial, ".\n",
            "Plot skipped.\n", call. = FALSE)
  }
  return(rowCol)
}
