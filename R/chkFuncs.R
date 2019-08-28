#' Check that x is numerical within a given range.
#'
#' @noRd
#' @keywords internal
chkNum <- function(x,
                   min = NULL,
                   max = NULL) {
  if (missing(x) || length(x) > 1 || !is.numeric(x) || isTRUE(x < min) ||
      isTRUE(x > max)) {
    if (!is.null(min) && !is.null(max)) {
      txt <- paste(" between", min, "and", max)
    } else if (!is.null(min)) {
      txt <- paste(" greater than", min)
    } else if (!is.null(max)) {
      txt <- paste(" smaller than", max)
    } else {
      txt <- ""
    }
    stop(match.call()$x, " should be a single numerical value", txt, ".\n",
         call. = FALSE)
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
                                          env = parent.frame()))), ".\n",
         call. = FALSE)
  }
  return(trials)
}
