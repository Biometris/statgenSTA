#' S3 class Extract
#'
#' Function for creating objects of S3 class Extract.
#'
#' @noRd
#' @keywords internal
createExtract <- function(result,
                          what) {
  extract <- structure(result,
                       class = c("extract", "list"),
                       what = what)
  return(extract)
}

#' Coerce extracted heritabilities to data.frame
#'
#' Function for coercing heritabilities extracted from an SSA object to a
#' data.frame for nicer printing and easier post processing.
#'
#' @param x An object of class extract.
#' @param row.names An optional character vector of row.names to be added to
#' the data.frame.
#' @param optional Ignored.
#' @param ... Ignored.
#'
#' @export
as.data.frame.extract <- function(x,
                                  row.names = NULL,
                                  optional = FALSE,
                                  ...) {
  if ("heritability" %in% attr(x, which = "what")) {
      herit <- sapply(X = x, FUN = `[`, "heritability")
      traits <- unique(unlist(sapply(X = herit, names)))
      heritDF <- data.frame(trial = names(x), row.names = row.names,
                            stringsAsFactors = FALSE)
      for (trait in traits) {
        heritDF[[trait]] <- sapply(herit, `[`, trait)
      }
      return(heritDF)
  } else {
    stop("Conversion to data.frame only possible if heritabilities present.\n")
  }
}
