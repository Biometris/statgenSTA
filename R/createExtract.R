#' S3 class Extract
#'
#' Function for creating objects of S3 class Extract.
#'
#' @author Bart-Jan van Rossum
#'
#' @family extract
#'
#' @name extractS3
NULL

#' @rdname extractS3
#' @keywords internal
createExtract <- function(result,
                          what) {
  extract <- structure(result,
                       class = c("extract", "list"),
                       what = what)
  return(extract)
}

#' @rdname extractS3
#' @export
as.data.frame.extract <- function(x,
                                  row.names = NULL,
                                  optional = FALSE,
                                  ...) {
  if ("heritability" %in% attr(x, which = "what")) {
      herit <- sapply(X = x, FUN = `[`, "heritability")
      traits <- unique(unlist(sapply(X = herit, names)))
      heritDF <- data.frame(trial = names(x), stringsAsFactors = FALSE)
      for (trait in traits) {
        heritDF[[trait]] <- sapply(herit, `[`, trait)
      }
      return(heritDF)
  } else {
    stop("Conversion to data.frame only possible if heritabilities present.\n")
  }
}
