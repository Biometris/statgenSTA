.onLoad <- function(libname = find.package("statgenSSA"), pkgname = "statgenSSA"){
  # CRAN Note avoidance
  if (getRversion() >= "2.15.1")
    utils::globalVariables(c(".", "..count.."))
  invisible()
}
