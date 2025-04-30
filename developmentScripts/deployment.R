covr::package_coverage(function_exclusions = c("extract", "extractAsreml", "extractSpATS",
                                     "extractLme4", "createExtract",
                                     "as.data.frame.extract"))



devtools::build_readme()
pkgdown::clean_site()
pkgdown::build_site()

devtools::check_win_devel()
devtools::check_mac_release()

rhub::check_for_cran(env_vars = c(`_R_CHECK_FORCE_SUGGESTS_` = "false"),
                     path = "C:/Projects/R_packages/statgenSTA")

## Check reverse dependencies
## First build tar.gz and copy to revdep folder.
# result <- tools::check_packages_in_dir("revdep", revdep = list())
# result

devtools::release()

