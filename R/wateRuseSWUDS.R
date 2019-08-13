.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(strwrap(
    "USGS Support Package:
    https://owi.usgs.gov/R/packages.html#support
    \nReport issues and ask questions:
    https://github.com/USGS-R/wateRuse/issues"),
    collapse = "\n"))
}

#' wateRuseSWUDS
#'
#' \tabular{ll}{
#' Package: \tab wateRuseSWUDS\cr
#' Type: \tab Package\cr
#' License: \tab Unlimited for this package, dependencies have more
#' restrictive licensing.\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Collection of functions to help parse and analyze AWUDS data.
#'
#' @name wateRuseSWUDS-package
#' @docType package
NULL


#' Example Water Use Data
#'
#' Example dump file read in as RData format.
#'
#' @name swuds_sample
#' @rdname swuds_sample
#' @examples
#' head(swuds_sample)
NULL
