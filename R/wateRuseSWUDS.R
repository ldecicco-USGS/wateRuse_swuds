.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(strwrap(
    'USGS Support Package: 
    https://owi.usgs.gov/R/packages.html#support
    Report issues and ask questions:
    https://github.com/USGS-R/wateRuse/issues'),
    collapse='\n'))
}

#' wateRuseSWUDS
#'
#' \tabular{ll}{
#' Package: \tab wateRuseSWUDS\cr
#' Type: \tab Package\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' https://www.usgs.gov/visual-id/credit_usgs.html#copyright\cr
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
#' @name swudsSample
#' @rdname swudsSample
#' @examples 
#' head(swudsSample)
NULL