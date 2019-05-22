#' filter_siteType
#'
#' Allows user to filter their water use data from SWUDS by site type(s)
#'
#' @param s.wuds dataframe, the swuds water use data 
#' @param siteType chr, the site type of intesitest to be filtered
#' 
#' @export
#' @examples
#' s.wuds <- swudsSample #example data from Ohio
#' siteType <- c("AS", "FA-DV")
#' test_siteType <- filter_siteType(s.wuds, siteType)
#' 
filter_siteType <- function(s.wuds, siteType){
  s.wuds <- s.wuds[which(s.wuds$FROM_SITE_TP_CD == siteType), ]
}