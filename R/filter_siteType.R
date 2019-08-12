#' filter_site_type
#'
#' Allows user to filter their water use data from SWUDS by site type(s)
#'
#' @param s_wuds dataframe, the swuds water use data 
#' @param site_type chr, the site type of intesitest to be filtered
#' 
#' @export
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' site_type <- c("AS", "FA-DV")
#' test_site_type <- filter_site_type(s_wuds, site_type)
#' 
filter_site_type <- function(s_wuds, site_type){
  s_wuds <- s_wuds[which(s_wuds$FROM_SITE_TP_CD == site_type), ]
}
