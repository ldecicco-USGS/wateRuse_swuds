#' filter_year
#'
#' Allows user to filter their water use data from SWUDS by year(s)
#'
#' @param s_wuds dataframe, the swuds water use data
#' @param year numeric, the year of interest to be filtered
#' 
#' @export
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' year <- c("1995", "2009", "2012")
#' test_year <- filter_year(s_wuds, year)
filter_year <- function(s_wuds, year){
  s_wuds <- s_wuds[which(s_wuds$YEAR == year), ]
}
