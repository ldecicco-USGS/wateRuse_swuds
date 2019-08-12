#' filter_year
#'
#' Allows user to filter their water use data from SWUDS by year(s)
#'
#' @param s.wuds dataframe, the swuds water use data 
#' @param year numeric, the year of interest to be filtered
#' 
#' @export
#' @examples
#' s.wuds <- swuds_sample #example data from Ohio
#' year <- c("1995", "2009", "2012")
#' test_year <- filter_year(s.wuds, year)
#' 
filter_year <- function(s.wuds, year){
  s.wuds <- s.wuds[which(s.wuds$YEAR == year), ]
}
