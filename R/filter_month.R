#' filter_month
#'
#' Allows user to filter their water use data from SWUDS by month(s)
#'
#' @param s.wuds dataframe, the swuds water use data 
#' @param month chr, the month of interest to be filtered in abb format (e.g. Jan, Feb, etc.)
#' 
#' @export
#' @examples
#' s.wuds <- swuds_sample #example data from Ohio
#' month <- c("Jan", "Feb")
#' test_month <- filter_month(s.wuds, month)
#' 
filter_month <- function(s.wuds, month){
  s.wuds <- s.wuds[which(s.wuds$months == month), ]
  return(s.wuds)
}
