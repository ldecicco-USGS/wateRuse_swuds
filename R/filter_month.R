#' filter_month
#'
#' Allows user to filter their water use data from SWUDS by month(s)
#'
#' @param s_wuds dataframe, the swuds water use data
#' @param month chr, the month of interest to be filtered in abb format (e.g. Jan, Feb, etc.)
#' 
#' @export
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' month <- c("Jan", "Feb")
#' test_month <- filter_month(s_wuds, month)
filter_month <- function(s_wuds, month){
  s_wuds <- s_wuds[which(s_wuds$months == month), ]
  return(s_wuds)
}
