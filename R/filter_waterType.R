#' filter_waterType
#'
#' Allows user to filter their water use data from SWUDS by water type(s)
#'
#' @param s.wuds dataframe, the swuds water use data 
#' @param waterType chr, the water type of interest to be filtered
#' 
#' @export
#' 
#' @examples
#' s.wuds <- swudsSample #example data from Ohio
#' waterType <- "SW"
#' test_waterType <- filter_waterType(s.wuds, waterType)

filter_waterType <- function(s.wuds, waterType){
  s.wuds <- s.wuds[which(s.wuds$`Water Type Code` == waterType), ]
}