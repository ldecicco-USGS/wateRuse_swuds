#' filter_water_type
#'
#' Allows user to filter their water use data from SWUDS by water type(s)
#'
#' @param s.wuds dataframe, the swuds water use data 
#' @param water_type chr, the water type of interest to be filtered
#' 
#' @export
#' 
#' @examples
#' s.wuds <- swuds_sample #example data from Ohio
#' water_type <- "SW"
#' test_water_type <- filter_water_type(s.wuds, water_type)

filter_water_type <- function(s.wuds, water_type){
  s.wuds <- s.wuds[which(s.wuds$`Water Type Code` == water_type), ]
}
