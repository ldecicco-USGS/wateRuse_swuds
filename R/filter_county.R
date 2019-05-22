#' filter_county
#'
#' Allows user to filter their water use data from SWUDS by county
#'
#' @param s.wuds dataframe, the swuds water use data 
#' @param state_county chr, the county of interested to be filtered. The state abb is appended to
#' the front of the county name
#' 
#' @export
#' @examples
#' s.wuds <- swudsSample #example data from Ohio
#' state_county <- c("Ohio_Madison County", "Ohio_Licking County")
#' test_county <- filter_county(s.wuds, state_county)
#' 
s.wuds$state_county <- paste(s.wuds$FROM_STATE_NM, s.wuds$FROM_COUNTY_NM, sep = "_")

filter_county <- function(s.wuds, state_county){
  s.wuds <- s.wuds[which(s.wuds$state_county == state_county), ]
}