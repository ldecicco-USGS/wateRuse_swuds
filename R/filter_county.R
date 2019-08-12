#' filter_county
#'
#' Allows user to filter their water use data from SWUDS by county
#'
#' @param s_wuds dataframe, the swuds water use data
#' @param state_county chr, the county of interested to be filtered. The state abb is appended to
#' the front of the county name
#' 
#' @export
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' state_county <- c("Ohio_Madison County", "Ohio_Licking County")
#' test_county <- filter_county(s_wuds, state_county)
filter_county <- function(s_wuds, state_county){
  s_wuds$state_county <- paste(s_wuds$FROM_STATE_NM,
                               s_wuds$FROM_COUNTY_NM, sep = "_")
  s_wuds <- s_wuds[which(s_wuds$state_county == state_county), ]
  return(s_wuds)
}
