#' filter_state
#'
#' Allows user to filter their water use data from SWUDS by state(s)
#'
#' @param s.wuds dataframe, the swuds water use data 
#' @param state chr, the state of interest to be filtered
#' 
#' @export
#' 
#' @examples
#' s.wuds <- swuds_sample #example data from Ohio
#' state <- c("Ohio")
#' test_state <- filter_state(s.wuds, state)
#' 
filter_state <- function(s.wuds, state){
  s.wuds <- s.wuds[which(s.wuds$FROM_STATE_NM == state), ]
}
