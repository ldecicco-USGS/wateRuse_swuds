#' filter_state
#'
#' Allows user to filter their water use data from SWUDS by state(s)
#'
#' @param s_wuds dataframe, the swuds water use data
#' @param state chr, the state of interest to be filtered
#' @export
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' state <- c("Ohio")
#' test_state <- filter_state(s_wuds, state)
filter_state <- function(s_wuds, state){
  s_wuds <- s_wuds[which(s_wuds$FROM_STATE_NM == state), ]
}
