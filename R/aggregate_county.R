#' aggregate_county
#'
#' Allows user to aggregate their data from SWUDS by user-defined county(ies)
#'
#' @param s_wuds dataframe, the swuds water use data 
#' @param state_county chr, the county of interest to be aggregated. State abbreviation is also
#' appended to the beginning of the county
#' @param sum_avg chr, user selects either "sum" or "avg" and the sum or average will be computed
#' 
#' @export
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' state_county <- "39033"
#' sum_avg <- "sum"
#' test_county <- aggregate_county(s_wuds, state_county, sum_avg)
#' 
aggregate_county <- function(s_wuds, state_county, sum_avg){
  s_wuds$state_county <- paste0(s_wuds$FROM_STATE_CD, s_wuds$FROM_COUNTY_CD)
  s_wuds <- s_wuds[s_wuds$state_county %in% state_county, ]
  s_wuds$Volume_mgd <- as.numeric(s_wuds$Volume_mgd)
  if (sum_avg == "sum"){
    s_wuds <- aggregate(as.numeric(s_wuds$Volume_mgd),
                        by = list(state_county = s_wuds$state_county),
                        FUN = sum, na.rm = TRUE)
  } else if (sum_avg == "avg"){
    s_wuds <- aggregate(as.numeric(s_wuds$Volume_mgd),
                        by = list(state_county = s_wuds$state_county),
                        FUN = mean, na.rm = TRUE)
  }
  return(s_wuds)
}
