#' aggregate_county
#'
#' Allows user to aggregate their data from SWUDS by user-defined county(ies)
#'
#' @param s.wuds dataframe, the swuds water use data 
#' @param state_county chr, the county of interest to be aggregated. State abbreviation is also
#' appended to the beginning of the county
#' @param sum_avg chr, user selects either "sum" or "avg" and the sum or average will be computed
#' 
#' @examples
#' s.wuds <- swudsSample #example data from Ohio
#' HUC <- c("041000040104", "050600020105")
#' sum_avg <- "sum"
#' test_county <- aggregate_HUC(s.wuds, state_county, sum_avg)
#' 
aggregate_county <- function(s.wuds, state_county, sum_avg){
  
  s.wuds <- s.wuds[s.wuds$state_county %in% state_county]
  s.wuds$Volume_mgd <- as.numeric(s.wuds$Volume_mgd)
  
  if(sum_avg == "sum"){
    s.wuds <- aggregate(as.numeric(s.wuds$Volume_mgd), by = list(state_county = s.wuds$state_county), FUN = sum, na.rm = TRUE)
    
    
  } else if (sum_avg == "avg"){
    s.wuds <- aggregate(as.numeric(s.wuds$Volume_mgd), by = list(state_county = s.wuds$state_county), FUN = mean, na.rm = TRUE)
  }
  
}
