#' aggregate_year
#'
#' Allows user to aggregate their data from SWUDS by user-defined year(s)
#'
#' @param s.wuds dataframe, the swuds water use data 
#' @param year numeric, the year of interest to be aggregated
#' @param sum_avg chr, user selects either "sum" or "avg" and the sum or average will be computed
#' 
#' @export
#'  
#' @examples
#' s.wuds <- swudsSample #example data from Ohio
#' year <- c("1995", "2004")
#' sum_avg <- "sum"
#' test_year <- aggregate_year(s.wuds, year, sum_avg)
#' 
aggregate_year <- function(s.wuds, year, sum_avg){
  
  s.wuds <- s.wuds[s.wuds$YEAR %in% year, ]
  s.wuds$Volume_mgd <- as.numeric(s.wuds$Volume_mgd)
  
  if(sum_avg == "sum"){
    s.wuds <- aggregate(as.numeric(s.wuds$Volume_mgd), by = list(Year = s.wuds$YEAR), FUN = sum, na.rm = TRUE)
    
    
  } else if (sum_avg == "avg"){
    s.wuds <- aggregate(as.numeric(s.wuds$Volume_mgd), by = list(Year = s.wuds$YEAR), FUN = mean, na.rm = TRUE)
  }
  
}
