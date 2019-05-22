#' aggregate_month
#'
#' Allows user to aggregate their data from SWUDS by user-defined month(s)
#' 
#' @param s.wuds dataframe, the swuds water use data 
#' @param month chr, the month of interest to be aggregated in abb format (i.e. Jan, Feb, etc.)
#' @param sum_avg chr, user selects either "sum" or "avg" and the sum or average will be computed
#' 
#' @examples
#' s.wuds <- swudsSample #example data from Ohio
#' month <- c("Jan", "Feb")
#' sum_avg <- "sum"
#' test_month <- aggregate_month(s.wuds, month, sum_avg)
#' 
aggregate_month<- function(s.wuds, month, sum_avg){
  
  s.wuds <- s.wuds[s.wuds$Month %in% month, ]
  s.wuds$Volume_mgd <- as.numeric(s.wuds$Volume_mgd)
  
  if(sum_avg == "sum"){
    s.wuds <- aggregate(as.numeric(s.wuds$Volume_mgd), by = list(Month = s.wuds$Month), FUN = sum, na.rm = TRUE)
    
    
  } else if (sum_avg == "avg"){
    s.wuds <- aggregate(as.numeric(s.wuds$Volume_mgd), by = list(Month = s.wuds$Month), FUN = mean, na.rm = TRUE)
  }
 
}
