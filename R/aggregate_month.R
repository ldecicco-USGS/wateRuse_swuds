#' aggregate_month
#'
#' Allows user to aggregate their data from SWUDS by user-defined month(s)
#' 
#' @param s_wuds dataframe, the swuds water use data
#' @param month chr, the month of interest to be aggregated in abb format (i.e. Jan, Feb, etc.)
#' @param sum_avg chr, user selects either "sum" or "avg" and the sum or average will be computed
#' 
#' @export
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' month <- c("Jan", "Feb")
#' sum_avg <- "sum"
#' test_month <- aggregate_month(s_wuds, month, sum_avg)
aggregate_month <- function(s_wuds, month, sum_avg){
  s_wuds <- s_wuds[s_wuds$Month %in% month, ]
  s_wuds$Volume_mgd <- as.numeric(s_wuds$Volume_mgd)
  if (sum_avg == "sum"){
    s_wuds <- aggregate(as.numeric(s_wuds$Volume_mgd),
                        by = list(Month = s_wuds$Month),
                        FUN = sum, na.rm = TRUE)
  } else if (sum_avg == "avg"){
    s_wuds <- aggregate(as.numeric(s_wuds$Volume_mgd),
                        by = list(Month = s_wuds$Month),
                        FUN = mean, na.rm = TRUE)
  }
  return(s_wuds)
}
