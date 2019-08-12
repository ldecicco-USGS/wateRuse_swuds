#' aggregate_HUC
#'
#' Allows user to aggregate their data from SWUDS by user-defined aquifer(s)
#'
#' @param s.wuds dataframe, the swuds water use data 
#' @param aquifer chr, the aquifer of interest to be aggregated
#' @param sum_avg chr, user selects either "sum" or "avg" and the sum or average will be computed
#' 
#' @export
#' @importFrom stats aggregate
#' @examples
#' s.wuds <- swudsSample #example data from Ohio
#' aquifer <- "TBD"
#' sum_avg <- "sum"
#' test_aquifer <- aggregate_aquifier(s.wuds, aquifer, sum_avg)
#' 
aggregate_aquifier <- function(s.wuds, aquifer, sum_avg){
  if (!("aquifer" %in% names(s.wuds))){
    message("no aquifer column")
    return(NULL)
  }
  s.wuds <- s.wuds[s.wuds$aquifer %in% aquifer, ]
  s.wuds$Volume_mgd <- as.numeric(s.wuds$Volume_mgd)
  if (sum_avg == "sum"){
    s.wuds <- aggregate(as.numeric(s.wuds$Volume_mgd),
                        by = list(aquifer = s.wuds$aquifer),
                        FUN = sum, na.rm = TRUE)
  } else if (sum_avg == "avg"){
    s.wuds <- aggregate(as.numeric(s.wuds$Volume_mgd),
                        by = list(aqiufer = s.wuds$aquifer),
                        FUN = mean, na.rm = TRUE)
  }
  return(s.wuds)
}
