#' aggregate_HUC
#'
#' Allows user to aggregate their data from SWUDS by user-defined aquifer(s)
#'
#' @param s_wuds dataframe, the swuds water use data
#' @param aquifer chr, the aquifer of interest to be aggregated
#' @param sum_avg chr, user selects either "sum" or "avg" and the sum
#' or average will be computed
#' @export
#' @importFrom stats aggregate
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' aquifer <- "TBD"
#' sum_avg <- "sum"
#' test_aquifer <- aggregate_aquifier(s_wuds, aquifer, sum_avg)
aggregate_aquifier <- function(s_wuds, aquifer, sum_avg){
  if (!("aquifer" %in% names(s_wuds))){
    message("no aquifer column")
    return(NULL)
  }
  s_wuds <- s_wuds[s_wuds$aquifer %in% aquifer, ]
  s_wuds$Volume_mgd <- as.numeric(s_wuds$Volume_mgd)
  if (sum_avg == "sum"){
    s_wuds <- aggregate(as.numeric(s_wuds$Volume_mgd),
                        by = list(aquifer = s_wuds$aquifer),
                        FUN = sum, na.rm = TRUE)
  } else if (sum_avg == "avg"){
    s_wuds <- aggregate(as.numeric(s_wuds$Volume_mgd),
                        by = list(aqiufer = s_wuds$aquifer),
                        FUN = mean, na.rm = TRUE)
  }
  return(s_wuds)
}
