#' aggregate_HUC
#'
#' Allows user to aggregate their data from SWUDS by user-defined HUC(S)
#'
#' @param s_wuds dataframe, the swuds water use data
#' @param HUC chr, the HUC of interest to be aggregated
#' @param sum_avg chr, user selects either "sum" or "avg" and
#' the sum or average will be computed
#' @export
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' HUC <- c("041000040104", "050600020105")
#' sum_avg <- "sum"
#' test_HUC <- aggregate_HUC(s_wuds, HUC, sum_avg)
aggregate_HUC <- function(s_wuds, HUC, sum_avg){
  if (!("FROM_HUC_CD" %in% names(s_wuds))){
    message("no aquifer column")
    return(NULL)
  }
  s_wuds <- s_wuds[s_wuds$FROM_HUC_CD %in% HUC, ]
  s_wuds$Volume_mgd <- as.numeric(s_wuds$Volume_mgd)
  if (sum_avg == "sum"){
    s_wuds <- aggregate(as.numeric(s_wuds$Volume_mgd),
                        by = list(HUC = s_wuds$FROM_HUC_CD),
                        FUN = sum, na.rm = TRUE)
  } else if (sum_avg == "avg"){
    s_wuds <- aggregate(as.numeric(s_wuds$Volume_mgd),
                        by = list(HUC = s_wuds$FROM_HUC_CD),
                        FUN = mean, na.rm = TRUE)
  }
  return(s_wuds)
}
