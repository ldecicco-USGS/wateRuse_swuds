#' filter_HUC
#'
#' Allows user to filter their water use data from SWUDS by HUC
#'
#' @param s_wuds dataframe, the swuds water use data
#' @param HUC chr, the HUC of interest to be aggregated
#' 
#' @export
#'
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' HUC <- c("041000040104", "050600020105")
#' test_HUC <- filter_HUC(s_wuds, HUC)
filter_HUC <- function(s_wuds, HUC){
  if (!("FROM_HUC_CD" %in% names(s_wuds))){
    stop("data has no column FROM_HUC_CD")
  } else {
    s_wuds <- s_wuds[which(s_wuds$FROM_HUC_CD == HUC), ]
  }
  return(s_wuds)
}
