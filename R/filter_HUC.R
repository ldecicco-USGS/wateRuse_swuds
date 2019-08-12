#' filter_HUC
#'
#' Allows user to filter their water use data from SWUDS by HUC
#'
#' @param s.wuds dataframe, the swuds water use data 
#' @param HUC chr, the HUC of interest to be aggregated
#' 
#' @export
#'
#' @examples
#' s.wuds <- swuds_sample #example data from Ohio
#' HUC <- c("041000040104", "050600020105")
#' test_HUC <- filter_HUC(s.wuds, HUC)
#' 
filter_HUC <- function(s.wuds, HUC){
  if (!("FROM_HUC_CD" %in% names(s.wuds))){
    stop("data has no column FROM_HUC_CD")
  } else {
    s.wuds <- s.wuds[which(s.wuds$FROM_HUC_CD == HUC), ]
  }
  return(s.wuds)
}
