#' filter_HUC
#'
#' Allows user to filter their water use data from SWUDS by HUC
#'
#' @examples
#' s.wuds <- swudsSample #example data from Ohio
#' HUC <- c("041000040104", "050600020105")
#' test_HUC <- filter_HUC(s.wuds, HUC)
#' 
filter_HUC <- function(s.wuds, HUC){
  s.wuds <- s.wuds[which(s.wuds$FROM_HUC_CD == HUC), ]
}