#' filter_category
#'
#' Allows user to filter their water use data from SWUDS by category
#'
#' @param s_wuds dataframe, the swuds water use data
#' @param category chr, the category of interest to be filtered
#' 
#' @export
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' category <- c("WS", "IR", "IN")
#' test_category <- filter_category(s_wuds, category)
filter_category <- function(s_wuds, category){

  if (!("FROM_NAT_WATER_USE_CD" %in% names(s_wuds))){
    stop("FROM_NAT_WATER_USE_CD not in dataset")
  } else {
    s_wuds <- s_wuds[which(s_wuds$FROM_NAT_WATER_USE_CD == category), ]
  }
  return(s_wuds)
}
