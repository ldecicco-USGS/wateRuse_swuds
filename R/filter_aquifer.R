#' filter_aquifer
#'
#' Allows user to filter their water use data from SWUDS by aquifer
#' 
#' @param s_wuds dataframe, the swuds water use data 
#' @param aquifer chr, the aquifer of interest to be filtered
#' 
#' @export
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' aquifer <- "TBD"
#' test_aquifer <- filter_aquifer(s_wuds, aquifer)
#' 
filter_aquifer <- function(s_wuds, aquifer){
  if (!("aquifer" %in% names(s_wuds))){
    message("aquifer not in dataset")
    return(NULL)
  } else {
    s_wuds <- s_wuds[which(s_wuds$aquifer == aquifer), ]
  }
  return(s_wuds)
}
