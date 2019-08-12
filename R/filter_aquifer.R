#' filter_aquifer
#'
#' Allows user to filter their water use data from SWUDS by aquifer
#' 
#' @param s.wuds dataframe, the swuds water use data 
#' @param aquifer chr, the aquifer of interest to be filtered
#' 
#' @export
#' @examples
#' s.wuds <- swudsSample #example data from Ohio
#' aquifer <- "TBD"
#' test_aquifer <- filter_aquifer(s.wuds, aquifer)
#' 
filter_aquifer <- function(s.wuds, aquifer){
  if (!("aquifer" %in% names(s.wuds))){
    message("aquifer not in dataset")
    return(NULL)
  } else {
    s.wuds <- s.wuds[which(s.wuds$aquifer == aquifer), ]
  }
  return(s.wuds)
}
