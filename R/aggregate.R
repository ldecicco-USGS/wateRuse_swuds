#' aggregate functions
#'
#' Allows user to aggregate their data from SWUDS by user-defined parameters.
#'
#' @param s_wuds swuds object (data frame from melt function)
#' @param filter_by chr, column to filter and aggregate by
#' @param filter_val values to filter to
#' @param aggregate_by column to aggregate. Defaults to "Volume_mgd"
#' @export
#' @rdname aggregate
#' @importFrom stats aggregate
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' test_all_huc <- aggregate_swuds(s_wuds, "FROM_HUC_CD")
#' test_some_huc <- aggregate_swuds(s_wuds, "FROM_HUC_CD",
#' c("041000040104", "050600020105"))
#' test_some_years <- aggregate_swuds(s_wuds, "YEAR", c("1995", "2004"))
#' #test_some_aquifers <- aggregate_swuds(s_wuds, "aquifer", "TBD")
#' test_some_months <- aggregate_swuds(s_wuds, "Month", c("Jan", "Feb"))
#' test_some_state_county <- aggregate_swuds(s_wuds, "state_county", "39033")
#' test_some_seasons <- aggregate_swuds(s_wuds, "season", "Fall")
aggregate_swuds <- function(s_wuds,
                            filter_by, filter_val = NA,
                            aggregate_by = "Volume_mgd"){
  if (!("swuds" %in% class(s_wuds))){
    s_wuds <- as_swuds(s_wuds)
  }

  if (!(filter_by %in% names(s_wuds))){
    stop("Requested filter column: ", filter_by, " not in data")
  }
  
  if (!all(is.na(filter_val))){
    s_wuds <- s_wuds[s_wuds[[filter_by]]  %in% filter_val, ]
  }
  
  s_wuds_ag <- dplyr::group_by(s_wuds, !!sym(filter_by))
  s_wuds_ag <- dplyr::summarise(s_wuds_ag,
                                avg = mean(!!sym(aggregate_by), na.rm = TRUE),
                                sum = sum(!!sym(aggregate_by), na.rm = TRUE))
  
  return(s_wuds_ag)
}
