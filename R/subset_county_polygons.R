#' subset_county_polygons
#'
#' Returns county polygons from: Siczewicz, Peter. U.S. Historical Counties
#' (Generalized .001 deg) on the basis of year of interest
#' Dataset is subset on basis of area of interest
#' @param areas chr, vector of codes indicating HUCs, counties, states,
#' regions, aquifers, etc.
#' @param area_column chr, defines which column to use to specify area
#' @param year int, the year of interest to be mapped (defines historical
#' basis for counties)
#' @export
#' @return hc_sub dataframe, a subset of county polygon data
#' @examples
#' areas <- "Delaware" # 3 counties present day
#' area_column <- "STATE_TERR"
#' year <- 2010
#' hc_sub <- subset_county_polygons(year, area_column, areas)
#' hc_sub$NAME
#' areas <- "Maine" # 16 counties present day
#' area_column <- "STATE_TERR"
#' year <- 2010
#' hc_sub <- subset_county_polygons(year, area_column, areas)
#' hc_sub$NAME
#' year <- 1850 # Maine had 13 counties in 1850; Delaware 3
#' hc_sub <- subset_county_polygons(year, area_column, areas)
#' hc_sub$NAME
subset_county_polygons <- function(year, area_column, areas){
  # year of interest converted to date
  test_date <- as.Date(paste0(year, "-12-31"))
  now_date <- format(Sys.Date(), "%Y-%m-%d")
  # select area of interest
  hc_sub <- histCounties[which(histCounties@data[, area_column] == areas), ]
  # substitute todays date for the last date
  # of the histCounties dataset (2000-12-31)
  hc_sub$END_DATE[hc_sub$END_DATE == max(hc_sub$END_DATE)] <- now_date
  # assuming the counties defined in 2000-12-31 are the same now, use todays
  # date to make sure the selection for contemporaneous counties works
  # for year of interest, get the counties
  hc_sub <- hc_sub[hc_sub$END_DATE >= test_date &
                     hc_sub$START_DATE <= test_date, ]
  return(hc_sub)
}
