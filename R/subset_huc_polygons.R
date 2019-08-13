#' subset_huc_polygons
#'
#' Returns HUC polygons
#' Dataset is subset on basis of area of interest
#' @param areas chr, vector of codes indicating area of interest to subset on
#' @param area_column chr, defines which column the areas
#' refer to for subsetting
#' @param year int, the year of interest to be mapped
#' (defines historical basis for HUCs)
#' @export
#' @return hc_sub dataframe, a subset of HUC polygon data
#' @examples
#' areas <- "ME" # 21 HUC8s in Maine
#' area_column <- "STATES"
#' year <- 1995
#' hc_sub <- subset_huc_polygons(year, area_column, areas)
#' hc_sub$HUCNAME
subset_huc_polygons <- function(year, area_column, areas){
  hc_sub <- huc08Poly[which(grepl(areas, huc08Poly@data[, area_column])), ]
  return(hc_sub)
}
