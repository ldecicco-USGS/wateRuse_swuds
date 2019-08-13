#' map_sites
#'
#' Returns map of data element mapped by sites that have lat-lons
#' Only works with one data element as currently written
#' Assumes the s_wuds dataframe being used as input has already been subsetted
#' using filter functions (by site type, state, or county, years, etc.)
#' that is, no subsetting is done inside this function
#' Dataframe must not have more than one row per site ID
#' Point sizes are scaled to value
#' @param s_wuds dataframe, the swuds water use data
#' @param data_element chr, data element to be plotted
#' @param year int, the year of interest to be mapped (defines
#' historical basis for counties)
#' @param state character name of state
#' @param norm_element chr, element used to normalize data_element
#' @param unit_type chr, type of unit to be mapped;
#' acceptable options are "county", "huc", or "aquifer"
#' @param site_from_to chr, either 'from' or 'to' to designate the site
#' lats-lons to use; the 'from' site or the 'to' site
#' @export
#' @import maps
#' @import ggplot2
#' @import maptools
#' @import mapproj
#' @import rgeos
#' @import scales
#' @importFrom ggthemes theme_map
#' @importFrom dplyr left_join
#' @importFrom scales pretty_breaks
#' @examples
#' s_wuds <- swuds_sample #example data from OHIO
#' # subset for this example
#' s_wuds <- s_wuds[which(s_wuds$FROM_SITE_TP_CD == "AS" &
#'                        s_wuds$YEAR == 2010 &s_wuds$Month_num == 8),]
#' data_element <- "Volume_mgd"
#' state <- "Ohio"
#' year <- 2010
#' unit_type <- "county"
#' norm_element <- NA
#' sites.map <- map_sites(s_wuds, data_element, year, state)
#' data_element <- "JUL_VAL"
#' norm_element <- "ANNUAL_VAL"
map_sites <- function(s_wuds, data_element, year, state,
                      norm_element=NA, unit_type="county",
                      site_from_to = "from"){
  if (unit_type == "county"){
    # get county polygons
    hc_sub <- subset_county_polygons(year, "STATE_TERR", state)
    hc_subf <- fortify(hc_sub, region = "FIPS")
    hc_sub@data$id <- hc_sub@data$FIPS
  } else if (unit_type == "huc"){
    #get huc polygons
    state <- stateCd$STUSAB[which(stateCd$STATE_NAME == state)]
    hc_sub <- subset_huc_polygons(year, "STATES", state)
    hc_subf <- fortify(hc_sub, region = "HUC8")
    hc_sub@data$id <- hc_sub@data$HUC8
  } else if (unit_type == "aquifer"){
    #get aquifer polygons
  }
  hc_subf <- left_join(hc_subf, hc_sub@data, by = "id")
  # coerce numeric class and normalize if a norm_element is specified
  s_wuds[, data_element] <- as.numeric(s_wuds[[data_element]])
  if (!is.na(norm_element)){
   s_wuds[, norm_element] <- as.numeric(as.character(s_wuds[[norm_element]]))
    for (i in data_element){
      s_wuds[, paste0(i, "_norm")] <- s_wuds[[i]] / s_wuds[[norm_element]]
    }
  }
  # element to be plotted
  p_elem <- ifelse(is.na(norm_element), data_element,
                   paste0(data_element, "_norm"))
  if (all(is.na(s_wuds[, p_elem]))) stop("No data available.")
  # check for multiple values per station
  # set common key name "id" to merge water use data with polygon data
  if (unit_type == "county") {
    s_wuds$id <- paste0(s_wuds$FROM_STATE_CD, s_wuds$FROM_COUNTY_CD)
  }
  if (unit_type == "huc") {
    names(s_wuds)[names(s_wuds) == "HUCCODE"] <- "id"
  }
  # merge polygons and water use data
  hc_subf <- left_join(hc_subf, s_wuds, by = "id")
  # plot element
  p_elem <- data_element
  if (all(is.na(s_wuds[p_elem]))) stop("No data available.")
  if (!is.na(norm_element)){
    p_elem <- paste0(data_element, "_norm")
  }
  if ("COUNTYNAME" %in% names(s_wuds) & !("COUNTYNAME" %in% names(hc_subf))){
    hc_subf <- left_join(hc_subf, s_wuds[, c("STATECOUNTYCODE", "COUNTYNAME")],
                         by = c("id" = "STATECOUNTYCODE"))
    hc_subf$labels <- paste("Area:", hc_subf$COUNTYNAME, "\n", p_elem, ":",
                            hc_subf[[p_elem]])
  } else if (unit_type == "county" &
             "Area.Name" %in% names(s_wuds) &
             !("Area.Name" %in% names(hc_subf))) {
    hc_subf <- left_join(hc_subf, s_wuds[, c("STATECOUNTYCODE", "Area.Name")],
                         by = c("id" = "STATECOUNTYCODE"))
    hc_subf$labels <- paste("Area:",
                            hc_subf$Area.Name,
                            "\n", p_elem, ":",
                            hc_subf[[p_elem]])
  } else if (unit_type == "huc" &
             "Area.Name" %in% names(s_wuds) &
             !("Area.Name" %in% names(hc_subf))) {
    hc_subf <- left_join(hc_subf, s_wuds[, c("HUCCODE", "Area.Name")],
                         by = c("id" = "HUCCODE"))
    hc_subf$labels <- paste("Area:",
                            hc_subf$Area.Name, "\n",
                            p_elem, ":", hc_subf[[p_elem]])
  } else {
    hc_subf$labels <- paste("Area:",
                            hc_subf$group, "\n",
                            p_elem, ":", hc_subf[[p_elem]])
  }
  hc_subf <- hc_subf[order(hc_subf$order), ]
  hc_subf <- unique(hc_subf[, c("long", "lat", "group", "labels", p_elem)])
  hc_subf <- hc_subf[!is.na(hc_subf[, p_elem]), ]
  ch_plot <- ggplot() +
    geom_polygon(data = hc_subf,
                 aes_string(x = "long", y = "lat",
                            group = "group", fill = p_elem, label = "labels"),
                            color = "black", size = 0.25) +
    coord_quickmap() +
    theme_map() +
    theme(legend.position = c(.8, .2)) +
    scale_fill_distiller(name = p_elem, palette = "YlGn",
                         breaks = pretty_breaks(n = 5),
                         trans = "reverse")
  return(ch_plot)
}
