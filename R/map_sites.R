#' map_sites
#'
#' Returns map of data element mapped by sites that have lat-lons 
#' Only works with one data element as currently written
#' Assumes the s.wuds dataframe being used as input has already been subsetted 
#' using filter functions (by site type, state, or county, years, etc.)
#' that is, no subsetting is done inside this function
#' Dataframe must not have more than one row per site ID
#' Point sizes are scaled to value
#' 
#' @param s.wuds dataframe, the swuds water use data 
#' @param data.element chr, data element to be plotted 
#' @param year int, the year of interest to be mapped (defines historical basis for counties)
#' @param state character name of state
#' @param norm.element chr, element used to normalize data.element 
#' @param unit.type chr, type of unit to be mapped; acceptable options are "county", "huc", or "aquifer"
#' @param site_from_to chr, either 'from' or 'to' to designate the site lats-lons to use; the 'from' site or the 'to' site
#'  
#' @export
#' 
#' @import maps
#' @import ggplot2
#' @import maptools
#' @import mapproj
#' @import rgeos
#' @import scales
#' @importFrom ggthemes theme_map
#' @importFrom dplyr left_join
#' @importFrom scales pretty_breaks
#' 
#' @examples 
#' s.wuds <- swuds_sample #example data from OHIO
#' # subset for this example
#' s.wuds <- s.wuds[which(s.wuds$FROM_SITE_TP_CD == "AS" & 
#'                        s.wuds$YEAR == 2010 &s.wuds$Month_num == 8),]
#' data.element <- "Volume_mgd"
#' state <- "Ohio"
#' year <- 2010
#' unit.type <- "county"
#' norm.element <- NA
#' sites.map <- map_sites(s.wuds, data.element, year, state)
#' data.element <- "JUL_VAL"
#' norm.element <- "ANNUAL_VAL"
map_sites <- function(s.wuds, data.element, year, state,
                      norm.element=NA, unit.type="county",
                      site_from_to = "from"){
  if (unit.type == "county"){
    # get county polygons
    hc.sub <- subset_county_polygons(year, "STATE_TERR", state)
    hc.subf <- fortify(hc.sub, region = "FIPS")
    hc.sub@data$id <- hc.sub@data$FIPS
  } else if (unit.type == "huc"){
    #get huc polygons
    state <- stateCd$STUSAB[which(stateCd$STATE_NAME == state)]
    hc.sub <- subset_huc_polygons(year, "STATES", state)
    hc.subf <- fortify(hc.sub, region = "HUC8")
    hc.sub@data$id <- hc.sub@data$HUC8
  } else if (unit.type == "aquifer"){
    #get aquifer polygons
  }
  hc.subf <- left_join(hc.subf, hc.sub@data, by = "id")
  # coerce numeric class and normalize if a norm.element is specified
  s.wuds[, data.element] <- as.numeric(s.wuds[[data.element]])
  if (!is.na(norm.element)){
   s.wuds[, norm.element] <- as.numeric(as.character(s.wuds[[norm.element]]))
    for (i in data.element){
      s.wuds[, paste0(i, "_norm")] <- s.wuds[[i]] / s.wuds[[norm.element]]
    }
  }
  # element to be plotted
  p.elem <- ifelse(is.na(norm.element), data.element,
                   paste0(data.element, "_norm"))
  if (all(is.na(s.wuds[, p.elem]))) stop("No data available.")
  # check for multiple values per station
  # set common key name "id" to merge water use data with polygon data
  if (unit.type == "county") {
    s.wuds$id <- paste0(s.wuds$FROM_STATE_CD, s.wuds$FROM_COUNTY_CD)
  }
  if (unit.type == "huc") {
    names(s.wuds)[names(s.wuds) == "HUCCODE"] <- "id"
  }
  # merge polygons and water use data
  hc.subf <- left_join(hc.subf, s.wuds, by = "id")
  # plot element
  p.elem <- data.element
  if (all(is.na(s.wuds[p.elem]))) stop("No data available.")
  if (!is.na(norm.element)){
    p.elem <- paste0(data.element, "_norm")
  }
  if ("COUNTYNAME" %in% names(s.wuds) & !("COUNTYNAME" %in% names(hc.subf))){
    hc.subf <- left_join(hc.subf, s.wuds[, c("STATECOUNTYCODE", "COUNTYNAME")],
                         by = c("id" = "STATECOUNTYCODE"))
    hc.subf$labels <- paste("Area:", hc.subf$COUNTYNAME, "\n", p.elem, ":",
                            hc.subf[[p.elem]])
  } else if (unit.type == "county" &
             "Area.Name" %in% names(s.wuds) &
             !("Area.Name" %in% names(hc.subf))) {
    hc.subf <- left_join(hc.subf, s.wuds[, c("STATECOUNTYCODE", "Area.Name")],
                         by = c("id" = "STATECOUNTYCODE"))
    hc.subf$labels <- paste("Area:",
                            hc.subf$Area.Name,
                            "\n", p.elem, ":",
                            hc.subf[[p.elem]])
  } else if (unit.type == "huc" &
             "Area.Name" %in% names(s.wuds) &
             !("Area.Name" %in% names(hc.subf))) {
    hc.subf <- left_join(hc.subf, s.wuds[, c("HUCCODE", "Area.Name")],
                         by = c("id" = "HUCCODE"))
    hc.subf$labels <- paste("Area:",
                            hc.subf$Area.Name, "\n",
                            p.elem, ":", hc.subf[[p.elem]])
  } else {
    hc.subf$labels <- paste("Area:",
                            hc.subf$group, "\n",
                            p.elem, ":", hc.subf[[p.elem]])
  }
  hc.subf <- hc.subf[order(hc.subf$order), ]
  hc.subf <- unique(hc.subf[, c("long", "lat", "group", "labels", p.elem)])
  hc.subf <- hc.subf[!is.na(hc.subf[, p.elem]), ]
  ch.plot <- ggplot() +
    geom_polygon(data = hc.subf,
                 aes_string(x = "long", y = "lat",
                            group = "group", fill = p.elem, label = "labels"),
                            color = "black", size = 0.25) +
    coord_quickmap() +
    theme_map() +
    theme(legend.position = c(.8, .2)) +
    scale_fill_distiller(name = p.elem, palette = "YlGn",
                         breaks = pretty_breaks(n = 5),
                         trans = "reverse")
  return(ch.plot)
}
