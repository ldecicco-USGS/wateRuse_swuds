#' timeseriesswuds.R
#' 
#' Plot water use monthly (volume_mgd) and annual (ANNUAL_VAL), (eventually daily)
#' 
#' @param areas is a geographical area as defined in your datafile such as county, HUC, or aquifer
#' @param data.elements character name of data element within available categories by year for state
#' @param area.column character that defines which column to use to specify area
#' @param years vector of integers specifying all years available for state. Defaults to NA which shows all years in dataset.
#' @param s.wuds dataframe, the swuds water use data
#' @param y.scale allows R to set the y-axis scale given available data range. Defaults to NA which lets R set the scale based on dataset values.
#' @param log = TRUE or FALSE allows user to set log scale, default is FALSE
#' @param plot.points is a logical function to show counties as points or clustered bar graph
#' @param legend is a logical function to include list of counties in a legend if manageable, default is TRUE
#' @param c.palette color palette to use for points or bars
#' 
#' @export
#' @import ggplot2
#' @importFrom tidyr gather_
#' @importFrom grDevices colorRampPalette
#' 
time_series_data <- function(s.wuds, data.elements, area.column,
                             plot.points = TRUE,
                             years= NA, areas= NA,
                             y.scale=NA, log= FALSE, legend= TRUE,
                             c.palette = c("#999999", "#E69F00", "#56B4E9",
                                           "#009E73", "#F0E442", "#0072B2",
                                           "#D55E00", "#CC79A7")){
  data.elements <- data.elements[which(!is.na(data.elements))]
  if (all(is.na(areas))){
    w.use.sub <- s.wuds[, c("YEAR", area.column, data.elements)]
  } else {
    w.use.sub <-  s.wuds[s.wuds[[area.column]] %in% areas,
                         c("YEAR", area.column, data.elements)]
  }
  if (!any(is.na(years))){
    w.use.sub <-  w.use.sub[w.use.sub$YEAR %in% years, ]
    w.use.sub$YEAR <- as.factor(w.use.sub$YEAR)
    levels(w.use.sub$YEAR) <- as.character(years)
  }
  df <- w.use.sub[, c("YEAR", area.column, data.elements)]
  df <- gather_(df, "dataElement", "value", c(data.elements))
  fix.labs <- gsub("\\,", "\\,\n", data.elements$NAME)
  names(fix.labs) <- data.elements$DATAELEMENT
  df$dataElement <- fix.labs[gsub(pattern = "\\.",
                                  replacement = "-",
                                  x = df$dataElement)]
  if (length(unique(df[[area.column]])) > length(c.palette)){
    c.palette.ramp <- colorRampPalette(c.palette)
    c.palette <- c.palette.ramp(length(unique(df[[area.column]])))
  }
  ts.object <- ggplot(data = df)
  if (plot.points){
    ts.object <- ts.object +
      geom_point(aes_string(x = "YEAR", y = "value",
                            color = area.column)) +
      scale_colour_manual(values = c.palette)
  } else {
    ts.object <- ts.object +
      geom_bar(aes_string(x = "YEAR", y = "value",
                          fill = area.column),
               position = "dodge", stat = "identity") +
      scale_fill_manual(values = c.palette)
  }
  ts.object <- ts.object +
    facet_grid(dataElement ~ ., scales = "free") +
    ylab("") +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1))
  if (!all(is.na(y.scale))){
    ts.object <- ts.object +
      scale_y_continuous(limits = y.scale)
  }
  if (log){
    ts.object <- ts.object +
      scale_y_log10()
  }
  if (!legend){
    ts.object <- ts.object +
      theme(legend.position = "none")
  }
  return(ts.object)
}
