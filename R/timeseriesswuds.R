#' timeseriesswuds
#' 
#' Plot water use monthly (volume_mgd) and annual
#' (ANNUAL_VAL), (eventually daily)
#' @param areas is a geographical area as defined in your datafile such as
#' county, HUC, or aquifer
#' @param data_elements character name of data element within available
#' categories by year for state
#' @param area_column character that defines which column to use to specify area
#' @param years vector of integers specifying all years available for state.
#' Defaults to NA which shows all years in dataset.
#' @param s_wuds dataframe, the swuds water use data
#' @param y_scale allows R to set the y-axis scale given available
#' data range. Defaults to NA which lets R set the scale
#' based on dataset values.
#' @param log = TRUE or FALSE allows user to set log scale, default is FALSE
#' @param plot.points is a logical function to show counties as points or
#' clustered bar graph
#' @param legend is a logical function to include list of counties in a
#' legend if manageable, default is TRUE
#' @param c_palette color palette to use for points or bars
#' @export
#' @import ggplot2
#' @importFrom tidyr gather_
#' @importFrom grDevices colorRampPalette
time_series_data <- function(s_wuds, data_elements, area_column,
                             plot.points = TRUE,
                             years= NA, areas= NA,
                             y_scale=NA, log= FALSE, legend= TRUE,
                             c_palette = c("#999999", "#E69F00", "#56B4E9",
                                           "#009E73", "#F0E442", "#0072B2",
                                           "#D55E00", "#CC79A7")){
  data_elements <- data_elements[which(!is.na(data_elements))]
  if (all(is.na(areas))){
    w_use_sub <- s_wuds[, c("YEAR", area_column, data_elements)]
  } else {
    w_use_sub <-  s_wuds[s_wuds[[area_column]] %in% areas,
                         c("YEAR", area_column, data_elements)]
  }
  if (!any(is.na(years))){
    w_use_sub <-  w_use_sub[w_use_sub$YEAR %in% years, ]
    w_use_sub$YEAR <- as.factor(w_use_sub$YEAR)
    levels(w_use_sub$YEAR) <- as.character(years)
  }
  df <- w_use_sub[, c("YEAR", area_column, data_elements)]
  df <- gather_(df, "data_element", "value", c(data_elements))
  fix_labs <- gsub("\\,", "\\,\n", data_elements$NAME)
  names(fix_labs) <- data_elements$data_element
  df$data_element <- fix_labs[gsub(pattern = "\\.",
                                  replacement = "-",
                                  x = df$data_element)]
  if (length(unique(df[[area_column]])) > length(c_palette)){
    c_palette_ramp <- colorRampPalette(c_palette)
    c_palette <- c_palette_ramp(length(unique(df[[area_column]])))
  }
  ts_object <- ggplot(data = df)
  if (plot.points){
    ts_object <- ts_object +
      geom_point(aes_string(x = "YEAR", y = "value",
                            color = area_column)) +
      scale_colour_manual(values = c_palette)
  } else {
    ts_object <- ts_object +
      geom_bar(aes_string(x = "YEAR", y = "value",
                          fill = area_column),
               position = "dodge", stat = "identity") +
      scale_fill_manual(values = c_palette)
  }
  ts_object <- ts_object +
    facet_grid(data_element ~ ., scales = "free") +
    ylab("") +
    theme(axis.text.x = element_text(angle = 90,
                                     vjust = 0.5,
                                     hjust = 1))
  if (!all(is.na(y_scale))){
    ts_object <- ts_object +
      scale_y_continuous(limits = y_scale)
  }
  if (log){
    ts_object <- ts_object +
      scale_y_log10()
  }
  if (!legend){
    ts_object <- ts_object +
      theme(legend.position = "none")
  }
  return(ts_object)
}
