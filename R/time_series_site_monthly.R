#' Monthly Time Series
#' 
#' Plot water use monthly (volume_mgd) overlaying multiple years 
#' 
#' @param s_wuds dataframe, the swuds water use data 
#' @param FROM_SITE_NUM chr, site number agency and site_no unigue to site
#' 
#' @export
#' @import ggplot2 scales
#' 
#' @examples 
#' s_wuds <- swuds_sample #example data from Ohio
#' FROM_SITE_NUM <- c("410233083375001")
#' testp2 <- time_series_site_monthly(s_wuds, FROM_SITE_NUM)
time_series_site_monthly <- function(s_wuds, FROM_SITE_NUM) { 
  
  df1 <- s_wuds[which(s_wuds$FROM_SITE_NO == FROM_SITE_NUM), ]
  
  df1 <- df1[!is.na(df1$Volume_mgd), ]   # remove missing values 
  df1$YEAR <- as.factor(df1$YEAR)     # need factor YEAR to get discrete colors
  df1$YEAR <- as.character.factor(df1$YEAR)  
  
  label1 <- paste(df1$FROM_AGENCY_CD[1],
                  df1$FROM_SITE_NO[1],
                  df1$FROM_STATION_NM[1],
                  df1$FROM_SITE_TP_CD[1],
                  sep = "", collapse = NULL)
  
  p1 <- ggplot(df1, aes_string(x = "Month_num",
                               y = "Volume_mgd",
                               col = "YEAR")) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = c(1:12)) +
    labs(title = label1)
  
  plot(p1)
  return(p1)
}
