#' time_series_site_monthly.R
#' 
#' Plot water use monthly (volume_mgd) overlaying multiple years 
#' 
#' other f: plot annual water use (ANNUAL_VAL) overlaying multiple sites (time_series_sites_annual.R)
#' 
#' @param s.wuds dataframe, the swuds water use data 
#' @param FROM_AGENCY_CD chr, agency code
#' @param FROM_SITE_NUM chr, site number agency and site_no unigue to site
#' @param FROM_STATION_NM chr, site name, up to 50 length
#' @param FROM_SITE_TP_CD chr, site type: FA-DV, GW, etc
#' @param 
#' @param 
#' @param 
#' @param 
#' 
#' @export
#' @import ggplot2 scales
#' 
#' @examples 
#' s.wuds <- swuds_sample #example data from Ohio
#' FROM_SITE_NUM<-c("410233083375001")
#' testp2 <- time_series_site_monthly(s.wuds,FROM_SITE_NUM)
#' 
#' library(ggplot2)
#' library(scales)
#' 
#' 
#'
#' 
time_series_site_monthly <- function(s.wuds,FROM_SITE_NUM ) { 
  
  df1 <- s.wuds[which(s.wuds$FROM_SITE_NO ==FROM_SITE_NUM),]
  
  df1 <- df1[!is.na(df1$Volume_mgd),]   # remove missing values 
  df1$YEAR<-as.factor(df1$YEAR)     # need factor YEAR to get discrete colors
  df1$YEAR<-as.character.factor(df1$YEAR)  
  
  
  label1 <- paste(df1$FROM_AGENCY_CD[1],df1$FROM_SITE_NO[1],df1$FROM_STATION_NM[1],df1$FROM_SITE_TP_CD[1],sep="",collapse = NULL)
  
  p1<-ggplot(df1, aes_string(x="Month_num", y="Volume_mgd", col="YEAR")) + geom_point() + geom_line()
  p2<-p1 + scale_x_continuous(breaks=c(1:12)) + labs(title=label1)
  
  plot(p2)
}
