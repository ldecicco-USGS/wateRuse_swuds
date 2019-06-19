#' timeseriesswuds.R
#' 
#' Plot water use monthly (volume_mgd) and annual (ANNUAL_VAL), (eventually daily)
#' 
#' @param dec_date is decimate date in fractions of year
#' @param volume_mgd is water-use rate in million gallons per day, monthly
#' @param ANNUAL_VAL is water-use rate in million gallons per day, annually
#' 
#' # old from wateRuse
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
#' @examples 
#' #setwd("~/wateRuse_swuds/wateRuse_swuds")
#' @examples
#' 
#' 
#' swudsSample <- readRDS("C:/kyproj/TRAINING/R_Training/wateRuse_swuds/wateRuse_swuds3/wateRuse_swuds/data/swudsSample.RDS")
#' 
#' df <- swudsSample #example data intakes from Ohio, all categories
#' 
#' df2 <- df[!is.na(df$Volume_mgd),]   # remove missing values to thin data
#' 
#' df3 <- df2[ which(df2$FROM_AGENCY_CD =='USEPA' & df2$FROM_SITE_NO == '410233083375001'),]# select specific site
#' 
#' df4 <- df3[ which(df3$YEAR > 1990),]  # subset years
#'   
#'    df4$YEAR<-as.factor(df4$YEAR)     # need factor YEAR to get discrete colors
#'    df4$YEAR<-as.character.factor(df4$YEAR)   
#'   
#'       
#'   library(ggplot2)
#'   library(scales)
#'  
#'    # with factor year 
#'    p1<-ggplot(df4, aes_string(x="Month_num", y="Volume_mgd", col="YEAR")) + geom_point() + geom_line()
#'    plot(p1)
#'    
#'    # ticks at 1:12 months
#'    p2<-p1 + scale_x_continuous(breaks=c(1:12))
#'    plot(p2)
#'    
#'      
#'#  Plot annual use ANNUAL_VAL overlaying multiple sites
#'     
#'#  Subset to a few sites by selecting on a permit    
#'   
#'   df2$From_RGHT_OH015<-as.character(df2$From_RGHT_OH015)
#'   
#'   #df5<-df2[which(df2$From_RGHT_OH015=="01049"), ]   # does not work--all NA--values lost in gather?
#'   #df5<-df2[which(df2$From_DOEN_USDOE=="2857" | df2$From_DOEN_USDOE=="2858"), ]   # n=240 this does work
#'
#'  # select on HUC
#'  # df5<-df2[which(df2$FROM_HUC_CD=="050301061202"), ]
#'   
#'  # select on county and national water use code
#'   df5<-df2[which(df2$FROM_COUNTY_NM=="Delaware County"& df2$FROM_NAT_WATER_USE_CD == 'WS'), ]
#'   
#'   df6<-subset(df5,select=c(FROM_AGENCY_CD,FROM_SITE_NO,FROM_STATION_NM,FROM_SITE_TP_CD,YEAR,ANNUAL_VAL,Volume_mgd,From_DOEN_USDOE,FROM_HUC_CD,FROM_COUNTY_NM))
#' 	
#' 	# select year range
#' 	 df7 <- df6[ which(df6$YEAR > 2000 & df6$YEAR < 2011),]
#' 	
#'    p3<-ggplot(df7, aes_string(x="YEAR", y="ANNUAL_VAL", col="FROM_STATION_NM")) + scale_x_continuous(breaks= pretty_breaks()) + geom_point() + geom_line()
#'    plot(p3)
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 

#' # OLD TIMESERIES CODE FOR AWUDS:
#' areas <- c("Kent County","Sussex County")
#' area.column = "COUNTYNAME"
#' data.elements <- c("PS.GWPop","TP.TotPop")
#' w.use <- subset_wuse(df, data.elements,area.column,areas)
#' year1 <- 2005
#' year2 <- 2010
#' years <- c(year1, year2)
#' time_series_data(w.use, data.elements, area.column = area.column,areas = areas)
#' time_series_data(w.use, data.elements, plot.points = FALSE,
#'        area.column = area.column,areas = areas)
#' time_series_data(w.use, data.elements, plot.points = FALSE,
#'        area.column = area.column,areas = areas, legend=FALSE)
#' time_series_data(w.use, data.elements, area.column)
#' time_series_data(w.use, data.elements, area.column, y.scale = c(0,1000))
#' time_series_data(w.use, data.elements, area.column, 
#'        y.scale = c(0,100),years = c(1990,2005))
time_series_data <- function(w.use, data.elements, area.column, plot.points = TRUE,
                             years= NA, areas= NA, y.scale=NA, log= FALSE, legend= TRUE,
                             c.palette = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")){

  w.use.sub <- subset_wuse(w.use, data.elements, area.column, areas)
  
  if(!any(is.na(years))){
    w.use.sub <-  w.use.sub[w.use.sub$YEAR %in% years,]
    w.use.sub$YEAR <- as.factor(w.use.sub$YEAR)
    levels(w.use.sub$YEAR) <- as.character(years)
  }
  
  df <- w.use.sub[,c("YEAR",area.column,data.elements)]
  
  df <- gather_(df, "dataElement", "value", c(data.elements))
  
  fix.labs <- gsub("\\,","\\,\n",dataelement$NAME)
  names(fix.labs) <- dataelement$DATAELEMENT
  
  df$dataElement <- fix.labs[gsub(pattern = "\\.", replacement = "-", x = df$dataElement)]
  
  if(length(unique(df[[area.column]])) > length(c.palette)){
    c.palette.ramp <- colorRampPalette(c.palette)
    c.palette <- c.palette.ramp(length(unique(df[[area.column]])))
  }
  
  ts.object <- ggplot(data = df) 
  
  if(plot.points){
    ts.object <- ts.object + geom_point(aes_string(x = "YEAR", y = "value", color = area.column)) +
      scale_colour_manual(values=c.palette)
  } else {
    ts.object <- ts.object + geom_bar(aes_string(x = "YEAR", y = "value", 
                                                 fill = area.column), 
                                      position = "dodge",stat="identity")+
      scale_fill_manual(values=c.palette)
  }

  ts.object <- ts.object + facet_grid(dataElement ~ ., scales = "free") +
    ylab("") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  if(!all(is.na(y.scale))){
    ts.object <- ts.object + scale_y_continuous(limits=y.scale)
  }

  if(log){
    ts.object <- ts.object + scale_y_log10()
  }
  
  if(!legend){
    ts.object <- ts.object + theme(legend.position = "none")
  }
  
  ts.object
  
  return(ts.object)
}
  
