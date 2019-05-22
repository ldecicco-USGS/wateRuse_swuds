#' aggregate_season
#'
#' @param s.wuds dataframe, the swuds water use data 
#' @param season chr, the aquifer of interest to be aggregated
#' @param sum_avg chr, user selects either "sum" or "avg" and the sum or average will be computed
#' 
#' Allows user to filter their water use data from SWUDS by season(s)
#'
#' @examples
#' s.wuds <- swudsSample #example data from Ohio
#' waterType <- "SW"
#' test_aggSeason <- aggregate_season(s.wuds, season, sum_avg)
#' 
aggregate_season<- function(s.wuds, season, sum_avg){
  
  if(season == "Winter" && sum_avg == "sum"){
    #get Winter sum
    s.wuds <- s.wuds[s.wuds$Month %in% c("Jan", "Feb", "Mar"), ]
    s.wuds$winterSum <- sum(as.numeric(s.wuds$Volume_mgd), na.rm = TRUE)
    
  } else if (season == "Winter" && sum_avg == "avg"){
    #get Winter avg
    s.wuds <- s.wuds[s.wuds$Month %in% c("Jan", "Feb", "Mar"), ]
    s.wuds$winterAvg <- mean(as.numeric(s.wuds$Volume_mgd), na.rm = TRUE)
      
  } else if (season == "Spring" && sum_avg == "sum"){
    #get Spring sum
    s.wuds <- s.wuds[s.wuds$Month %in% c("Mar", "Apr", "May"), ]
    s.wuds$springSum <- sum(as.numeric(s.wuds$Volume_mgd), na.rm = TRUE)
    
  } else if (season == "Spring" && sum_avg == "avg"){
    #get Spring avg
    s.wuds <- s.wuds[s.wuds$Month %in% c("Mar", "Apr", "May"), ]
    s.wuds$springAvg <- mean(as.numeric(s.wuds$Volume_mgd), na.rm = TRUE)
    
  } else if (season == "Summer" && sum_avg == "sum"){
    #get Summer sum
    s.wuds <- s.wuds[s.wuds$Month %in% c("Jun", "Jul", "Aug"), ]
    s.wuds$summerSum <- sum(as.numeric(s.wuds$Volume_mgd), na.rm = TRUE)
    
  } else if (season == "Summer" && sum_avg == "avg"){
    #get Summer avg
    s.wuds <- s.wuds[s.wuds$Month %in% c("Jun", "Jul", "Aug"), ]
    s.wuds$summerAvg <- mean(as.numeric(s.wuds$Volume_mgd), na.rm = TRUE)
    
  } else if (season == "Fall" && sum_avg == "sum"){
    #get Fall sum
    s.wuds <- s.wuds[s.wuds$Month %in% c("Sep", "Oct", "Nov"), ]
    s.wuds$fallSum <- sum(as.numeric(s.wuds$Volume_mgd), na.rm = TRUE)
    
  } else if (season == "Fall" && sum_avg == "avg"){
    #get Fall avg
    s.wuds <- s.wuds[s.wuds$Month %in% c("Sep", "Oct", "Nov"), ]
    s.wuds$fallAvg <- mean(as.numeric(s.wuds$Volume_mgd), na.rm = TRUE)
  }
}