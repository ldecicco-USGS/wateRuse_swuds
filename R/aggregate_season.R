#' aggregate_season
#'
#' Allows user to filter their water use data from SWUDS by season(s)
#' 
#' @param s_wuds dataframe, the swuds water use data 
#' @param season chr, the aquifer of interest to be aggregated
#' @param sum_avg chr, user selects either "sum" or "avg" and the sum or average will be computed
#' 
#' @export
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' sum_avg <- "sum"
#' season <- "Fall"
#' test_aggSeason <- aggregate_season(s_wuds, season, sum_avg)
#' 
aggregate_season <- function(s_wuds, season, sum_avg){
  if (season == "Winter" && sum_avg == "sum"){
    #get Winter sum
    s_wuds <- s_wuds[s_wuds$Month %in% c("Jan", "Feb", "Mar"), ]
    s_wuds$winter_sum <- sum(as.numeric(s_wuds$Volume_mgd), na.rm = TRUE)
  } else if (season == "Winter" && sum_avg == "avg"){
    #get Winter avg
    s_wuds <- s_wuds[s_wuds$Month %in% c("Jan", "Feb", "Mar"), ]
    s_wuds$winter_avg <- mean(as.numeric(s_wuds$Volume_mgd), na.rm = TRUE)
  } else if (season == "Spring" && sum_avg == "sum"){
    #get Spring sum
    s_wuds <- s_wuds[s_wuds$Month %in% c("Mar", "Apr", "May"), ]
    s_wuds$spring_sum <- sum(as.numeric(s_wuds$Volume_mgd), na.rm = TRUE)
  } else if (season == "Spring" && sum_avg == "avg"){
    #get Spring avg
    s_wuds <- s_wuds[s_wuds$Month %in% c("Mar", "Apr", "May"), ]
    s_wuds$spring_avg <- mean(as.numeric(s_wuds$Volume_mgd), na.rm = TRUE)
  } else if (season == "Summer" && sum_avg == "sum"){
    #get Summer sum
    s_wuds <- s_wuds[s_wuds$Month %in% c("Jun", "Jul", "Aug"), ]
    s_wuds$summer_sum <- sum(as.numeric(s_wuds$Volume_mgd), na.rm = TRUE)
  } else if (season == "Summer" && sum_avg == "avg"){
    #get Summer avg
    s_wuds <- s_wuds[s_wuds$Month %in% c("Jun", "Jul", "Aug"), ]
    s_wuds$summer_avg <- mean(as.numeric(s_wuds$Volume_mgd), na.rm = TRUE)
  } else if (season == "Fall" && sum_avg == "sum"){
    #get Fall sum
    s_wuds <- s_wuds[s_wuds$Month %in% c("Sep", "Oct", "Nov"), ]
    s_wuds$fall_sum <- sum(as.numeric(s_wuds$Volume_mgd), na.rm = TRUE)
  } else if (season == "Fall" && sum_avg == "avg"){
    #get Fall avg
    s_wuds <- s_wuds[s_wuds$Month %in% c("Sep", "Oct", "Nov"), ]
    s_wuds$fall_avg <- mean(as.numeric(s_wuds$Volume_mgd), na.rm = TRUE)
  }
  return(s_wuds)
}
