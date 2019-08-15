#' aggregate functions
#'
#' Allows user to aggregate their data from SWUDS by user-defined parameters.
#'
#' @param s_wuds swuds object (data frame from melt function)
#' @param aquifer chr, the aquifer of interest to be aggregated
#' @param sum_avg chr, user selects either "sum" or "avg" and the sum
#' or average will be computed
#' @export
#' @rdname aggregate
#' @importFrom stats aggregate
#' @examples
#' s_wuds <- swuds_sample #example data from Ohio
#' aquifer <- "TBD"
#' sum_avg <- "sum"
#' test_aquifer <- aggregate_aquifier(s_wuds, aquifer, sum_avg)
aggregate_aquifier <- function(s_wuds, aquifer, sum_avg){
  
  if(!("swuds" %in% class(s_wuds))){
    s_wuds <- as_swuds(s_wuds)
  }
  
  if (!("aquifer" %in% names(s_wuds))){
    message("no aquifer column")
    return(NULL)
  }

  match.arg(sum_avg, c("sum","avg"))
  
  s_wuds <- s_wuds[s_wuds$aquifer %in% aquifer, ]
  
  if (sum_avg == "sum"){
    s_wuds <- aggregate(s_wuds$Volume_mgd,
                        by = list(aquifer = s_wuds$aquifer),
                        FUN = sum, na.rm = TRUE)
  } else if (sum_avg == "avg"){
    s_wuds <- aggregate(s_wuds$Volume_mgd,
                        by = list(aqiufer = s_wuds$aquifer),
                        FUN = mean, na.rm = TRUE)
  }
  return(s_wuds)
}


#' @export
#' @rdname aggregate
#' @param state_county chr, the county of interest to be aggregated.
#' State abbreviation is also
#' appended to the beginning of the county
#' @examples
#' state_county <- "39033"
#' test_county <- aggregate_county(s_wuds, state_county, sum_avg)
aggregate_county <- function(s_wuds, state_county, sum_avg){
  s_wuds$state_county <- paste0(s_wuds$FROM_STATE_CD, s_wuds$FROM_COUNTY_CD)
  s_wuds <- s_wuds[s_wuds$state_county %in% state_county, ]
  
  if (sum_avg == "sum"){
    s_wuds <- aggregate(s_wuds$Volume_mgd,
                        by = list(state_county = s_wuds$state_county),
                        FUN = sum, na.rm = TRUE)
  } else if (sum_avg == "avg"){
    s_wuds <- aggregate(s_wuds$Volume_mgd,
                        by = list(state_county = s_wuds$state_county),
                        FUN = mean, na.rm = TRUE)
  }
  return(s_wuds)
}

#' @export
#' @param HUC chr, the HUC of interest to be aggregated
#' @rdname aggregate
#' @examples
#' HUC <- c("041000040104", "050600020105")
#' test_HUC <- aggregate_HUC(s_wuds, HUC, sum_avg)
aggregate_HUC <- function(s_wuds, HUC, sum_avg){
  if (!("FROM_HUC_CD" %in% names(s_wuds))){
    message("no aquifer column")
    return(NULL)
  }
  s_wuds <- s_wuds[s_wuds$FROM_HUC_CD %in% HUC, ]
  
  if (sum_avg == "sum"){
    s_wuds <- aggregate(s_wuds$Volume_mgd,
                        by = list(HUC = s_wuds$FROM_HUC_CD),
                        FUN = sum, na.rm = TRUE)
  } else if (sum_avg == "avg"){
    s_wuds <- aggregate(s_wuds$Volume_mgd,
                        by = list(HUC = s_wuds$FROM_HUC_CD),
                        FUN = mean, na.rm = TRUE)
  }
  return(s_wuds)
}

#' @export
#' @rdname aggregate
#' @param month chr, the month of interest to be aggregated in abb
#' format (i.e. Jan, Feb, etc.)
#' @examples
#' month <- c("Jan", "Feb")
#' test_month <- aggregate_month(s_wuds, month, sum_avg)
aggregate_month <- function(s_wuds, month, sum_avg){
  s_wuds <- s_wuds[s_wuds$Month %in% month, ]
  
  if (sum_avg == "sum"){
    s_wuds <- aggregate(s_wuds$Volume_mgd,
                        by = list(Month = s_wuds$Month),
                        FUN = sum, na.rm = TRUE)
  } else if (sum_avg == "avg"){
    s_wuds <- aggregate(s_wuds$Volume_mgd,
                        by = list(Month = s_wuds$Month),
                        FUN = mean, na.rm = TRUE)
  }
  return(s_wuds)
}

#' @export
#' @rdname aggregate
#' @param season chr, the aquifer of interest to be aggregated
#' @examples
#' season <- "Fall"
#' test_aggSeason <- aggregate_season(s_wuds, season, sum_avg)
aggregate_season <- function(s_wuds, season, sum_avg){
  if (season == "Winter" && sum_avg == "sum"){
    #get Winter sum
    s_wuds <- s_wuds[s_wuds$Month %in% c("Jan", "Feb", "Mar"), ]
    s_wuds$winter_sum <- sum(s_wuds$Volume_mgd, na.rm = TRUE)
  } else if (season == "Winter" && sum_avg == "avg"){
    #get Winter avg
    s_wuds <- s_wuds[s_wuds$Month %in% c("Jan", "Feb", "Mar"), ]
    s_wuds$winter_avg <- mean(s_wuds$Volume_mgd, na.rm = TRUE)
  } else if (season == "Spring" && sum_avg == "sum"){
    #get Spring sum
    s_wuds <- s_wuds[s_wuds$Month %in% c("Mar", "Apr", "May"), ]
    s_wuds$spring_sum <- sum(s_wuds$Volume_mgd, na.rm = TRUE)
  } else if (season == "Spring" && sum_avg == "avg"){
    #get Spring avg
    s_wuds <- s_wuds[s_wuds$Month %in% c("Mar", "Apr", "May"), ]
    s_wuds$spring_avg <- mean(s_wuds$Volume_mgd, na.rm = TRUE)
  } else if (season == "Summer" && sum_avg == "sum"){
    #get Summer sum
    s_wuds <- s_wuds[s_wuds$Month %in% c("Jun", "Jul", "Aug"), ]
    s_wuds$summer_sum <- sum(s_wuds$Volume_mgd, na.rm = TRUE)
  } else if (season == "Summer" && sum_avg == "avg"){
    #get Summer avg
    s_wuds <- s_wuds[s_wuds$Month %in% c("Jun", "Jul", "Aug"), ]
    s_wuds$summer_avg <- mean(s_wuds$Volume_mgd, na.rm = TRUE)
  } else if (season == "Fall" && sum_avg == "sum"){
    #get Fall sum
    s_wuds <- s_wuds[s_wuds$Month %in% c("Sep", "Oct", "Nov"), ]
    s_wuds$fall_sum <- sum(s_wuds$Volume_mgd, na.rm = TRUE)
  } else if (season == "Fall" && sum_avg == "avg"){
    #get Fall avg
    s_wuds <- s_wuds[s_wuds$Month %in% c("Sep", "Oct", "Nov"), ]
    s_wuds$fall_avg <- mean(s_wuds$Volume_mgd, na.rm = TRUE)
  }
  return(s_wuds)
}

#' @export
#' @rdname aggregate
#' @param year numeric, the year of interest to be aggregated
#' @examples
#' year <- c("1995", "2004")
#' test_year <- aggregate_year(s_wuds, year, sum_avg)
aggregate_year <- function(s_wuds, year, sum_avg){
  s_wuds <- s_wuds[s_wuds$YEAR %in% year, ]
  
  if (sum_avg == "sum"){
    s_wuds <- aggregate(s_wuds$Volume_mgd,
                        by = list(Year = s_wuds$YEAR),
                        FUN = sum, na.rm = TRUE)
  } else if (sum_avg == "avg"){
    s_wuds <- aggregate(s_wuds$Volume_mgd,
                        by = list(Year = s_wuds$YEAR),
                        FUN = mean, na.rm = TRUE)
  }
  return(s_wuds)
}
