#' filter.R
#'
#' Filter on multiple attributes 
#' 
#' @param attributes vector of character names of attributes for filtering
#' @param conditions vector of character values of corresponding attributes for filtering
#'
#' 
#' @export
#' @import 
#' 
#' @examples
#' library(dplyr) 
#' s.wuds <- swuds_sample #example data from Ohio
#' 
#' attributes <-c("FROM_NAT_WATER_USE_CD")
#' conditions <- c("WS")
#' 
#' attributes <-c("FROM_NAT_WATER_USE_CD","FROM_COUNTY_NM")
#' conditions <- c("WS","Lawrence County")
#'
#' attributes <-c("FROM_NAT_WATER_USE_CD","FROM_COUNTY_NM", "YEAR")
#' conditions <- c("WS","Lawrence County", "1990")
#' 
#' # throws error: Error in UseMethod("filter_") : 
#' # no applicable method for 'filter_' applied to an object of class "character"
#' # but works within if clauses in function--why an error when called in function?
#'  
#' filtered(attributes,conditions)
#' 
#' # Forms:
#' #s.wuds %>% filter(FROM_NAT_WATER_USE_CD == "WS")
#' #s.wuds %>% filter(FROM_SITE_NO == "410233083375001")
#' #s.wuds %>% filter(From_RGHT_OH015=="01049")  # broken boolean
#' 
#' #s.wuds %>% filter(FROM_NAT_WATER_USE_CD == "WS")# %>% filter(FROM_COUNTY_NM == "Lawrence County")  %>% filter(YEAR=="1990")
#' 
#'  # Basic direct function 
#'  #s.wuds %>% filter(
#'  #.data[[attributes[[1]]]] == conditions[[1]],
#'  #.data[[attributes[[2]]]] == conditions[[2]],
#'  #.data[[attributes[[3]]]] == conditions[[3]]
#'  #)
#'  
#'  
#'
#'  
#'  
#'  
#'  
filtered <- function(s.wuds, attributes, conditions){ 
  if(length(attributes) == 1){s.wuds %>% filter(
    .data[[attributes[[1]]]] == conditions[[1]]
  )
  }
  
  
  if (length(attributes)==2){s.wuds %>% filter(
    .data[[attributes[[1]]]] == conditions[[1]],
    .data[[attributes[[2]]]] == conditions[[2]]
  )
  }
  
  if (length(attributes)==3){s.wuds %>% filter(
    .data[[attributes[[1]]]] == conditions[[1]],
    .data[[attributes[[2]]]] == conditions[[2]],
    .data[[attributes[[3]]]] == conditions[[3]]
  )
  } 
  if (length(attributes)==4){s.wuds %>% filter(
    .data[[attributes[[1]]]] == conditions[[1]],
    .data[[attributes[[2]]]] == conditions[[2]],
    .data[[attributes[[3]]]] == conditions[[3]],
    .data[[attributes[[4]]]] == conditions[[4]]
  )
    
  }  
  if (length(attributes)==5){s.wuds %>% filter(
    .data[[attributes[[1]]]] == conditions[[1]],
    .data[[attributes[[2]]]] == conditions[[2]],
    .data[[attributes[[3]]]] == conditions[[3]],
    .data[[attributes[[4]]]] == conditions[[4]],
    .data[[attributes[[5]]]] == conditions[[5]]
  )
    
  }}
