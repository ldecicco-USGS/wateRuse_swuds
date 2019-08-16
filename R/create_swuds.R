#' Load and check swuds data
#'
#' @param path_pop file path to Excel file of population file
#' @param path_quant file path to Excel file of water quantity file
#' @param \dots additional paremeters sent to \code{as_swuds}.
#' 
#' @export
#' @rdname swuds
#' @examples 
#' path_to_data <-  system.file("extdata", package="wateRuseSWUDS")
#' file_name_1 <- "OH_CTF_SW_monthly_permit_sample_data.xlsx"
#' file_name_2 <- "OHpopserved_output.xlsx"
#' excel_path_quant <- file.path(path_to_data, file_name_1)
#' excel_path_pop <- file.path(path_to_data, file_name_2)
#' swuds_1 <- create_swuds(excel_path_pop, excel_path_quant)
#' summary(swuds_1)
create_swuds <- function(path_pop, path_quant, ...){
   
  if (!file.exists(path_pop)){
    stop("Population file does not exist, check path and spelling")
  }
  
  if (!file.exists(path_quant)){
    stop("Quantity file does not exist, check path and spelling")
  }
  
  dq <- read_swuds_quant(path_quant)
  dp <- read_swuds_pop(path_pop)
  
  raw_swuds <- as_swuds(dq, dp, ...)
  
  return(raw_swuds)
  
}

#' @export
#' @rdname swuds
#' @param object swuds object
summary.swuds <- function(object, ...){
  
  print(utils::head(object[, 1:3]))

}

#' @export
#' @rdname swuds
#' @param dp data frame population
#' @param dq data frame quanity
as_swuds <- function(dq, dp=NULL){
  
  if (is.null(dp)){
    df_melt <- dq
  } else {
    df <- merge_dq_dp(dq = dq, dp = dp)
    df_melt <- melt_water_quant_pop(df)
  }
  
  required_columns <- c("Volume_mgd", "FROM_STATE_CD",
                        "FROM_COUNTY_CD", "Month", "YEAR")
  
  # Check on some important columns:
  if (!all(required_columns %in% names(df_melt))){
    stop("Missing columns: ",
         paste0(required_columns[!(required_columns %in% names(df_melt))],
               collapse = ", "))
  }
  
  # Check or create important column types:
  df_melt$Volume_mgd <- as.numeric(df_melt$Volume_mgd)
  
  df_melt$season <- "Winter"
  df_melt$season[df_melt$Month %in% c("Mar", "Apr", "May")] <- "Spring"
  df_melt$season[df_melt$Month %in% c("Jun", "Jul", "Aug")] <- "Summer"
  df_melt$season[df_melt$Month %in% c("Sep", "Oct", "Nov")] <- "Fall"
  
  df_melt$state_county <- paste0(df_melt$FROM_STATE_CD, df_melt$FROM_COUNTY_CD)
  
  class(df_melt) <- c("swuds", class(df_melt))
  
  return(df_melt)
}
