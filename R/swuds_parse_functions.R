#' Read Excel file Water Quantity
#' 
#' Function to read in SWUDS Water Quantity Excel file into a dataframe
#' and converts column names to NWIS codes using a lookup table nwis_lookup.xlsx
#' @param file_path path to Excel file
#' 
#' @export
#' 
#' @importFrom readxl read_xlsx
#' @return data frame 
#' @examples 
#' pathToSample <- system.file("extdata",package = "wateRuseSWUDS")
#' df <- readWaterQuantityXL(file.path(pathToSample, "OH_CTF_SW_monthly_permit_sample_data.xlsx"))
readWaterQuantityXL <- function(file_path){
  dq <- readxl::read_xlsx(path = file_path,
                          guess_max = 2000,
                          na = "NA")
  names(dq)[names(dq) %in% nwisLU$swuds] <- nwisLU$nwis[match(names(dq)[names(dq) %in%
                                                        nwisLU$swuds],
                                                        nwisLU$swuds)]
  return(dq)
}

#' Function to read in SWUDS Population Served Excel file into a dataframe
#' and converts column names to NWIS codes using a lookup table nwis_lookup.xlsx
#'
#' @param file_path path to file
#' 
#' @export
#' @importFrom readxl read_xlsx
#' @examples 
#' pathToSample <- system.file("extdata",package = "wateRuseSWUDS")
#' dp <- readPopServedXL(file.path(pathToSample,"OHpopserved_output.xlsx"))
readPopServedXL <- function(file_path){
  dp <- readxl::read_xlsx(path = file_path,
                          guess_max = 2000)
  names(dp)[names(dp) %in% nwisLU$swuds] <- nwisLU$nwis[match(names(dp)[names(dp) %in%
                                              nwisLU$swuds], nwisLU$swuds)]
  return(dp)
}

#' Merge Water Quantity and Population
#' 
#' Function to merge Water Quantity and Population Served data frames into one
#' by "to_agency code", "to_site_no", "Year"
#'
#' @param waterQuantDF data frame
#' @param popServDF data frame
#' @importFrom dplyr left_join
#' 
#' @export
#' 
#' @examples 
#' pathToSample <- system.file("extdata",package = "wateRuseSWUDS")
#' dp <- readPopServedXL(file.path(pathToSample,"OHpopserved_output.xlsx"))
#' dq <- readWaterQuantityXL(file.path(pathToSample,
#'               "OH_CTF_SW_monthly_permit_sample_data.xlsx"))
#' df <- mergeWaterQuantPopServ(waterQuantDF = dq, popServDF = dp)
mergeWaterQuantPopServ <- function(waterQuantDF, popServDF){
  df <- left_join(waterQuantDF, popServDF,
                  by = c("TO_AGENCY_CD", "TO_SITE_NO", "YEAR"))
  return(df)
}


#' meltWaterQuantPopServ
#' 
#' Function to create month, year, month#, day, date, decimal date, and water
#' year to the mergeWaterQuantPopServe data frame
#'
#' @param mergeWaterQuantPopServ data frame
#' @importFrom lubridate decimal_date
#' @importFrom lubridate days_in_month
#' @importFrom tidyr gather
#'
#' @export
#'   
#' @examples 
#' pathToSample <- system.file("extdata",package = "wateRuseSWUDS")
#' dp <- readPopServedXL(file.path(pathToSample,"OHpopserved_output.xlsx"))
#' dq <- readWaterQuantityXL(file.path(pathToSample,
#'               "OH_CTF_SW_monthly_permit_sample_data.xlsx"))
#' df <- mergeWaterQuantPopServ(waterQuantDF = dq, popServDF = dp)
#' df_melt <- meltWaterQuantPopServ(mergeWaterQuantPopServ = df)
meltWaterQuantPopServ <- function(mergeWaterQuantPopServ){
  names(mergeWaterQuantPopServ)[names(mergeWaterQuantPopServ) %in%
                                  paste0(toupper(month.abb), "_VAL")] <- month.abb
  # df_melt <- dplyr::rename(mergeWaterQuantPopServ,
  #                          Jan = JAN_VAL,
  #                          Feb = FEB_VAL,
  #                          Mar = MAR_VAL,
  #                          Apr = APR_VAL,
  #                          May = MAY_VAL,
  #                          Jun = JUN_VAL,
  #                          Jul = JUL_VAL,
  #                          Aug = AUG_VAL,
  #                          Sep = SEP_VAL,
  #                          Oct = OCT_VAL,
  #                          Nov = NOV_VAL,
  #                          Dec = DEC_VAL)
  df_melt <- tidyr::gather(mergeWaterQuantPopServ, Month, Volume_mgd, Jan:Dec)
  df_melt$Month_num <- match(df_melt$Month, month.abb)
  df_melt$Month_num <- ifelse(df_melt$Month_num < 10,
                              paste0("0", df_melt$Month_num),
                              as.character(df_melt$Month_num))
  df_melt$date <- paste(df_melt$YEAR, df_melt$Month_num, "01", sep = "-")
  df_melt$Day <- days_in_month(as.Date(df_melt$date))
  df_melt$date <- NULL
  df_melt$Date <- paste(df_melt$YEAR, df_melt$Month_num, df_melt$Day, sep = "-")
  df_melt$dec_date <- decimal_date(as.Date(df_melt$Date))
  df_melt$water_year <- wtr_yr(df_melt$Date)
  # repair column names
  colnames(df_melt) <- gsub(pattern = "+", replacement = "_",
                            x = colnames(df_melt), fixed = TRUE)
  colnames(df_melt) <- gsub(pattern = " ", replacement = "_",
                            x = colnames(df_melt), fixed = TRUE)
  # change character "NA" to real NA values
  df_melt$Volume_mgd[df_melt$Volume_mgd == "NA"] <- NA
  # enforce numeric
  df_melt$ANNUAL_VAL <- as.numeric(df_melt$ANNUAL_VAL)
  df_melt$Month_num <- as.numeric(df_melt$Month_num)
  df_melt$Volume_mgd <- as.numeric(df_melt$Volume_mgd)
  df_melt$Day <- as.numeric(df_melt$Day)
  df_melt$dec_date <- as.numeric(df_melt$dec_date)
  df_melt$water_year <- as.numeric(df_melt$water_year)
  df_melt$YEAR <- as.numeric(df_melt$YEAR)
  df_melt$TO_DEC_LAT_VA <- as.numeric(df_melt$TO_DEC_LAT_VA)
  df_melt$FROM_DEC_LAT_VA <- as.numeric(df_melt$FROM_DEC_LAT_VA)
  df_melt$TO_DEC_LONG_VA <- as.numeric(df_melt$TO_DEC_LONG_VA)
  df_melt$FROM_DEC_LONG_VA <- as.numeric(df_melt$FROM_DEC_LONG_VA)
  df_melt$Annual_Population <- as.numeric(df_melt$Annual_Population)
  return(df_melt)
}

# bringing it out in case you want to use it again...but not exporting
wtr_yr <- function(dates, start_month=9) {
  # Convert dates into POSIXlt
  dates.posix <- as.POSIXlt(dates)
  # Year offset
  offset <- ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year <- dates.posix$year + 1900 + offset
  # Return the water year
  return(adj.year)
}
