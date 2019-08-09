#' Read Excel file Water Quantity
#' 
#' Function to read in SWUDS Water Quantity Excel file into a dataframe
#' and converts column names to NWIS codes using a lookup table nwis_lookup.xlsx
#' @param file_path path to Excel file
#' @export
#' @importFrom readxl read_xlsx
#' @return data frame 
#' @examples 
#' pathToSample <- system.file("extdata",package = "wateRuseSWUDS")
#' df <- readWaterQuantityXL(file.path(pathToSample, "OH_CTF_SW_monthly_permit_sample_data.xlsx"))
readWaterQuantityXL <- function(file_path){
  
  dq <- readxl::read_xlsx(path = file_path)
  
  # convert column names to NWIS names 
  # Probably want to put this in sysdata:
  nwisLU <- readxl::read_xlsx("inst/extdata/nwis_lookup.xlsx")
  
  swudcols <- nwisLU$swuds
  NWIScols <- nwisLU$nwis
  
  for (sc in swudcols){
    colnames(dq)[colnames(dq) == sc] <- NWIScols[which(swudcols == sc)]
  }
  
  
  return(dq)
}

#' Function to read in SWUDS Population Served Excel file into a dataframe
#' and converts column names to NWIS codes using a lookup table nwis_lookup.xlsx
#'
#' @param file_path path to file
#' @export
#' @importFrom readxl read_xlsx
#'  
readPopServedXL <- function(file_path){
  
  dp <- readxl::read_xlsx(path = file_path)
  
  # convert column names to NWIS names 
  nwisLU <- readxl::read_xlsx("./inst/extdata/nwis_lookup.xlsx")
  
  swudcols <- nwisLU$swuds
  NWIScols <- nwisLU$nwis
  
  for (sc in swudcols){
    colnames(dp)[colnames(dp) == sc] <- NWIScols[which(swudcols == sc)]
  }
  
  return(dp)
  
  
}
#---------------------------

# Function to merge Water Quantity and Population Served data frames into one
# by "to_agency code", "to_site_no", "Year"
#---------------------------
mergeWaterQuantPopServ <- function(waterQuantDF, popServDF){
  
  df <- dplyr::left_join(waterQuantDF, popServDF, by = c("TO_AGENCY_CD", "TO_SITE_NO", "YEAR"))
  
  return(df)
  
}
#---------------------------

# Function to create month, year, month#, day, date, decimal date, and water
# year to the mergeWaterQuantPopServe data frame
#---------------------------
meltWaterQuantPopServ <- function(mergeWaterQuantPopServ){
  
  df_melt <- dplyr::rename(mergeWaterQuantPopServ, c("JAN_VAL" = "Jan", "FEB_VAL" = "Feb", "MAR_VAL" = "Mar", "APR_VAL" = "Apr", "MAY_VAL" = "May",
                                 "JUN_VAL" = "Jun", "JUL_VAL" = "Jul", "AUG_VAL" = "Aug", "SEP_VAL" = "Sep", "OCT_VAL" = "Oct", "NOV_VAL" = "Nov", 
                                 "DEC_VAL" = "Dec"))
  
  df_melt <- tidyr::gather(df_melt, Month, Volume_mgd, Jan:Dec)
  
  df_melt$Month_num <- match(df_melt$Month, month.abb)
  df_melt$Month_num <- stringr::str_pad(df_melt$Month_num, width = 2, side = "left", pad = "0")
  
  df_melt$date <- paste(df_melt$YEAR, df_melt$Month_num, "01", sep = "-")
  df_melt$Day <- days_in_month(as.Date(df_melt$date))
  df_melt$date <- NULL
  df_melt$Date <- paste(df_melt$YEAR, df_melt$Month_num, df_melt$Day, sep = "-")
  df_melt$dec_date <- decimal_date(as.Date(df_melt$Date))
  
  wtr_yr <- function(dates, start_month=9) {
    # Convert dates into POSIXlt
    dates.posix = as.POSIXlt(dates)
    # Year offset
    offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
    # Water year
    adj.year = dates.posix$year + 1900 + offset
    # Return the water year
    adj.year
  }
  
  df_melt$water_year <- wtr_yr(df_melt$Date)
  
  # repair column names 
  colnames(df_melt) <- gsub(pattern = '+', replacement = '_', x = colnames(df_melt), fixed = T)
  colnames(df_melt) <- gsub(pattern = ' ', replacement = '_', x = colnames(df_melt), fixed = T)
  
  # change character "NA" to real NA values
  df_melt$Volume_mgd[df_melt$Volume_mgd=="NA"] <- NA
  #df_melt[df_melt=="NA"] <- NA
  
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


