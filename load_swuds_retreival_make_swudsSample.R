library(tidyverse)
library(dplyr)
library(lubridate)

# Read in the water quantity table
dq <- readWaterQuantityXL("./data/OH_CTF_SW_monthly_permit_owner_allsectors_preferredandnot.xlsx")

# Read in the population served table
dp <- readPopServedXL("./data/OHpopserved_output.xlsx")

# merge the tables
df <- mergeWaterQuantPopServ(waterQuantDF = dq, popServDF = dp)

#melt the table
df_melt <- meltWaterQuantPopServ(mergeWaterQuantPopServ = df)

# save out swudsSample.RDS in the data folder
# sampleData used for testing and developing functions
saveRDS(df_melt, file = "./data/swudsSample.RDS")

rm(df, df_melt, dp, dq)

swudsSample <- readRDS("./data/swudsSample.RDS")
