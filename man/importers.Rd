% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swuds_parse_functions.R
\name{read_swuds_quant}
\alias{read_swuds_quant}
\alias{read_swuds_pop}
\title{Read Excel file Water Quantity}
\usage{
read_swuds_quant(file_path)

read_swuds_pop(file_path)
}
\arguments{
\item{file_path}{path to Excel file}
}
\value{
data frame
}
\description{
Function to read in SWUDS Water Quantity Excel and
SWUDS Population Served Excel
file into a dataframe and converts column names
to NWIS codes using a lookup table.
}
\examples{
sample_path <- system.file("extdata",package = "WUReview")
df <- read_swuds_quant(file.path(sample_path,
         "OH_CTF_SW_monthly_permit_sample_data.xlsx"))
dp <- read_swuds_pop(file.path(sample_path,
         "OHpopserved_output.xlsx"))
}
