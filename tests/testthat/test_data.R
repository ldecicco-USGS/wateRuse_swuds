context("Sample Data")

test_that("Data", {
  testthat::skip_on_cran()
  expect_equal(1, 1)
  expect_equal(ncol(swuds_sample), 154)
  
  # Test loading:
  path_to_sample <- system.file("extdata", package = "wateRuseSWUDS")
  
  # Read in the water quantity table
  dq <- read_swuds_quant(file.path(path_to_sample,
                                   "OH_CTF_SW_monthly_permit_sample_data.xlsx"))
  expect_equal(ncol(dq), 109)
  
  # Read in the population served table
  dp <- read_swuds_pop(file.path(path_to_sample,
                                 "OHpopserved_output.xlsx"))
  expect_equal(ncol(dp), 51)
  
  # merge the tables
  df <- merge_dq_dp(dq, dp)
  expect_equal(ncol(df), 157)
  
  #melt the table
  df_melt <- melt_water_quant_pop(df)
  expect_equal(nrow(df_melt), 11988)  
})
