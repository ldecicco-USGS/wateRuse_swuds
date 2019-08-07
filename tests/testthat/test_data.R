context("Sample Data")

test_that("Data", {
  testthat::skip_on_cran()
  expect_equal(1,1)
  
  expect_equal(ncol(swudsSample), 152)
  
})