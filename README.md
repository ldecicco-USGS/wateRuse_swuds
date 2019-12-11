# WUReview <img src="man/figures/logo.png" alt="WUReview" height="150px" align="right" />

[![travis](https://travis-ci.org/USGS-R/wateRuse_swuds.svg?branch=master)](https://travis-ci.org/USGS-R/wateRuse_swuds) [![Codecov test coverage](https://codecov.io/gh/USGS-R/wateRuse_swuds/branch/master/graph/badge.svg)](https://codecov.io/gh/USGS-R/wateRuse_swuds?branch=master)  [![status](https://img.shields.io/badge/USGS-Support-yellow.svg)](https://owi.usgs.gov/R/packages.html#support)  

## Setup data

```r
pathToSample <- system.file("extdata",package = "WUReview")

# Read in the water quantity table
dq <- read_swuds_quant(file.path(pathToSample,"OH_CTF_SW_monthly_permit_sample_data.xlsx"))

# Read in the population served table
dp <- read_swuds_pop(file.path(pathToSample,"OHpopserved_output.xlsx"))

# merge the tables
df <- merge_dq_dp(dq, dp)

#melt the table
df_melt <- melt_water_quant_pop(df)
```

## Contributing

### Pull requests

- Please run `lintr::lint_package()` before submitting a pull request.  
- consider running `goodpractice::gp()` on the package before contributing.

## Installation

To install the package while the package is still internal, users will need to get a "Personal Access Token" from code.usgs.gov. 

1. Go to https://code.chs.usgs.gov/profile/personal_access_tokens
2. Create a Personal Access Token and click the "api" scope:
![pat](man/figures/pat.png)
3. After clicking the green "Create personal access token", you will see a screen like this:
![pat](man/figures/save_pat.png)
4. Save your token in a safe place (KeyPass for instance) so you don't need to constantly regenerate tokens. 
5. In R, you can will need the `remotes` package to install:
```r
remotes::install_gitlab("water-use/wureview", 
                        host = "code.chs.usgs.gov", 
                        auth_token = "abc123")
```

## Disclaimer

This software is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The software has not received final approval by the U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of release constitute any such warranty. The software is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the software.
