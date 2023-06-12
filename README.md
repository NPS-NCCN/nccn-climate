
# nccnclimate

<!-- badges: start -->
<!-- badges: end -->

The goal of nccnclimate is to provide tools in R that allow users to easily retrieve NCCN climate data from Aquarius and export it to Excel.

## Installation

You can install the development version of nccnclimate from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("nationalparkservice/nccn-climate")
```

## How to use

### Setup

Each time you use R, you need to load the nccnclimate package and log in to Aquarius.

``` r
library(nccnclimate)
## You must authenticate with Aquarius first
connectToAquarius("aquarius read only username here", "aquarius read only password here")
```

### Export data to Excel

After you run the setup code, you can use the rest of the functions in this package.
Here is an example of how to export data to an Excel spreadsheet.

``` r
exportNCCNDailySummaries(park_code = "OLYM", water_year = 2022, file_out = "test_file.xlsx")
```

