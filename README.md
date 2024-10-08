
# nccnclimate

<!-- badges: start -->
<!-- badges: end -->

The goal of nccnclimate is to provide tools in R that allow users to easily retrieve NCCN climate data from Aquarius and export it to Excel.

## Installation

You can install the development version of nccnclimate from [its GitHub repository](https://github.com/NPS-NCCN/nccn-climate) with:

``` r
# install.packages("devtools")
devtools::install_github("NPS-NCCN/nccn-climate")
```

## How to use

It's recommended (but not required) that you create an RStudio project to store your data retrieval script and your data exports together in one place. For more information on how to set up RStudio projects and why they're useful, check out [this introduction for beginners](https://rfortherestofus.com/2022/10/rstudio-projects/).

### Setup

Each time you use the nccnclimate package, you need to load it and then use the `connectToAquarius` function to log in to Aquarius. *You must be on the DOI network.*
Be sure to put the following code at the top of any script that uses this package. 
If your code worked the last time you ran it but is now generating errors, it is most likely because you aren't on the DOI network, forgot to load the nccnclimate package, and/or forgot to connect to Aquarius.

``` r
library(nccnclimate)
## You must authenticate with Aquarius first
connectToAquarius("aquarius read only username here", "aquarius read only password here")
```

It's also good practice to put `disconnectFromAquarius()` at the end of your script.

### Export data to Excel

After you run the setup code, you can use the rest of the functions in this package.
Here is an example of how to export data to Excel spreadsheets.

``` r
park <- "OLYM"
wy <- 2022

exportNCCNDailySummaries(park_code = park, water_year = wy)
exportNCCNMonthlySummaries(park_code = park, water_year = wy)
exportNCCNDailyPeriodOfRecord(park_code = park)
exportNCCNMonthlyPeriodOfRecord(park_code = park)
```

#### Changing where exports are saved

In the example above, each data export will be saved under a default file name in R's current working directory. If you're using an RStudio project, this will just be the project folder and you don't need to do anything else. This is the simplest option.

If you aren't using an RStudio project, you can paste `getwd()` ("get working directory") into RStudio's console window and hit Enter to see which folder the data exports will be saved to by default. To use a different folder, run `setwd("C:/Users/yourusername/your/folder/here")`, replacing the path in quotes with the path to the folder where you want the exports saved.

#### Choosing custom file names

You can also choose a custom file name for your data export, like so:

``` r
exportNCCNDailySummaries(park_code = park, water_year = wy, file_out = "your_custom_filename.xlsx")
```

Make sure to include the ".xlsx" file extension. You can also use the `file_out` option to save your custom named file to a different folder instead of using `setwd`. Make sure that the folder actually exists, as the export functions will not create new folders for you.

``` r
exportNCCNDailySummaries(park_code = park, water_year = wy, file_out = "your/folder/path/your_custom_filename.xlsx")
```

#### Overwriting existing exports

By default, the export functions will never overwrite an existing file. If you want to override that default, change the `overwrite` option to `TRUE`:

``` r
exportNCCNDailySummaries(park_code = park, water_year = wy, overwrite = TRUE)
```

#### View exported data in R

All four export functions *invisibly return* the data that was written to the Excel file. This means that if you run the code examples above, the data won't be printed to the console. However, if you wish to examine the data in R, you can store it in a variable (see below). The export functions return a list of data frames in R where each data frame corresponds to a tab in the Excel file.

``` r
park <- "OLYM"
wy <- 2022

# Write the data to Excel, but also store it in a variable so it can be examined in R
olym_2022 <- exportNCCNDailySummaries(park_code = park, water_year = wy)

# See the names of the data frames in the list
names(olym_2022)

# Preview the top several rows of the average air temp data
head(olym_2022$AirTemp_Average_F)

# Get a basic summary of the avg air temp data (useful for QC!)
summary(olym_2022$AirTemp_Average_F)
```
