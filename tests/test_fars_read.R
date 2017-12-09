

# This function tests, by comparing to a saved image of correctly loaded data
library(testthat)
devtools::document()
setwd(system.file("inst/extdata", package = "farsfunctions"))
filename <- make_filename(2014)
my_data <- fars_read(filename)

compare <- readRDS("../../tests/fars_read_data_04122017.RDS")
testthat::expect_equal(compare, my_data)
