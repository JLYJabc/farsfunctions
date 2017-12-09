

# This function tests, by comparing to a saved image of correctly loaded data

# source
library(testthat)

# change dir to load fresh batch of data
setwd(system.file("inst/extdata", package = "farsfunctions"))
filename <- make_filename(2014)
my_data <- fars_read(filename)

# change dir to load saved image of data, when the function were
setwd(system.file("tests/", package = "farsfunctions"))
compare <- readRDS("fars_read_data_04122017.RDS")

# we expect these objects to be identical
testthat::expect_equal(compare, my_data)
