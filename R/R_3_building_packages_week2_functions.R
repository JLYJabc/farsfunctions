#'
#' Reads data
#'
#' @description 
#' Loads a datafile from disk. If no path is provided the current directory is 
#' used. An error is given if the file does not exist.
#'
#' @param filename, name of file containing data in csv format 
#'
#' @return a dataframe of class tbl_df
#' 
#' @importFrom dplyr, tbl_df
#' 
#' @importFrom readr, read_csv
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' fars_read("C:/Users/My_filepath/accident_2013.csv.bz2")
#' 
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Construct filename
#'
#' @description 
#' Constructs a filename of the type accident_'year'_csv.z2, where year is 
#' given as input.
#'
#' @param year, should be an integer or a string coercible to an integer
#'
#' @return a string
#' 
#' @examples 
#' make_filename(1996)
#' 
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("data/accident_%d.csv.bz2", year)
}

#' Read in data for multiple years
#'
#' @description 
#' Given years as input, this function generates proper filenames using 
#' make_filename(), then reads the files using fars_read(). The data is subsequently 
#' manipulated, by creating an extra column, year, and selecting the colunms MONTH and year. 
#' A warning message is provided if data does not exist for a given year. This 
#' function will never fail, as whole function is run with tryCatch.
#'
#' @param years, a list of years (should be a list of integers) 
#'
#' @importFrom dplyr, select, mutate, tbl_df
#' @importFrom readr, read_csv
#' @importFrom magrittr, %>%
#'
#' @return 
#' A list of lists of data for each valid year in the input list 'years'. The data
#'
#' @examples
#' fars_read_years(c(2013,2014))
#' my_yrs <- c(2013,2014,2015))
#' fars__read_years(my_yrs)
#' 
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>% 
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarize data
#' 
#' @description 
#' Given a list of years, loads data associated with the years, selects and 
#' summarizes relevant data.
#'
#' @param years, a list of years (should be a list of integers)   
#'
#' @return A dataframe, with summary data of accidents for each month
#'
#' @importFrom readr read_csv
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows summarize select mutate tbl_df
#' @importFrom magrittr %>%
#'
#' @examples
#' fars_summarize_years(c(2014,2015))
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Show accidents on a state map
#'
#' @description 
#' Given a year and a state number, this functions creates an image of the state
#' with the accidents marked. An error is given if either the state number or the
#' year is not valid. A message is provided if there were no accidents in a 
#' paritcular state and year.
#'
#' @param state.num, an integer, must be less than 57.
#' @param year, an integer or string coercible to an integer
#'
#' @importFrom maps, map
#' @importFrom graphics, points
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df filter
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @return A map of the state, where accidents are marked 
#'
#' @examples
#' fars_map_state(44,2014)
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)
  
  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
