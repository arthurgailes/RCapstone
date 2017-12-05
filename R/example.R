#' Read in Fatality Analysis Reporting System data file
#'
#' \code{fars_read} reads in the US National Highway Traffic Safety
#'   Administration's Fatality Analysis Reporting System data.
#'
#' @param filename csv file contains the data
#'
#' @return \code{fars_read} searches within the specified path for the
#'   filename provided. If the file exists, it will be imported and returned as
#'   a data frame tbl.  If it does not exist an error message is returned.
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!exists(filename))
    stop("file '", filename, "' does not exist")
  data <- get(filename)
  dplyr::tbl_df(data)
}


#' Make File Name
#'
#' \code{make_file} creates a name for the accident csv file based on the
#'   year provided.
#'
#' @param year the year to add in the file name
#'
#' @return \code{make_file} returns a file name based on the year provided.
#'   For example, if 2017 is provided as the year the name that is returned
#'   will be "accident_2017.csv.bz2".
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read in Fatality Analysis Reporting System data files
#'
#' \code{fars_read_years}  reads in multiple Fatality Analysis Reporting
#'   System data files based on the years provided.
#'
#' @param years The years relating to the file names to be read in
#'
#' @return \code{fars_read_years} searches for the file names based on the
#'   years provided. For example, if 2014:2015 is provided \code{fars_read_years}
#'   searches for the following files:
#'   \itemize{
#'     \item "accident_2014.csv.bz2"
#'     \item "accident_2015.csv.bz2"
#'   }
#'   If the files exist a list containing the respective data is returned.
#'   If the files do not exist an error is returned stating the invalid year(s).
#'
#' @seealso \code{\link{make_filename}} for naming convention
#'
#' @examples
#' fars_read_years(2013:2014)
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#'
#' @export
fars_read_years <- function(years) {
  MONTH=STATE=NULL
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


#' Summarize Observations by Year
#'
#' \code{fars_summarize_years} reads in multiple Fatality Analysis Reporting
#'   System data files based on the years provided and summarise the number of
#'   observations by month and year.
#'
#' @param years The years relating to the file names to be read in
#'
#' @return \code{fars_summarize_years} returns a wide-formatted data frame.
#'
#' @seealso \code{\link{fars_read_years}} to understand how the file name is created
#'
#' @examples
#' fars_summarize_years(2013:2015)
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom dplyr %>%
#'
#' @export
fars_summarize_years <- function(years) {
  MONTH=STATE=n=year=NULL
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Map State Accidents
#'
#' \code{fars_map_state} plots the accidents on a map for a given state
#'   and year.
#'
#' @param state.num State number
#' @param years The year of concern
#'
#' @return \code{fars_map_state} returns a map plot of accidents for the given
#'   state and year. If no accidents occurred in that state for that year a
#'   notification is provided and if an invalid state number is provided
#'   an error is returned.
#'
#' @seealso
#' \code{\link{make_filename}} to understand how the file name is created
#' \code{\link{fars_read}} to understand how the file is read in
#'
#' @examples
#' fars_map_state(1, 2013)
#'
#' @importFrom dplyr filter
#' @import maps
#' @importFrom graphics points
#'
#' @export
fars_map_state <- function(state.num, year) {
  STATE=NULL
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
