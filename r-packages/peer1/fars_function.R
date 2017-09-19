#' Read FARS csv data
#'
#' This function reads FARS (Fatality Analysis Reporting System) data from a csv input and returns a
#' data frame tbl (thin wrapper around data.frame) object
#'
#' @param filename A csv path
#' @return A data frame tbl object.
#'
#' @details If the input file does not exists, an error will be thrown with the message
#'     "file <requested_file> does not exists"
#'
#' @importFrom readr::read_csv dplyr::tbl_df
#'
#' @examples
#'   fars_read("data/my_fars_data.csv")
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Make the filename for the fars data for a given year
#'
#' @param year A year to construct the csv filename
#' @return The csv filename for the requested year
#'
#' @details If the paramer year cannot be cast to an integer, the name accident_NA.csv.bz2 will be
#'     returned
#'
#' @examples
#'   make_filename(2007)
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Read the available year, month from the dataset
#'
#' This function reads the available month, year combinations from the dataset
#'
#' @param years The years to be read
#' @return A list of data frames corresponding to the requested years. Each data frame contains
#'     column: MONTH and year, indicating the avaiable year-month from the data
#'
#' @details If a year results in error (file not available, corrupted data, etc) a warning will be
#'     shown and a NULL object is returned
#' @importFrom dplyr::mutate dplyr::select
#' @examples
#'   fars_read_years(c(2013,2014))
#'
#' @export
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

#' Summarize the data for given years
#'
#' This function takes some years as input and return a dataframe showing the number of records per
#' each year
#'
#' @param years The years to be read
#' @return A data frame of one row, and as many columns as the requested years, the cell represents
#'     the number of records for the given year
#'
#' @importFrom dplyr::bind_rows dplyr::group_by dplyr::summarize tidyr::spread
#' @examples
#'   fars_summarize_years(c(2013,2014)
#'   The result should look like
#'
#'         2013    2014
#'  n     97987  121313
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Map the FARS data for a state in a given year
#'
#' This functions map the accident data for a state-year combination
#'
#' @param state.num An integer representing the state ID
#' @param year A year
#' @return A map object or NULL if there is no accidents to report
#'
#' @details
#'   If the state.num is invalid an error will be thrown specifying this
#'   If there is no data associated with the state, a message "no accidents to plot" will be shown,
#'     a invisible NULL is returned
#'   If there is some data, points representing where the accidents occur is shown on a map
#'
#' @importFrom maps::map graphics::points
#' @export
#'
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
