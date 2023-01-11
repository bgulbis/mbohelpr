#' Get data from csv files
#'
#' Read data from one or more csv files into a single data frame (tibble)
#'
#' @param path a character vector of full path names
#' @param pattern a regular expression.  Only file names which match the regular
#'   expression will be returned.
#' @param col_types one of NULL (default), a cols() specification, or a string
#' @param tz time zone
#'
#' @return A tibble
#' @export
get_data <- function(path, pattern, col_types = NULL, tz = "US/Central") {
    f <- list.files(path, pattern, full.names = TRUE)

    n <- f |>
        purrr::map_int(~ nrow(data.table::fread(.x, select = 1L)))

    f[n > 0] |>
        purrr::map_df(
            readr::read_csv,
            locale = readr::locale(tz = tz),
            col_types = col_types
        ) |>
        dplyr::rename_all(stringr::str_to_lower) |>
        dplyr::distinct()
}

#' Get data from xlsx files
#'
#' Read data from one or more Excel files into a single data frame (tibble)
#'
#' @param path a character vector of full path names
#' @param pattern a regular expression.  Only file names which match the regular
#'   expression will be returned.
#' @param sheet sheet to read, either a string (the name of a sheet), or an integer (the position of the sheet).
#' @param col_names \code{TRUE} to use the first row as column names,
#'   \code{FALSE} to get default names, or a character vector giving a name for
#'   each column
#' @param skip Minimum number of rows to skip before reading anything, be it
#'   column names or data
#' @param col_types Either \code{NULL} to guess all from the spreadsheet or a
#'   character vector containing one entry per column from these options:
#'   "skip", "guess", "logical", "numeric", "date", "text" or "list".
#'
#' @return A tibble
#' @export
get_xlsx_data <- function(path, pattern, sheet = NULL, col_names = TRUE, col_types = NULL, skip = 0) {
    f <- list.files(path, pattern, full.names = TRUE)

    # n <- f |>
    #     purrr::map_int(~ nrow(data.table::fread(.x, select = 1L)))

    # f[n > 0] |>
    f |>
        purrr::map_df(
            readxl::read_excel,
            sheet = sheet,
            col_names = col_names,
            col_types = col_types,
            skip = skip
        ) |>
        dplyr::rename_all(stringr::str_to_lower) |>
        dplyr::distinct()
}