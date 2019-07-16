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

    n <- f %>%
        purrr::map_int(~ nrow(data.table::fread(.x, select = 1L)))

    f[n > 0] %>%
        purrr::map_df(
            readr::read_csv,
            locale = readr::locale(tz = tz),
            col_types = col_types
        ) %>%
        dplyr::rename_all(stringr::str_to_lower) %>%
        dplyr::distinct()
}