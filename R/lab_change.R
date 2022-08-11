# lab_change.R

#' Determine if a lab changed by a set amount within a specific time frame
#'
#' \code{lab_change} checks for changes in lab values
#'
#' This function takes a data frame with lab data for a single lab and checks
#' whether the lab changes by a certain amount within a given period of time.
#'
#' For FUN, use max when looking for a decrease in lab value, and min when
#' looking for an increase in lab value.
#'
#' @param df A data frame with lab data
#' @param change_by A numeric indicating the threshold for lab changes
#' @param FUN A function for \code{rollapplyr}, most commonly max or
#'   min
#' @param back An optional numeric specifying the number of days back to go.
#'   Defaults to 2 days.
#' @param .id Patient identifier column, defaults to encntr_id
#' @param .dt_tm Date/time column, defaults to lab_datetime
#' @param .result Lab result, defaults to result_val
#'
#' @return A data frame
#'
#' @examples
#' # checks for a >= 2 decrease in the hemoglobin value within the past 2 days
#' x <- tidy_data(labs)
#'
#' print(head(
#'   lab_change(x, "hgb", -2, max, back = 2)
#' ))
#'
#' @export
lab_change <- function(df, change_by, FUN, back = 2, .id = encntr_id,
                       .dt_tm = lab_datetime, .result = result_val) {

    # calculate the number of rows that are included within the window, then
    # calculate the running min/max during the time window, then calculate the
    # change from the running min/max to current value, then filter values which
    # exceed the change.by value
    # id <- set_id_quo(x)

    # lab <- rlang::sym(lab_col)
    rowsback <- rlang::sym("rowsback")
    running <- rlang::sym("running")

    df |>
        dplyr::arrange({{ .id }}, {{ .dt_tm }}) |>
        dplyr::group_by({{ .id }}) |>
        dplyr::mutate(!!"rowsback" := count_rowsback({{ .dt_tm }}, back)) |>
        dplyr::filter(!is.na(!!rowsback)) |>
        dplyr::mutate(
            !!"running" := zoo::rollapplyr({{ .result }}, !!rowsback, FUN, fill = NA, partial = TRUE),
            !!"change" := {{ .result }} - !!running
        ) |>
        dplyr::filter(!!rlang::parse_expr("abs(change) >= abs(change_by)")) |>
        dplyr::ungroup()
}

#' Count the number of rows to go back in data frame
#'
#' Takes a vector of POSIXct and counts the number of rows which would fall
#' within the specified time frame. Typically called from
#' \code{mutate} and the results are passed on to
#' \code{rollapplyr}.
#'
#' @param x = vector of type POSIXct
#' @param back = integer indicating the number of days back to include
#'
#' @return integer vector
#'
#' @keywords internal
count_rowsback <- function(x, back = 2) {
    purrr::map_int(x, function(y) sum(x >= y - lubridate::days(back) & x <= y))
}
