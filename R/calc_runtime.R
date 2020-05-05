# calc_runtime.R

#' Calculate the running time for continuous infusion data
#'
#' \code{drip_runtime} calculates the duration of time at current value and
#' total run time
#'
#' This function takes a data frame with continuous infusion data and produces a
#' data frame with the with the duration of time at each rate and cumulative run
#' time.
#'
#' This could be used to then calculate the AUC or to summarize the continuous
#' data.
#'
#' The data will be grouped into distinct sets of infusions, for patients who
#' may have been restarted on the drip one or more times. Use the
#' \code{drip_off} argument to modify the criteria for determining distinct
#' infusions.

#' @param df A data frame
#' @param .grp_var column names to group by, wrapped by dplyr::vars
#' @param ... optional named arguments with column names
#' @param drip_off An optional numeric indicating the number of hours a
#'   medication infusion should be off to count as a new infusion, defaults to
#'   12 hours
#' @param no_doc An optional numeric indicating the number of hours without
#'   documentation which will be used to indicate a drip has ended, defaults to
#'   24 hours
#' @param units An optional character string specifying the time units to use in
#'   calculations, default is hours
#'
#' @return A data frame
#'
#' @export
drip_runtime <- function(df, .grp_var, ..., drip_off = 12, no_doc = 24, units = "hours") {
    cols <- rlang::enquos(...)

    # if ("id" %in% names(cols)) {
    #     id <- cols$id
    # } else {
    #     id <- rlang::sym("encounter_id")
    # }

    if ("medication" %in% names(cols)) {
        med <- cols$medication
    } else {
        med <- rlang::sym("medication")
    }

    if ("med_datetime" %in% names(cols)) {
        med_datetime <- cols$med_datetime
    } else {
        med_datetime <- rlang::sym("med_datetime")
    }

    if ("rate" %in% names(cols)) {
        rate <- cols$rate
    } else {
        rate <- rlang::sym("rate")
    }

    if ("rate_unit" %in% names(cols)) {
        rate_unit <- cols$rate_unit
    } else {
        rate_unit <- rlang::sym("rate_unit")
    }

    change_num <- rlang::sym("change_num")
    rate_change <- rlang::sym("rate_change")
    rate_duration <- rlang::sym("rate_duration")
    rate_start <- rlang::sym("rate_start")
    rate_stop <- rlang::sym("rate_stop")
    time_next <- rlang::sym("time_next")
    drip_start <- rlang::sym("drip_start")
    drip_stop <- rlang::sym("drip_stop")
    drip_count <- rlang::sym("drip_count")
    duration <- rlang::sym("duration")

    df_drip <- df %>%
        dplyr::group_by(!!!.grp_var, !!med) %>%
        dplyr::arrange(!!med_datetime, .by_group = TRUE) %>%

        # determine if it's a valid rate documentation
        dplyr::mutate(
            !!"rate_change" := !is.na(!!rate_unit),
            !!"change_num" := cumsum(!!rate_change)
        ) %>%

        # fill in missing rates
        dplyr::group_by(!!!.grp_var, !!med, !!change_num) %>%
        dplyr::mutate(
            !!"rate" := dplyr::if_else(
                is.na(!!rate_unit),
                dplyr::first(!!rate),
                !!rate
            )
        ) %>%

        # calculate time between rows and order of rate changes
        dplyr::group_by(!!!.grp_var, !!med) %>%
        dplyr::mutate(
            !!"time_next" := difftime(
                dplyr::lead(!!med_datetime),
                !!med_datetime,
                units = units
            ),
            !!"rate_change" := is.na(dplyr::lag(!!rate)) |
                rate != dplyr::lag(!!rate),
            !!"change_num" := cumsum(!!rate_change)
        ) %>%

        # calculate how long the drip was at each rate
        dplyr::group_by(!!!.grp_var, !!med, !!change_num) %>%
        dplyr::summarize(
            !!"rate" := dplyr::first(!!rate),
            !!"rate_start" := dplyr::first(!!med_datetime),
            !!"rate_stop" := dplyr::last(!!med_datetime),
            !!"rate_duration" := difftime(
                dplyr::last(!!med_datetime),
                dplyr::first(!!med_datetime),
                units = units
            ),
            !!"time_next" := dplyr::last(!!time_next)
        ) %>%

        # identify individual drips
        dplyr::group_by(!!!.grp_var, !!med) %>%
        dplyr::mutate(
            !!"duration" := dplyr::if_else(
                !!time_next < drip_off & !is.na(!!time_next),
                !!rate_duration + !!time_next,
                !!rate_duration
            ),
            !!"drip_stop" := is.na(!!time_next) | !!time_next > no_doc |
                (!!rate == 0 & !!duration > drip_off),
            !!"drip_start" := !!change_num == 1 | dplyr::lag(!!drip_stop),
            !!"drip_count" := cumsum(!!drip_start)
        ) %>%
        dplyr::mutate_at("duration", as.numeric) %>%

        # calculate run time
        dplyr::group_by(!!!.grp_var, !!med, !!drip_count) %>%
        dplyr::mutate(
            !!"start_time" := difftime(
                !!rate_start,
                dplyr::first(!!rate_start),
                units = units
            )
        ) %>%

        # remove unnecessary columns
        dplyr::select(
            -!!rate_duration,
            -!!time_next,
            -!!drip_start,
            -!!drip_stop,
            -!!change_num
        )

    # update drip stop information if rate of last row isn't 0
    drip_end <- df_drip %>%
        dplyr::filter(
            !!rate_stop == dplyr::last(!!rate_stop),
            !!rate > 0
        ) %>%

        # calculate the run time for the last drip row
        dplyr::mutate(
            !!"start_time" := !!duration + !!rlang::sym("start_time"),
            !!"rate_start" := !!rate_stop,
            !!"duration" := 0
        ) %>%
        dplyr::ungroup()

    # bind the rows with drip end data and arrange by date/time; need to ungroup
    # first for bind_rows to keep edwr class assigment
    df_drip %>%
        dplyr::ungroup() %>%
        dplyr::bind_rows(drip_end) %>%
        dplyr::arrange(!!!.grp_var, !!med, !!drip_count, !!rate_start) %>%
        dplyr::distinct(
            !!!.grp_var,
            !!med,
            !!drip_count,
            !!rlang::sym("start_time"),
            .keep_all = TRUE
        )
}

#' Calculate the running time for serial measurement data
#'
#' \code{calc_runtime} calculates the duration of time at current value and
#' total run time
#'
#' This function takes a data frame with serial measurement data and produces a
#' data frame with the with the duration of time at each rate and cumulative run
#' time.

#' @param df A data frame
#' @param .grp_var column names to group by, wrapped by dplyr::vars
#' @param ... optional named arguments with column names
#' @param units An optional character string specifying the time units to use in
#'   calculations, default is hours
#'
#' @return A data frame
#'
#' @export
calc_runtime <- function(df, .grp_var, ..., units = "hours") {
    cols <- rlang::enquos(...)

    # if ("id" %in% names(cols)) {
    #     id <- cols$id
    # } else {
    #     id <- rlang::sym("encounter_id")
    # }

    if ("event" %in% names(cols)) {
        event <- cols$event
    } else {
        event <- rlang::sym("event")
    }

    if ("event_datetime" %in% names(cols)) {
        event_datetime <- cols$event_datetime
    } else {
        event_datetime <- rlang::sym("event_datetime")
    }

    df %>%
        dplyr::group_by(!!!.grp_var, !!event) %>%
        dplyr::arrange(!!event_datetime, .by_group = TRUE) %>%
        dplyr::mutate(
            !!"duration" := difftime(
                !!event_datetime,
                dplyr::lag(!!event_datetime),
                units = units
            ),
            !!"duration" := dplyr::coalesce(!!sym("duration"), 0),
            !!"start_time" := difftime(
                !!event_datetime,
                dplyr::first(!!event_datetime),
                units = units
            )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::distinct(
            !!!.grp_var,
            !!event,
            !!rlang::sym("start_time"),
            .keep_all = TRUE
        )

}
