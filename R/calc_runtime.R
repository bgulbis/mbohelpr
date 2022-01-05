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
#' @param ... optional named arguments with column names; optional arguments
#'   include: id, med, med_datetime, rate, and rate_unit; if not specified,
#'   encntr_id, medication, med_datetime, rate, and rate_unit are used for
#'   column names, respectively
#' @param .grp_var additional columns to group by, uses \code{tidy_select}
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
drip_runtime <- function(df, ..., .grp_var, drip_off = 12, no_doc = 24, units = "hours") {
    cols <- rlang::enquos(...)

    if ("id" %in% names(cols)) {
        id <- cols$id
    } else {
        id <- rlang::sym("encntr_id")
    }

    if ("med" %in% names(cols)) {
        med <- cols$med
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
    # rate_curr <- rlang::sym("rate_curr")
    rate_duration <- rlang::sym("rate_duration")
    rate_start <- rlang::sym("rate_start")
    rate_stop <- rlang::sym("rate_stop")
    time_next <- rlang::sym("time_next")
    drip_start <- rlang::sym("drip_start")
    drip_stop <- rlang::sym("drip_stop")
    drip_count <- rlang::sym("drip_count")
    duration <- rlang::sym("duration")

    df_drip <- df |>
        dplyr::group_by(!!id, !!med, {{ .grp_var }}) |>
        dplyr::arrange(!!med_datetime, .by_group = TRUE) |>

        # determine if it's a valid rate documentation
        dplyr::mutate(
            !!"rate_change" := !is.na(!!rate_unit),
            !!"change_num" := cumsum(!!rate_change)
        ) |>

        # fill in missing rates
        dplyr::group_by(!!id, !!med, {{ .grp_var }}, !!change_num) |>
        dplyr::mutate(
            !!"rate" := dplyr::if_else(
                is.na(!!rate_unit),
                dplyr::first(!!rate),
                !!rate
            )
        ) |>

        # calculate time between rows and order of rate changes
        dplyr::group_by(!!id, !!med, {{ .grp_var }}) |>
        dplyr::mutate(
            !!"time_next" := difftime(
                dplyr::lead(!!med_datetime),
                !!med_datetime,
                units = units
            ),
            !!"rate_change" := is.na(dplyr::lag(!!rate)) |
                rate != dplyr::lag(!!rate),
            !!"change_num" := cumsum(!!rate_change)
        ) |>

        # calculate how long the drip was at each rate
        dplyr::group_by(!!id, !!med, {{ .grp_var }}, !!change_num) |>
        dplyr::summarize(
            # !!"rate_curr" := dplyr::first(!!rate),
            dplyr::across(!!rate, dplyr::first),
            !!"rate_start" := dplyr::first(!!med_datetime),
            !!"rate_stop" := dplyr::last(!!med_datetime),
            !!"rate_duration" := difftime(
                dplyr::last(!!med_datetime),
                dplyr::first(!!med_datetime),
                units = units
            ),
            !!"time_next" := dplyr::last(!!time_next)
        ) |>

        # identify individual drips
        dplyr::group_by(!!id, !!med, {{ .grp_var }}) |>
        dplyr::mutate(
            !!"duration" := dplyr::if_else(
                !!time_next < drip_off & !is.na(!!time_next),
                !!rate_duration + !!time_next,
                !!rate_duration
            ),
            dplyr::across(!!duration, as.numeric),
            !!"drip_stop" := is.na(!!time_next) | !!time_next > no_doc |
                (!!rate == 0 & !!duration > drip_off),
            !!"drip_start" := !!change_num == 1 | dplyr::lag(!!drip_stop),
            !!"drip_count" := cumsum(!!drip_start)
        ) |>
        # dplyr::mutate_at("duration", as.numeric) |>

        # calculate run time
        dplyr::group_by(!!id, !!med, {{ .grp_var }}, !!drip_count) |>
        dplyr::mutate(
            !!"start_time" := difftime(
                !!rate_start,
                dplyr::first(!!rate_start),
                units = units
            )
        ) |>

        # remove unnecessary columns
        dplyr::select(
            -!!rate_duration,
            -!!time_next,
            -!!drip_start,
            -!!drip_stop,
            -!!change_num
        )

    # update drip stop information if rate of last row isn't 0
    drip_end <- df_drip |>
        dplyr::filter(
            !!rate_stop == dplyr::last(!!rate_stop),
            !!rate > 0
        ) |>

        # calculate the run time for the last drip row
        dplyr::mutate(
            !!"start_time" := !!duration + !!rlang::sym("start_time"),
            !!"rate_start" := !!rate_stop,
            !!"duration" := 0
        ) |>
        dplyr::ungroup()

    # bind the rows with drip end data and arrange by date/time; need to ungroup
    # first for bind_rows to keep edwr class assigment
    df_drip |>
        dplyr::ungroup() |>
        dplyr::bind_rows(drip_end) |>
        dplyr::arrange(!!id, !!med, {{ .grp_var }}, !!drip_count, !!rate_start) |>
        dplyr::distinct(
            !!id,
            !!med,
            {{ .grp_var }},
            !!drip_count,
            !!rlang::sym("start_time"),
            .keep_all = TRUE
        )
}

#' Calculate the running time for intermittent medication data
#'
#' \code{med_runtime} calculates the duration of time at current value and
#' total run time
#'
#' This function takes a data frame with intermittent medication data and produces a
#' data frame with the with the duration of time at each rate and cumulative run
#' time.
#'
#' This could be used to then calculate the AUC or to summarize the medication course data.
#'
#' The data will be grouped into distinct courses of medications, for patients who
#' may have been restarted on the medication one or more times. Use the
#' \code{med_off} argument to modify the criteria for determining distinct
#' courses.

#' @param df A data frame
#' @param ... optional named arguments with column names; optional arguments
#'   are: id, med, med_datetime, and dose; if not specified, encntr_id, medication,
#'   med_datetime, and dose are used for column names, respectively
#' @param .grp_var column names to group by, uses \code{tidy_select}
#' @param med_off An optional numeric indicating the number of hours between
#'   medication doses which will be counted as a new course, defaults to 36
#'   hours
#' @param no_doc An optional numeric indicating the number of hours without
#'   documentation which will be used to indicate a course has ended, defaults
#'   to 24 hours
#' @param units An optional character string specifying the time units to use in
#'   calculations, default is hours
#'
#' @return A data frame
#'
#' @export
med_runtime <- function(df, ..., .grp_var, med_off = 24, no_doc = 24, units = "hours") {
    cols <- rlang::enquos(...)

    if ("id" %in% names(cols)) {
        id <- cols$id
    } else {
        id <- rlang::sym("encntr_id")
    }

    if ("med" %in% names(cols)) {
        med <- cols$med
    } else {
        med <- rlang::sym("medication")
    }

    if ("med_datetime" %in% names(cols)) {
        med_datetime <- cols$med_datetime
    } else {
        med_datetime <- rlang::sym("med_datetime")
    }

    if ("dose" %in% names(cols)) {
        dose <- cols$dose
    } else {
        dose <- rlang::sym("dose")
    }

    change_num <- rlang::sym("change_num")
    dose_change <- rlang::sym("dose_change")
    time_next <- rlang::sym("time_next")
    num_doses <- rlang::sym("num_doses")
    # curr_dose <- rlang::sym("curr_dose")
    dose_start <- rlang::sym("dose_start")
    dose_stop <- rlang::sym("dose_stop")
    dose_duration <- rlang::sym("dose_duration")
    course_start <- rlang::sym("course_start")
    course_stop <- rlang::sym("course_stop")
    course_count <- rlang::sym("course_count")
    duration <- rlang::sym("duration")

    df_meds <- df |>
        dplyr::group_by(!!id, !!med, {{ .grp_var }}) |>
        dplyr::arrange(!!med_datetime, .by_group = TRUE) |>

        # calculate time between rows and order of dose changes
        dplyr::mutate(
            !!"time_next" := difftime(
                dplyr::lead(!!med_datetime),
                !!med_datetime,
                units = units
            ),
            !!"dose_change" := is.na(dplyr::lag(!!dose)) |
                !!dose != dplyr::lag(!!dose),
            !!"change_num" := cumsum(!!dose_change)
        ) |>

        # calculate how long the med was at each dose
        dplyr::group_by(!!id, !!med, {{ .grp_var }}, !!change_num) |>
        dplyr::summarize(
            !!"num_doses" := dplyr::n(),
            dplyr::across(!!dose, dplyr::first),
            # !!"curr_dose" := dplyr::first(!!dose),
            !!"dose_start" := dplyr::first(!!med_datetime),
            !!"dose_stop" := dplyr::last(!!med_datetime),
            !!"dose_duration" := difftime(
                dplyr::last(!!med_datetime),
                dplyr::first(!!med_datetime),
                units = units
            ),
            !!"time_next" := dplyr::last(!!time_next)
        ) |>

        # identify individual courses of therapy
        dplyr::group_by(!!id, !!med, {{ .grp_var }}) |>
        dplyr::mutate(
            !!"duration" := dplyr::if_else(
                !!time_next <= med_off & !is.na(!!time_next),
                !!dose_duration + !!time_next,
                !!dose_duration
            ),
            dplyr::across(!!duration, as.numeric),
            !!"course_stop" := is.na(!!time_next) | !!time_next > med_off |
                (!!dose == 0 & !!duration > no_doc),
            !!"course_start" := !!change_num == 1 | dplyr::lag(!!course_stop),
            !!"course_count" := cumsum(!!course_start)
        ) |>

        # calculate run time
        dplyr::group_by(!!id, !!med, {{ .grp_var }}, !!course_count) |>
        dplyr::mutate(
            !!"start_time" := difftime(
                !!dose_start,
                dplyr::first(!!dose_start),
                units = units
            )
        ) |>

        # remove unnecessary columns
        dplyr::select(
            -!!dose_duration,
            -!!time_next,
            -!!course_start,
            -!!course_stop,
            -!!change_num
        ) |>
        # dplyr::rename(!!"dose" := !!curr_dose) |>
        dplyr::ungroup()

    # update drip stop information if rate of last row isn't 0
    # drip_end <- df_drip |>
    #     dplyr::filter(
    #         !!rate_stop == dplyr::last(!!rate_stop),
    #         !!rate > 0
    #     ) |>
    #
    #     # calculate the run time for the last drip row
    #     dplyr::mutate(
    #         !!"start_time" := !!duration + !!rlang::sym("start_time"),
    #         !!"rate_start" := !!rate_stop,
    #         !!"duration" := 0
    #     ) |>
    #     dplyr::ungroup()

    # bind the rows with drip end data and arrange by date/time; need to ungroup
    # first for bind_rows to keep edwr class assigment
    # df_drip |>
    #     dplyr::ungroup() |>
    #     dplyr::bind_rows(drip_end) |>
    #     dplyr::arrange(!!!.grp_var, !!med, !!drip_count, !!rate_start) |>
    #     dplyr::distinct(
    #         !!!.grp_var,
    #         !!med,
    #         !!drip_count,
    #         !!rlang::sym("start_time"),
    #         .keep_all = TRUE
    #     )
    df_meds
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
#' @param ... optional named arguments with column names; optional arguments
#'   are: id, event, and event_datetime; if not specified, encntr_id, event, and
#'   event_datetime are used for column names, respectively
#' @param .grp_var column names to group by, uses \code{tidy_select}
#' @param units An optional character string specifying the time units to use in
#'   calculations, default is hours
#'
#' @return A data frame
#'
#' @export
calc_runtime <- function(df, ..., .grp_var, units = "hours") {
    cols <- rlang::enquos(...)

    if ("id" %in% names(cols)) {
        id <- cols$id
    } else {
        id <- rlang::sym("encntr_id")
    }

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

    # if ("result" %in% names(cols)) {
    #     result <- cols$result
    # } else {
    #     result <- rlang::sym("result")
    # }

    df |>
        dplyr::group_by(!!id, !!event, {{ .grp_var }}) |>
        dplyr::arrange(!!event_datetime, .by_group = TRUE) |>
        dplyr::mutate(
            !!"duration" := difftime(
                !!event_datetime,
                dplyr::lag(!!event_datetime),
                units = units
            ),
            !!"duration" := as.numeric(!!sym("duration")),
            !!"duration" := dplyr::coalesce(!!sym("duration"), 0),
            !!"start_time" := difftime(
                !!event_datetime,
                dplyr::first(!!event_datetime),
                units = units
            )
        ) |>
        dplyr::ungroup() |>
        dplyr::distinct(
            !!id,
            !!event,
            {{ .grp_var }},
            !!rlang::sym("start_time"),
            .keep_all = TRUE
        )

}
