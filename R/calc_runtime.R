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
#' \code{.drip_off} argument to modify the criteria for determining distinct
#' infusions.

#' @param df A data frame
#' @param ... Optional columns to group by
#' @param .id Patient identifier column, defaults to encntr_id
#' @param .med Medication column, defaults to medication
#' @param .dt_tm Date/time column, defaults to med_datetime
#' @param .rate Infusion rate column, defaults to rate
#' @param .rate_unit Infusion rate units column, defaults to rate_unit
#' @param .drip_off Number of hours a medication infusion should be off to count
#'   as a new infusion, defaults to 12 hours
#' @param .no_doc Number of hours without documentation which will be used to
#'   indicate a drip has ended, defaults to 24 hours
#' @param .units A string specifying the time units to use in calculations,
#'   default is "hours"
#'
#' @return A data frame
#'
#' @export
drip_runtime <- function(df, ..., .id = encntr_id, .med = medication,
                         .dt_tm = med_datetime, .rate = rate,
                         .rate_unit = rate_unit, .drip_off = 12, .no_doc = 24,
                         .units = "hours") {

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
    start_time <- rlang::sym("start_time")

    df_drip <- df |>
        dplyr::group_by({{ .id }}, {{ .med }}, ...) |>
        dplyr::arrange({{ .dt_tm }}, .by_group = TRUE) |>

        # determine if it's a valid rate documentation
        dplyr::mutate(
            dplyr::across({{ .rate }}, ~dplyr::if_else(is.na({{ .rate_unit }}), NA_real_, .))
        ) |>

        # fill in missing rates
        tidyr::fill({{ .rate }}, .direction = "down") |>

        # calculate time between rows and order of rate changes
        dplyr::mutate(
            !!"time_next" := difftime(dplyr::lead({{ .dt_tm }}), {{ .dt_tm }}, units = .units),
            !!"rate_change" := is.na(dplyr::lag({{ .rate }})) | {{ .rate }} != dplyr::lag({{ .rate }}),
            !!"change_num" := cumsum(!!rate_change)
        ) |>

        # calculate how long the drip was at each rate
        dplyr::group_by({{ .id }}, {{ .med }}, ..., !!change_num) |>
        dplyr::summarize(
            dplyr::across({{ .rate }}, dplyr::first),
            !!"rate_start" := dplyr::first({{ .dt_tm }}),
            !!"rate_stop" := dplyr::last({{ .dt_tm }}),
            !!"rate_duration" := difftime(!!rate_stop, !!rate_start, units = .units),
            dplyr::across(!!time_next, dplyr::last)
        ) |>

        # identify individual drips
        dplyr::group_by({{ .id }}, {{ .med }}, ...) |>
        dplyr::mutate(
            !!"duration" := dplyr::if_else(
                !!time_next < .drip_off & !is.na(!!time_next),
                !!rate_duration + !!time_next,
                !!rate_duration
            ),
            dplyr::across(!!duration, as.numeric),
            !!"drip_stop" := is.na(!!time_next) | !!time_next > .no_doc |
                ({{ .rate }} == 0 & !!duration > .drip_off),
            !!"drip_start" := !!change_num == 1 | dplyr::lag(!!drip_stop),
            !!"drip_count" := cumsum(!!drip_start)
        ) |>

        # calculate run time
        dplyr::group_by({{ .id }}, {{ .med }}, ..., !!drip_count) |>
        dplyr::mutate(
            !!"start_time" := difftime(!!rate_start, dplyr::first(!!rate_start), units = .units)
        ) |>

        # remove unnecessary columns
        dplyr::select(-!!rate_duration, -!!time_next, -!!drip_start, -!!drip_stop, -!!change_num)

    # update drip stop information if rate of last row isn't 0
    drip_end <- df_drip |>
        dplyr::filter(
            !!rate_stop == dplyr::last(!!rate_stop),
            {{ .rate }} > 0
        ) |>

        # calculate the run time for the last drip row
        dplyr::mutate(
            across(!!start_time, ~ . + !!duration),
            across(c({{ .rate }}, !!duration), ~ 0),
            across(!!rate_start, ~ !!rate_stop)
        ) |>
        dplyr::ungroup()

    # bind the rows with drip end data and arrange by date/time; need to ungroup
    # first for bind_rows to keep edwr class assigment
    df_drip |>
        dplyr::ungroup() |>
        dplyr::bind_rows(drip_end) |>
        dplyr::arrange({{ .id }}, {{ .med }}, ..., !!drip_count, !!rate_start) |>
        dplyr::distinct({{ .id }}, {{ .med }}, ..., !!drip_count, !!start_time, .keep_all = TRUE)
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
#' @param ... Optional columns to group by
#' @param .id Patient identifier column, defaults to encntr_id
#' @param .med Medication column, defaults to medication
#' @param .dt_tm Date/time column, defaults to med_datetime
#' @param .dose Dose column, defaults to dose
#' @param .med_off Number of hours between medication doses which will be
#'   counted as a new course, defaults to 36 hours
#' @param .no_doc Number of hours without documentation which will be used to
#'   indicate a course has ended, defaults to 24 hours
#' @param .units A string specifying the time units to use in calculations,
#'   default is "hours"
#'
#' @return A data frame
#'
#' @export
med_runtime <- function(df, ..., .id = encntr_id, .med = medication,
                        .dt_tm = med_datetime, .dose = dose, .med_off = 36,
                        .no_doc = 24, .units = "hours") {

    time_next <- rlang::sym("time_next")
    dose_change <- rlang::sym("dose_change")
    change_num <- rlang::sym("change_num")
    num_doses <- rlang::sym("num_doses")
    dose_start <- rlang::sym("dose_start")
    dose_stop <- rlang::sym("dose_stop")
    dose_duration <- rlang::sym("dose_duration")
    course_start <- rlang::sym("course_start")
    course_stop <- rlang::sym("course_stop")
    course_count <- rlang::sym("course_count")
    duration <- rlang::sym("duration")

    df_meds <- df |>
        dplyr::group_by({{ .id }}, {{ .med }}, ...) |>
        dplyr::arrange({{ .dt_tm }}, .by_group = TRUE) |>

        # calculate time between rows and order of dose changes
        dplyr::mutate(
            !!"time_next" := difftime(dplyr::lead({{ .dt_tm }}), {{ .dt_tm }}, units = .units),
            !!"dose_change" := is.na(dplyr::lag({{ .dose }})) | {{ .dose }} != dplyr::lag({{ .dose }}),
            !!"change_num" := cumsum(!!dose_change)
        ) |>

        # calculate how long the med was at each dose
        dplyr::group_by({{ .id }}, {{ .med }}, ..., !!change_num) |>
        dplyr::summarize(
            !!"num_doses" := dplyr::n(),
            dplyr::across({{ .dose }}, dplyr::first),
            !!"dose_start" := dplyr::first({{ .dt_tm }}),
            !!"dose_stop" := dplyr::last({{ .dt_tm }}),
            !!"dose_duration" := difftime(!!dose_stop, !!dose_start, units = .units),
            !!"time_next" := dplyr::last(!!time_next)
        ) |>

        # identify individual courses of therapy
        dplyr::group_by({{ .id }}, {{ .med }}, ...) |>
        dplyr::mutate(
            !!"duration" := dplyr::if_else(
                !!time_next <= .med_off & !is.na(!!time_next),
                !!dose_duration + !!time_next,
                !!dose_duration
            ),
            dplyr::across(!!duration, as.numeric),
            !!"course_stop" := is.na(!!time_next) | !!time_next > .med_off |
                ({{ .dose }} == 0 & !!duration > .no_doc),
            !!"course_start" := !!change_num == 1 | dplyr::lag(!!course_stop),
            !!"course_count" := cumsum(!!course_start)
        ) |>

        # calculate run time
        dplyr::group_by({{ .id }}, {{ .med }}, ..., !!course_count) |>
        dplyr::mutate(
            !!"start_time" := difftime(!!dose_start, dplyr::first(!!dose_start), units = .units)
        ) |>

        # remove unnecessary columns
        dplyr::select(
            -!!dose_duration,
            -!!time_next,
            -!!course_start,
            -!!course_stop,
            -!!change_num
        ) |>
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
