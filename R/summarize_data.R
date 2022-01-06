# summarize_data.R

#' Summary calculations for continuous infusion data
#'
#' \code{summarize_drips} summarizes continuous infusion data
#'
#' This function takes a data frame with continuous infusion data which contains
#' the running time at each rate and produces a data frame with summary data for
#' each patient. The summary calculations include: first rate, last rate,
#' minimum rate, maximum rate, AUC, time-weighted average rate, total infusion
#' duration, total infusion running time, and cumulative dose.
#'
#' @param df A data frame with continuous data
#' @param ... Optional columns to group by
#' @param .id Patient identifier column, defaults to encntr_id
#' @param .med Medication column, defaults to medication
#' @param .rate Infusion rate column, defaults to rate
#' @param .units A string specifying the time units to use in calculations,
#'   default is "hours"
#'
#' @return A data frame
#'
#' @export
summarize_drips <- function(df, ..., .id = encntr_id, .med = medication,
                            .rate = rate, .units = "hours") {
    # turn off scientific notation
    options(scipen = 999)

    drip_count <- rlang::sym("drip_count")
    duration <- rlang::sym("duration")
    start_time <- rlang::sym("start_time")
    rate_start <- rlang::sym("rate_start")
    rate_stop <- rlang::sym("rate_stop")
    auc_val <- rlang::sym("auc_val")

    # get last and min non-zero rate
    nz_rate <- df |>
        dplyr::group_by({{ .id }}, {{ .med }}, ..., !!drip_count) |>
        dplyr::filter({{ .rate }} > 0) |>
        dplyr::summarize(
            !!"last_rate" := dplyr::last({{ .rate }}),
            !!"min_rate" := min({{ .rate }}, na.rm = TRUE),
            !!"start_time" := sum(!!duration, na.rm = TRUE)
        )

    # get first and max rates and AUC
    df |>
        dplyr::group_by({{ .id }}, {{ .med }}, ..., !!drip_count) |>
        dplyr::summarize(
            !!"start_datetime" := dplyr::first(!!rate_start),
            !!"stop_datetime" := dplyr::if_else(
                dplyr::last({{ .rate }}) == 0,
                dplyr::last(!!rate_start),
                dplyr::last(!!rate_stop)
            ),
            !!"cum_dose" := sum({{ .rate }} * !!duration, na.rm = TRUE),
            !!"first_rate" := dplyr::first({{ .rate }}),
            !!"max_rate" := max({{ .rate }}, na.rm = TRUE),
            !!"auc_val" := MESS::auc(!!start_time, {{ .rate }}),
            !!"duration" := dplyr::last(!!start_time)
        ) |>
        # join the last and min data, then calculate the time-weighted average
        # and interval
        dplyr::inner_join(
            nz_rate
            # by = c(
            #     rlang::as_name(id),
            #     rlang::as_name(med),
            #     purrr::map_chr(.grp_var, rlang::as_name),
            #     "drip_count"
            # )
        ) |>
        dplyr::group_by({{ .id }}, {{ .med }}, ..., !!drip_count) |>
        dplyr::mutate(!!"time_wt_avg_rate" := !!auc_val / !!duration) |>
        dplyr::ungroup() |>
        dplyr::rename(!!"infusion_run_time" := !!start_time)
}



#' Summarize home meds
#'
#' @param x data frame
#' @param ... columns
#' @param ref a reference
#' @param pts patients
#' @param home home meds vs discharge prescriptions
#'
#' @details The data frame passed to \code{ref} should contain three character
#'   columns: name, type, and group. The name column should contain either
#'   generic medication names or medication classes. The type column should
#'   specify whether the value in name is a "class" or "med". The group column
#'   should specify whether the medication is a continous ("cont") or scheduled
#'   ("sched") medication.
#'
#' @export
summarize_home_meds <- function(x, ..., ref, pts = NULL, home = TRUE) {
    id <- set_id_quo(x)
    type <- sym("type")
    # for any med classes, lookup the meds included in the class
    y <- filter(ref, !!type == "class")
    meds <- med_lookup(y$name)

    # join the list of meds with any indivdual meds included
    y <- filter(ref, !!type == "med")
    lookup.meds <- c(y$name, meds$med.name)

    # filter to either home medications or discharge medications, then use the
    # medication name or class to group by, then remove any duplicate patient /
    # group combinations, then convert the data to wide format
    if (home) {
        f <- parse_expr("med.type == 'Recorded / Home Meds'")
    } else {
        f <- parse_expr("med.type == 'Prescription / Discharge Order'")
    }

    df <- x |>
        filter(
            !!f,
            !!parse_expr("med %in% lookup.meds")
        ) |>
        left_join(meds, by = c("med" = "med.name")) |>
        mutate(
            !!"group" := !!parse_expr("dplyr::if_else(is.na(med.class), med, med.class)"),
            !!"value" := TRUE
        ) |>
        distinct(!!!quos(!!id, !!sym("group"), !!sym("value"))) |>
        tidyr::spread(!!sym("group"), !!sym("value"), fill = FALSE, drop = FALSE)

    # join with list of all patients, fill in values of FALSE for any patients
    # not in the data set
    if (!is.null(pts)) {
        df <- reclass(x, df) |>
            add_patients(pts)
    }

    reclass(x, df)
}

#' Summarize serial measurement data
#'
#' \code{summarize_data} calculates summary data for serial measurements.
#'
#' This function takes a data frame with serial measurement data which has been
#' transformed by \code{calc_runtime} and produces a data frame with summary
#' data for each patient.

#' @param df A data frame with continuous data
#' @param .grp_var column names to group by, wrapped by dplyr::vars
#' @param ... optional named arguments with column names
#'
#' @return tibble
#'
#' @export
summarize_data <- function(df, .grp_var, ...) {
    # turn off scientific notation
    options(scipen = 999)


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

    if ("result" %in% names(cols)) {
        result <- cols$result
    } else {
        result <- rlang::sym("result")
    }

    df |>
        dplyr::add_count(!!!.grp_var, !!event) |>
        group_by(!!!.grp_var, !!event, !!rlang::sym("n")) |>
        summarize(
            !!"first_datetime" := dplyr::first(!!event_datetime),
            !!"last_datetime" := dplyr::last(!!event_datetime),
            !!"cum_sum" := sum(!!result),
            !!"first_result" := dplyr::first(!!result),
            !!"last_result" := dplyr::last(!!result),
            !!"median_result" := stats::median(!!result, na.rm = TRUE),
            !!"max_result" := max(!!result, na.rm = TRUE),
            !!"min_result" := min(!!result, na.rm = TRUE),
            !!"auc" := MESS::auc(!!rlang::sym("start_time"), !!result),
            !!"duration" := dplyr::last(!!rlang::sym("start_time"))
        ) |>
        group_by(!!!.grp_var, !!event) |>
        dplyr::mutate_at("duration", as.numeric) |>
        mutate(!!"time_wt_avg" := !!rlang::sym("auc") / !!rlang::sym("duration")) |>
        ungroup()
}
