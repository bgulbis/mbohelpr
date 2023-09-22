
#' Calculate SOFA score
#'
#' This takes a data frame with columns for each parameter need to calculate the
#' SOFA score. The columns containing each of these parameters can optionally be
#' passed as named arguments if they differ from the default column names.
#'
#' @param df data_frame
#' @param .id Patient identifier column, defaults to encntr_id
#' @param .pao2 Column containing the minimum PaO2 value in the previous 24
#'   hours, defaults to pao2
#' @param .fio2 Column containing the maximum FiO2 value in the previous 24
#'   hours, defaults to fio2
#' @param .intubated Column containing a logical which indicates whether the
#'   patient was intubated in the previous 24 hours, defaults to intubated
#' @param .platelet Column containing the minimum platelets value in the
#'   previous 24 hours, defaults to platelet
#' @param .gcs Column containing the minimum Glasgow Coma Score in the previous
#'   24 hours, defaults to gcs
#' @param .bili Column containing the maximum total bilirubin in the previous 24
#'   hours, defaults to bili
#' @param .map Column containing the minimum mean arterial pressure in the
#'   previous 24 hours, defaults to map
#' @param .scr Column containing the maximum serum creatinine in the previous 24
#'   hours, defaults to scr
#' @param .uop Column containing the urine output for the previous 24 hours,
#'   defaults to uop
#' @param .dopamine Column containing the maximum dopamine rate in the previous
#'   24 hours, defaults to dopamine
#' @param .dobutamine Column containing the maximum dobutamine rate in the
#'   previous 24 hours, defaults to dobutamine
#' @param .epinephrine Column containing the maximum epinephrine rate in the
#'   previous 24 hours, defaults to epinephrine
#' @param .norepinephrine Column containing the maximum norepinephrine rate in
#'   the previous 24 hours, defaults to norepinephrine
#'
#' @return data_frame
#'
#' @references Sequential Organ Failure Assessment (SOFA) Score:
#'   https://www.mdcalc.com/calc/691/sequential-organ-failure-assessment-sofa-score
#'
#' @export
calc_sofa <- function(df, .id = encntr_id, .pao2 = pao2, .fio2 = fio2,
                      .intubated = intubated, .platelet = platelet, .gcs = gcs,
                      .bili = bili, .map = map, .scr = scr, .uop = uop,
                      .dopamine = dopamine, .dobutamine = dobutamine,
                      .epinephrine = epinephrine, .norepinephrine = norepinephrine) {

    plt_score <- rlang::sym("plt_score")
    gcs_score <- rlang::sym("gcs_score")
    bili_score <- rlang::sym("bili_score")
    map_score <- rlang::sym("map_score")
    scr_score <- rlang::sym("scr_score")
    pao2_fio2 <- rlang::sym("pao2_fio2")
    vent_score <- rlang::sym("vent_score")

    df |>
        dplyr::group_by({{ .id }}) |>
        dplyr::mutate(
            !!"plt_score" := dplyr::case_when(
                {{ .platelet }} >= 150 ~ 0,
                {{ .platelet }} >= 100 ~ 1,
                {{ .platelet }} >= 50 ~ 2,
                {{ .platelet }} >= 20 ~ 3,
                {{ .platelet }} < 20 ~ 4
            ),
            !!"gcs_score" := dplyr::case_when(
                {{ .gcs }} >= 15 ~ 0,
                {{ .gcs }} >= 13 ~ 1,
                {{ .gcs }} >= 10 ~ 2,
                {{ .gcs }} >= 6 ~ 3,
                {{ .gcs }} < 6 ~ 4
            ),
            !!"bili_score" := dplyr::case_when(
                {{ .bili }} < 1.2 ~ 0,
                {{ .bili }} <= 1.9 ~ 1,
                {{ .bili }} <= 5.9 ~ 2,
                {{ .bili }} >= 6 ~ 3
            ),
            !!"map_score" := dplyr::case_when(
                {{ .dopamine }} > 15 | {{ .epinephrine }} > 0.1 | {{ .norepinephrine }} > 0.1 ~ 4,
                {{ .dopamine }} > 5 | {{ .epinephrine }} <= 0.1 | {{ .norepinephrine }} <= 0.1 ~ 3,
                {{ .dopamine }} <= 5 | !is.na({{ .dobutamine }}) ~ 2,
                {{ .map }} < 70 ~ 1,
                TRUE ~ 0
            ),
            !!"scr_score" := dplyr::case_when(
                {{ .scr }} >= 5 | {{ .uop }} < 200 ~ 4,
                {{ .scr }} >= 3.5 | {{ .uop }} < 500 ~ 3,
                {{ .scr }} >= 2 ~ 2,
                {{ .scr }} >= 1.2 ~ 1,
                TRUE ~ 0
            ),
            !!"pao2_fio2" := {{ .pao2 }} / ({{ .fio2 }} / 100),
            dplyr::across({{ .intubated }}, \(x) dplyr::coalesce(x, FALSE)),
            !!"vent_score" := dplyr::case_when(
                {{ .intubated }} & !!pao2_fio2 < 100 ~ 4,
                {{ .intubated }} & !!pao2_fio2 <= 199 ~ 3,
                !!pao2_fio2 <= 299 ~ 2,
                !!pao2_fio2 <= 399 ~ 1,
                TRUE ~ 0
            ),
            !!"sofa" := sum(!!plt_score, !!gcs_score, !!bili_score, !!scr_score, !!map_score, !!vent_score, na.rm = TRUE)
        )
}