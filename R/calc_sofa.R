
#' Calculate SOFA score
#'
#' This takes a data frame with columns for each parameter need to calculate the
#' SOFA score. The columns containing each of these parameters can optionally be
#' passed as named arguments if they differ from the default column names.
#'
#' @param df data_frame
#' @param ... optional named arguments with column names
#'
#' @return data_frame
#'
#' @references Sequential Organ Failure Assessment (SOFA) Score: https://www.mdcalc.com/calc/691/sequential-organ-failure-assessment-sofa-score
#'
#' @export
calc_sofa <- function(df, .id = encntr_id, .pa02 = pao2, .fio2 = fio2,
                      .intubated = intubated, .platelet = platelet, .gcs = gcs,
                      .bili = bili, .map = map, .scr = scr, .uop = uop,
                      .dopamine = dopamine, .dobutamine = dobutamine,
                      .epinephrine = epinephrine, .norepinephrine = norepinephrine) {

    pao2_fio2 <- rlang::sym("pao2_fio2")

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