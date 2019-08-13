
#' Calculate morphine equivalents
#'
#' This takes a data frame with columns: medication, med_product, dose,
#' dose_units, frequency, and route and will convert the dose to the IV morphine
#' equivalent. The columns containing each of these parameters can optionally be
#' passed as named arguments if they differ from the default column names.
#'
#' @param df data_frame
#' @param ... optional named arguments with column names
#'
#' @return data_frame
#'
#' @references Lexicomp - Opiod Agonist Conversion (from drug, to Morphine IM);
#'   https://www.cms.gov/Medicare/Prescription-Drug-Coverage/PrescriptionDrugCovContra/Downloads/Opioid-Morphine-EQ-Conversion-Factors-March-2015.pdf
#'    (note, doses listed in oral mme, converted to iv mme by * 0.3);
#'   http://clincalc.com/opioids/
#'
#' @export
calc_morph_eq <- function(df, ...) {
    cols <- rlang::enquos(...)

    if ("medication" %in% names(cols)) {
        med <- cols$medication
    } else {
        med <- rlang::sym("medication")
    }

    if ("med_product" %in% names(cols)) {
        med_product <- cols$med_product
    } else {
        med_product <- rlang::sym("med_product")
    }

    if ("dose" %in% names(col)) {
        med_dose <- cols$dose
    } else {
        med_dose <- rlang::sym("dose")
    }

    if ("dose_unit" %in% names(col)) {
        med_dose_units <- cols$dose_unit
    } else {
        med_dose_units <- rlang::sym("dose_unit")
    }

    if ("frequency" %in% names(col)) {
        frequency <- cols$frequency
    } else {
        frequency <- rlang::sym("frequency")
    }

    if ("route" %in% names(col)) {
        route <- cols$route
    } else {
        route <- rlang::sym("route")
    }

    route_group <- rlang::sym("route_group")
    dose_mg <- rlang::sym("dose_mg")

    # parse product name to get tablet strength, needed for combination products
    products <- rlang::quos(
        stringr::str_detect(!!med_product, "7.5-200mg") ~ 7.5,
        stringr::str_detect(!!med_product, "10/325|325( )?(mg)?-10") ~ 10,
        stringr::str_detect(!!med_product, "325( )?(mg)?-7.5") ~ 7.5,
        stringr::str_detect(!!med_product, "300( mg)?-15") ~ 15,
        stringr::str_detect(!!med_product, "#3|300( mg)?-30") ~ 30,
        stringr::str_detect(!!med_product, "#4|300( mg)?-60|325( mg)?-60") ~ 60,
        stringr::str_detect(!!med_product, "50 mg") ~ 50,
        stringr::str_detect(!!med_product, "100 mg") ~ 100,
        stringr::str_detect(!!med_product, "325 mg -7.5 mg/15ml") ~ 0.5,
        stringr::str_detect(!!med_product, "5 ml 120-12 mg/5 ml") ~ 2.4,
        stringr::str_detect(!!med_product, "10 mg/ml") ~ 10,
        stringr::str_detect(!!med_product, "2.5 mg/2.5 ml") ~ 1,
        stringr::str_detect(!!med_product, "16.2-30") ~ 30,
        stringr::str_detect(!!med_product, "12 microgram/hr") ~ 12,
        stringr::str_detect(!!med_product, "25 microgram/hr") ~ 25,
        stringr::str_detect(!!med_product, "50 microgram/hr") ~ 50,
        stringr::str_detect(!!med_product, "75 microgram/hr") ~ 75,
        stringr::str_detect(!!med_product, "100 microgram/hr") ~ 100,
        stringr::str_detect(!!med_product, "8 mg - 2 mg") ~ 8,
        stringr::str_detect(!!med_product, "5/325|325( )?(mg)?-5|5( )?mg") ~ 5,
        !!med_product == "acetaminophen-hydrocodone" ~ 5,
        !!med_product == "acetaminophen-codeine" ~ 30
    )

    # determine frequency for patches
    freq <- rlang::quos(
        stringr::str_detect(!!frequency, "Q72|ONCE") & route_group == "TOP" ~ 3,
        stringr::str_detect(!!frequency, "Q48") & route_group == "TOP" ~ 2
    )

    # conversion to morphine equivalents
    convert <- rlang::quos(
        !!med == "buprenorphine" & route_group == "TOP" ~ 3.8,
        stringr::str_detect(!!med, "buprenorphine") & route_group == "PO" ~ 3.3,
        !!med == "butorphenol" ~ dose_mg * 5,
        stringr::str_detect(!!med, "codeine") & route_group == "PO" ~ dose_mg * 0.05,
        stringr::str_detect(!!med, "codeine") & route_group == "IV" ~ dose_mg * 0.1,
        !!med == "fentanyl" & route_group == "IV" ~ dose_mg * 0.1,
        !!med == "fentanyl" & route_group == "NASAL" ~ dose_mg * 0.16 * 0.3,
        !!med == "fentanyl" & route_group == "TOP" ~ dose_mg * dose_freq * 2.4 * 0.3,
        stringr::str_detect(!!med, "hydrocodone") ~ dose_mg * 0.3,
        !!med == "hydromorphone" & route_group == "PO" ~ dose_mg * 1.3,
        !!med == "hydromorphone" & route_group == "IV" ~ dose_mg * 6.7,
        !!med == "levorphanol" ~ dose_mg * 5,
        !!med == "mepridine" ~ dose_mg * 0.1,
        !!med == "methadone" & route_group == "PO" ~ dose_mg * 3 * 0.3,
        !!med == "morphine" & route_group == "PO" ~ dose_mg * 0.3,
        !!med == "nalbuphine" ~ dose_mg * 1,
        !!med == "opium" ~ dose_mg * 0.3,
        stringr::str_detect(!!med, "oxycodone") & route_group == "PO" ~ dose_mg * 0.5,
        !!med == "oxymorphone" & route_group == "PO" ~ dose_mg * 1,
        !!med == "oxymorphone" & route_group == "IV" ~ dose_mg * 10,
        !!med == "pentazocine" ~ dose_mg * 0.37 * 0.3,
        !!med == "tapentadol" ~ dose_mg * 0.1,
        !!med == "remifentanil" & med_dose_units == "mg" ~ dose_mg * 100, # dose recorded in mg; assume equiv with fentanyl
        !!med == "sufentanil" ~ dose_mg * 0.5, # 100mcg = 50mg
        !!med == "tramadol" ~ dose_mg * 0.08
    )

    # group all oral routes
    routes_po <- c(
        "DHT",
        "GT",
        "JT",
        "NG",
        "NJ",
        "OGT",
        "PEG",
        "PO",
        "PR",
        "SL"
    )

    # group all IV routes
    routes_iv <- c(
        "IM",
        "IV",
        "IV Central",
        "IVP",
        "IVPB",
        "EPIDURAL",
        "DIALYSIS"
    )

    # group transdermal routes
    routes_top <- c("TOP", "Transdermal")

    # consolidate route groups
    routes <- rlang::quos(
        !!route %in% routes_po ~ "PO",
        !!route %in% routes_iv ~ "IV",
        !!route %in% routes_top ~ "TOP",
        !!route == "NASAL" ~ "NASAL"
    )

    # if frequency not included in data frame, assume any fentanyl patches are
    # q72h
    if (!("frequency" %in% colnames(df))) {
        df$frequency <- 3
    }

    df %>%
        dplyr::mutate_at(dplyr::vars(!!med), stringr::str_to_lower) %>%
        dplyr::mutate(
            !!"route_group" := dplyr::case_when(!!!routes),
            !!"tab_mg" := dplyr::case_when(!!!products),
            # calculate the mg given in each dose; needed when dose is charted
            # in number of tabs, etc.
            !!"dose_mg" := dplyr::if_else(
                !!med_dose_units %in% c("tab", "mL", "supp", "patch"),
                !!rlang::sym("tab_mg") * !!med_dose,
                !!med_dose
            ),
            !!"dose_freq" := dplyr::case_when(!!!freq),
            !!"mme_iv" := dplyr::case_when(!!!convert)
        )

}

