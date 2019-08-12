
#' Calculate morphine equivalents
#'
#' This takes a data frame with columns: med, med.dose, med.dose.units, and
#' route and will convert the med.dose to the morphine equivalent.
#'
#' @param df data_frame
#'
#' @return data_frame
#'
#' @references Lexicomp - Opiod Agonist Conversion (from drug, to Morphine IM);
#' https://www.cms.gov/Medicare/Prescription-Drug-Coverage/PrescriptionDrugCovContra/Downloads/Opioid-Morphine-EQ-Conversion-Factors-March-2015.pdf (note, doses listed in oral mme, converted to iv mme by * 0.3);
#' http://clincalc.com/opioids/
#'
#' @export
calc_morph_eq <- function(df) {
    med <- rlang::sym("med")
    route.group <- rlang::sym("route.group")
    dose.mg <- rlang::sym("dose.mg")
    med.dose.units <- rlang::sym("med.dose.units")

    # parse product name to get tablet strength, needed for combination products
    products <- rlang::quos(
        stringr::str_detect(med.product, "7.5-200mg") ~ 7.5,
        stringr::str_detect(med.product, "10/325|325( )?(mg)?-10") ~ 10,
        stringr::str_detect(med.product, "325( )?(mg)?-7.5") ~ 7.5,
        stringr::str_detect(med.product, "300( mg)?-15") ~ 15,
        stringr::str_detect(med.product, "#3|300( mg)?-30") ~ 30,
        stringr::str_detect(med.product, "#4|300( mg)?-60|325( mg)?-60") ~ 60,
        stringr::str_detect(med.product, "50 mg") ~ 50,
        stringr::str_detect(med.product, "100 mg") ~ 100,
        stringr::str_detect(med.product, "325 mg -7.5 mg/15ml") ~ 0.5,
        stringr::str_detect(med.product, "5 ml 120-12 mg/5 ml") ~ 2.4,
        stringr::str_detect(med.product, "10 mg/ml") ~ 10,
        stringr::str_detect(med.product, "2.5 mg/2.5 ml") ~ 1,
        stringr::str_detect(med.product, "16.2-30") ~ 30,
        stringr::str_detect(med.product, "12 microgram/hr") ~ 12,
        stringr::str_detect(med.product, "25 microgram/hr") ~ 25,
        stringr::str_detect(med.product, "50 microgram/hr") ~ 50,
        stringr::str_detect(med.product, "75 microgram/hr") ~ 75,
        stringr::str_detect(med.product, "100 microgram/hr") ~ 100,
        stringr::str_detect(med.product, "8 mg - 2 mg") ~ 8,
        stringr::str_detect(med.product, "5/325|325( )?(mg)?-5|5( )?mg") ~ 5,
        med.product == "acetaminophen-hydrocodone" ~ 5,
        med.product == "acetaminophen-codeine" ~ 30
    )

    # determine frequency for patches
    freq <- rlang::quos(
        stringr::str_detect(frequency, "Q72|ONCE") & route.group == "TOP" ~ 3,
        stringr::str_detect(frequency, "Q48") & route.group == "TOP" ~ 2
    )

    # conversion to morphine equivalents
    convert <- rlang::quos(
        med == "buprenorphine" & route.group == "TOP" ~ 3.8,
        stringr::str_detect(med, "buprenorphine") & route.group == "PO" ~ 3.3,
        med == "butorphenol" ~ dose.mg * 5,
        stringr::str_detect(med, "codeine") & route.group == "PO" ~ dose.mg * 0.05,
        stringr::str_detect(med, "codeine") & route.group == "IV" ~ dose.mg * 0.1,
        med == "fentanyl" & route.group == "IV" ~ dose.mg * 0.1,
        med == "fentanyl" & route.group == "NASAL" ~ dose.mg * 0.16 * 0.3,
        med == "fentanyl" & route.group == "TOP" ~ dose.mg * dose.freq * 2.4 * 0.3,
        stringr::str_detect(med, "hydrocodone") ~ dose.mg * 0.3,
        med == "hydromorphone" & route.group == "PO" ~ dose.mg * 1.3,
        med == "hydromorphone" & route.group == "IV" ~ dose.mg * 6.7,
        med == "levorphanol" ~ dose.mg * 5,
        med == "mepridine" ~ dose.mg * 0.1,
        med == "methadone" & route.group == "PO" ~ dose.mg * 3 * 0.3,
        med == "morphine" & route.group == "PO" ~ dose.mg * 0.3,
        med == "nalbuphine" ~ dose.mg * 1,
        med == "opium" ~ dose.mg * 0.3,
        stringr::str_detect(med, "oxycodone") & route.group == "PO" ~ dose.mg * 0.5,
        med == "oxymorphone" & route.group == "PO" ~ dose.mg * 1,
        med == "oxymorphone" & route.group == "IV" ~ dose.mg * 10,
        med == "pentazocine" ~ dose.mg * 0.37 * 0.3,
        med == "tapentadol" ~ dose.mg * 0.1,
        med == "remifentanil" & med.dose.units == "mg" ~ dose.mg * 100, # dose recorded in mg; assume equiv with fentanyl
        med == "sufentanil" ~ dose.mg * 0.5, # 100mcg = 50mg
        med == "tramadol" ~ dose.mg * 0.08,
        TRUE ~ dose.mg
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
        route %in% routes_po ~ "PO",
        route %in% routes_iv ~ "IV",
        route %in% routes_top ~ "TOP",
        route == "NASAL" ~ "NASAL"
    )

    df %>%
        dplyr::mutate(
            !!"route.group" := dplyr::case_when(!!!routes),
            !!"tab.mg" := dplyr::case_when(!!!products),
            # calculate the mg given in each dose; needed when dose is charted
            # in number of tabs, etc.
            !!"dose.mg" := !!rlang::parse_expr(
                'dplyr::if_else(
                    med.dose.units %in% c("tab", "mL", "supp", "patch"),
                    tab.mg * med.dose,
                    med.dose
                )'
            ),
            !!"dose.freq" := dplyr::case_when(!!!freq),
            !!"mme.iv" := dplyr::case_when(!!!convert)
        )

}

