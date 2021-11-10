# concat_encounters.R

#' Make concatenated list of encounters
#'
#' \code{concat_encounters} takes a vector of encounters and produces a
#' concatenated list for use in EDW
#'
#' This function takes a vector of encounters and concatenates them together,
#' separated by a semi-colon. The list is then split into the desired group
#' size. The resulting string can be used in an EDW query prompt.
#'
#' @param encounters A character vector with the encounters
#' @param num.split An optional numeric indicating the number to split on,
#'   default is 900
#'
#' @return A list
#'
#' @export
concat_encounters <- function(encounters, num.split = 900) {
    x <- unique(encounters)

    split(x, ceiling(seq_along(x) / num.split)) %>%
        purrr::map(stringr::str_c, collapse = ";")
}
