#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export


pluralize_gift <- function( gift  ){
  gift <- gift %>%
    str_replace("oo", "ee") %>%
    str_replace("y$", "ies") %>%
    str_replace("ve$", "ves") %>%
    str_replace("t$", "ts") %>%
    str_replace("w$", "ws") %>%
    str_replace("m$", "ms") %>%
    str_replace("n$", "ns") %>%
    str_replace("d$", "ds") %>%
    str_replace("g$", "gs") %>%
    str_replace("r$", "rs")

  return(gift)
}
