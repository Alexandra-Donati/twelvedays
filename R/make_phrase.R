#' Puts the various parts of speech together into a full phrase.
#'
#' @param num An integer
#' @param num_word A string corresponding to the integer
#' @param item A string
#' @param verb A string
#' @param adjective A string
#' @param location A string
#'
#' @return A string containing the words in grammatical order.
#'
#' @import stringr
#' @import glue
#' @import dplyr
#' @import purrr
#'
#' @export




file( "pluralize_gift.R" )

make_phrase <- function(num, num_word, item, verb, adjective, location){

  item <- pluralize_gift( item )
  verb <- str_replace_na(verb, "")
  adjective <- str_replace_na(adjective, "")
  location <- str_replace_na(location, "")
  num_word <- num_word %>%
    str_replace("first", "A")%>%
    str_replace("second", "Two")%>%
    str_replace("third", "Three") %>%
    str_replace("fourth", "Four") %>%
    str_replace("fifth", "Five") %>%
    str_replace("sixth", "Six") %>%
    str_replace("seventh", "Seven") %>%
    str_replace("eighth", "Eight") %>%
    str_replace("ninth", "Nine") %>%
    str_replace("tenth", "Ten") %>%
    str_replace("eleventh", "Eleven") %>%
    str_replace("twelfth", "Twelve")


  glue::glue("{num_word} {adjective} {item} {verb} {location}")
}

xmas$Full.Phrase <- make_phrase( xmas$Day,xmas$Day.in.Words, xmas$Gift.Item,
                   xmas$Verb, xmas$Adjective, xmas$Location )



