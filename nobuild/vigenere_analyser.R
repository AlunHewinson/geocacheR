## codebreaking analyses
library(magrittr)
library(tidyverse)

plain <- "The cache is found at north fifty one forty five point six nine five and west zero one fourteen point two seven six"
cipher <- vigenere(plain, key="", decrypt = FALSE)
cipher <- "XBDFRWLNPQRFGNDBLRPAMHVHOQHIEHAWOWGZBNNTAUE"

expectation_crib <- list(
  geocaching = c("zero", "nought", "one", "two", "three", "four", "five", "six",
                 "seven", "eight", "nine", "north", "south", "east", "west",
                 "geocache", "found", "congratulations", "found"),
  mini = c("and", "the")
)

analyse_vigenere <- function(x, expected_words, alphabet=standard_alphabet) {
  expl <- expected_words %>% sapply(nchar) %>% unique()
  stalph <- alphabet %>% unlist() %>% unique()
  xalph <- x %>% str_split("") %>% unlist()
  xalph <- xalph[xalph %in% stalph] %>% paste(collapse="")
  #return(xalph)
  xnc <- xalph %>%  nchar()
  snibbles <- sapply(expl, function(q) {
    if (q < xnc) {
      stubs <- str_sub(xalph, 1:(1+xnc-q), q:xnc)
      sapply(stubs, function(stub) {
        sapply(expected_words[nchar(expected_words)==q], function(ew_q) {
          vigenere(stub, ew_q, alphabet = alphabet)
        })
      })
    }
  })
  snibbles

  ## take each word in the expectation and place it at position 1 of the cipher
  ## text, and do a vigenere on that portion. Then move it to position two, etc.
  ## CIPHERTEXT
  ## needle
  ##  needle
  ##   needle
  ##    needle
  ##     needle
  ## repeat for each word in the expectation vector
  ## do SOME kind of analysis on the resultant snibbles
}

rest <- analyse_vigenere(cipher, expectation_crib$geocaching) %>%
  unlist() #%>% table()
rest %>% table() %>% sort(decreasing = TRUE) %>% names() %>% cat(sep=" ")
