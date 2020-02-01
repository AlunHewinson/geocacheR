## text manipulation

#'@export
standard_alphabet <- list(lw=letters, up=LETTERS)

#' rot a string by a given number of letters.
#'
#' @param x A string.
#' @param n A number of letters to rot the string by.
#' @param alphabet A list containing lower and upper case alphabets.
#' @param showWarn boolean. Do you want to see warnings about alphabets?
#'
#' @return A string
#'
#' @examples
#' rot("abc")
#' rot("abc", n=2)
#' rot("abc", n=5, list(lw=letters[1:7], up=LETTERS[1:7]))
#' @export
rot <- function(x, n=13, alphabet=standard_alphabet, showWarn=TRUE) {
  ## is alphabet a list? If not, make it so
  if(is.list(alphabet)) {
    ## warn if upper and lower contain the same
    if (length(intersect(alphabet$lw, alphabet$up)) > 0) {
      if (showWarn) {
      warning("Upper and lower case alphabets contain the same characters.
              This could result in the conversion from upper to lower case
              letters with nested rot()s.")
      }
    }
  } else {
    if (showWarn) {
      message("monocase alphabet provided")
    }
    alphabet <- list(lw=alphabet, up=alphabet)
  }
  ## warn if alphabets are of different lengths
  if (length(alphabet$lw) != length(alphabet$up)) {
    if (showWarn) {
      warning("Upper and lower case alphabets are different lengths")
    }
  }

  x %<>% stringr::str_split("") %>% unlist()
  match_lower <- match(x, alphabet$lw)
  match_upper <- match(x, alphabet$up)
  mod_lower <- 1 + (match_lower + n - 1) %% length(alphabet$lw)
  mod_upper <- 1 + (match_upper + n - 1) %% length(alphabet$up)

  y_lower <- alphabet$lw[mod_lower]
  y_upper <- alphabet$up[mod_upper]
  dplyr::coalesce(y_lower, y_upper, x) %>% paste(collapse="")
}

#' rot a string over all possible n
#'
#' @param x A string.
#' @param alphabet A list containing lower and upper case alphabets.
#'
#' @return a vector of strings
#'
#' @examples
#' rot_all("abc")
#' rot_all("abc", list(lw=letters[1:7], up=LETTERS[1:7]))
#' @export
rot_all <- function(x, alphabet=standard_alphabet) {
  ## is alphabet a list? If not, make it so
  if(is.list(alphabet)) {
    ## warn if upper and lower contain the same
    if (length(intersect(alphabet$lw, alphabet$up)) > 0) {
      warning("Upper and lower case alphabets contain the same characters.
              This could result in the conversion from upper to lower case
              letters with nested rot()s.")
    }
    } else {
      message("monocase alphabet provided")
      alphabet <- list(lw=alphabet, up=alphabet)
    }
  ## warn if alphabets are of different lengths
  if (length(alphabet$lw) != length(alphabet$up)) {
    warning("Upper and lower case alphabets are different lengths")
  }
  sapply(seq_along(alphabet$lw), rot, x=x, alphabet=alphabet, showWarn=FALSE)
}

#' Encrypt or decrypt a string using a key
#'
#' @param x A string to encrypt or decrypt
#' @param key The encryption or decryption key
#' @param decrypt Are you decrypting an encrypted string?
#' @param alphabet A list of letters in lower and upper case
#'
#' @return A string
#'
#' @export
vigenere <- function(x, key, decrypt=TRUE, alphabet=standard_alphabet) {
  ## parse key for appearance in alphabets
  keyv <- key %>% str_split("") %>% unlist()
  keyl <- keyv %>% match(alphabet$lw)
  keyu <- keyv %>% match(alphabet$up)
  keyi <- coalesce(keyl, keyu)
  keyi <- keyi[!is.na(keyi)]
  if (length(keyi)==0) {
    warning("key doesn't contain any alphabetic characters")
    return(x)
  }

  ## split the text
  xv <- x %>% str_split("") %>% unlist()
  xl <- xv %>% match(alphabet$lw)
  xu <- xv %>% match(alphabet$up)
  xi <- coalesce(xl, xu)
  if (length(xi)==0) {
    warning("message doesn't contain any alphabetic characters")
    return(x)
  }
  xvalid <- xi %>% is.na() %>% magrittr::not() %>% as.numeric()
  xkeyn <- xvalid %>%
    cumsum() %>%
    magrittr::subtract(1) %>%
    magrittr::mod(length(keyi)) %>%
    magrittr::add(1)
  xkey <- (decrypt*-2+1)*(keyi[xkeyn] - 1)

  mapply(FUN=rot, x=xv, n=xkey,
         MoreArgs = list(alphabet=alphabet, showWarn=FALSE)) %>%
    paste(collapse="") %>%
    return()
}

#' Encrypt a string using the Vigenere cipher
#' @export
qqmiaiii <- function(x, key, alphabet=standard_alphabet) {
  vigenere(x=x, key=key, decrypt = FALSE, alphabet = alphabet)
}
