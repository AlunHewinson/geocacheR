## text manipulation

#' The standard alphabet for the locale, for use in Caesar-based encryption etc.
#' @export
standard_alphabet <- list(lw=letters, up=LETTERS)

#' Caesar-shift a string by a given number of letters.
#'
#' @param x A string.
#' @param n A number of letters to shift the string by.
#' @param alphabet A list containing lower and upper case alphabets
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

#' Caesar-shift a string over all possible number n
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
#' @examples
#' vigenere("MN vdopf wq brcep zwtcd.", "midway")
#' vigenere("My treasure is buried he... find it who may.", "La Bouche", decrypt = FALSE)
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
#'
#' This is a wrapper for \code{vigenere} where decrypt is set to FALSE
#'
#' @param x A string to encrypt or decrypt
#' @param key The encryption or decryption key
#' @param alphabet A list of letters in lower and upper case
#'
#' @seealso \code{\link{vigenere}}
#' @export
qqmiaiii <- function(x, key, alphabet=standard_alphabet) {
  vigenere(x=x, key=key, decrypt = FALSE, alphabet = alphabet)
}

#' Make a frequency table of the characters in a string
#'
#' @param x A character vector
#' @param case_sensitive Logical. Should lower and upper case letters be kept separate
#' @param alphabet A list containing lower and upper case alphabets
#' @param collapse A character value to be passed to paste() to collapse x into a single string.
#' If collapse=FALSE (default), no collapse is implemented
#'
#' @return A tibble of the frequencies, with one row for each input string
#'
#' @export
analyse_frequency <- function(x, case_sensitive=FALSE, alphabet=standard_alphabet, collapse=FALSE) {
  if(case_sensitive) {
    alphabet %<>% unlist() %>% unique()
  } else {
    x %<>% toupper()
    alphabet %<>% unlist() %>% toupper() %>% unique()
  }

  if (collapse != FALSE) x <- paste(x, collapse=collapse)

  tables <- x %>%
    str_split("") %>%
    lapply(function(q) {
      freq <- q %>% c(alphabet) %>% table()
      freq[names(freq) %in% alphabet] <- freq[names(freq) %in% alphabet] - 1
      freq %>% as.matrix() %>% t() %>% as.data.frame()
    })
  toRet <- tables %>% bind_rows() %>% mutate_all(tidyr::replace_na, 0)
  toRet$input_string <- x
  toRet
}

#' Create "hashes" of the letters in a character vector
#'
#' The "hash" here is not a true hash, since the output has a variable number of
#' characters, but is instead a string representation of the number of characters
#' in the input string in hexadecimal format.
#'
#' @param x A character vector to be hashed
#' @param case_sensitive NOT IN USE. This parameter will probably be removed
#' @param alphabet NOT IN USE. This parameter will probably be removed
#'
#' @return A character vector equal in length to the input x
#'
#'
alphash <- function(x, case_sensitive=FALSE, alphabet=standard_alphabet) {
  x %>%
    analyse_frequency() %>%
    select(!!alphabet) %>%
    apply(1, function(qq) qq %>% as.hexmode %>% paste(collapse="g"))
}
