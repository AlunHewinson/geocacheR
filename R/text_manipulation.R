## text manipulation

#' rot a string by a given number of letters.
#'
#' @param x A string.
#' @param n A number of letters to rot the string by.
#' @param alphabet A list containing lower and upper case alphabets.
#'
#' @return A string
#'
#' @examples
#' rot("abc")
#' rot("abc", n=2)
#' rot("abc", n=5, list(lw=letters[1:7], up=LETTERS[1:7]))
#' @export
rot <- function(x, n=13, alphabet=list(lw=letters, up=LETTERS)) {
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
rot_all <- function(x, alphabet=list(lw=letters, up=LETTERS)) {
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
  sapply(seq_along(alphabet$lw), rot, x=x, alphabet=alphabet)
}
