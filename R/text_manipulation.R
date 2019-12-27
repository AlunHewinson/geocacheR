## text manipulation

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

  x %<>% str_split("") %>% unlist()
  match_lower <- match(x, alphabet$lw)
  match_upper <- match(x, alphabet$up)
  mod_lower <- 1 + (match_lower + n - 1) %% length(alphabet$lw)
  mod_upper <- 1 + (match_upper + n - 1) %% length(alphabet$up)

  y_lower <- alphabet$lw[mod_lower]
  y_upper <- alphabet$up[mod_upper]
  dplyr::coalesce(y_lower, y_upper, x) %>% paste(collapse="")
}
