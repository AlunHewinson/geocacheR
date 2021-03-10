#' Value and frequency of Scrabble letters
#' @export
Scrabble <- tibble::tibble(
  tile=c(LETTERS, "_"),
  score=c(1, 3, 3, 2, 1, 4, 2, 4, 1, 8, 5, 1, 3,
           1, 1, 3, 10, 1, 1, 1, 1, 4, 4, 8, 4, 10, 0) %>% as.integer(),
  frequency=c(9, 2, 2, 4, 12, 2, 3, 2, 9, 1, 1, 4, 2,
              6, 8, 2, 1, 6, 4, 6, 4, 2, 2, 1, 2, 1, 2) %>% as.integer()
)

#' Find the Scrabble value of words
#'
#' @param x A vector of character strings
#' @param language A character string for the linguistic Scrabble edition, conforming to ISO 639-1
#'
#' Current supported languages: en
#'
#' @return An integer vector
#'
#' @examples
#' Scrabble_score(c("kwyjibo", "jozxyqk"))
#'
#' @export
Scrabble_score <- function(x, language="en") {
  matches <- x %>% str_split("")
  matches %>% sapply(function(q) {
    Scrabble$score[q %>% toupper() %>% match(Scrabble$tile)] %>%
      sum(na.rm = TRUE)
  })
}

#' Find the value of words
#'
#' @param x A vector of character strings
#'
#' @return An integer vector
#'
#' @examples
#' word_score(c("infinite", "monkey", "cage"))
#'
#' @export
word_score <- function(x) {
  matches <- x %>% str_split("")
  matches %>% sapply(function(q) {
    (1:26)[q %>% toupper() %>% match(LETTERS)] %>%
      sum(na.rm = TRUE)
  })
}

#' A helper table for base64 conversion and lookup
#' @export
base64 <- tibble::tribble(
  ~decimal, ~binary, ~b64,
  0,	"000000", "A",
  1,	"000001", "B",
  2,	"000010", "C",
  3,	"000011", "D",
  4,	"000100", "E",
  5,	"000101", "F",
  6,	"000110", "G",
  7,	"000111", "H",
  8,	"001000", "I",
  9,	"001001", "J",
  10,	"001010", "K",
  11,	"001011", "L",
  12,	"001100", "M",
  13,	"001101", "N",
  14,	"001110", "O",
  15,	"001111", "P",
  16,	"010000", "Q",
  17,	"010001", "R",
  18,	"010010", "S",
  19,	"010011", "T",
  20,	"010100", "U",
  21,	"010101", "V",
  22,	"010110", "W",
  23,	"010111", "X",
  24,	"011000", "Y",
  25,	"011001", "Z",
  26,	"011010", "a",
  27,	"011011", "b",
  28,	"011100", "c",
  29,	"011101", "d",
  30,	"011110", "e",
  31,	"011111", "f",
  32,	"100000", "g",
  33,	"100001", "h",
  34,	"100010", "i",
  35,	"100011", "j",
  36,	"100100", "k",
  37,	"100101", "l",
  38,	"100110", "m",
  39,	"100111", "n",
  40,	"101000", "o",
  41,	"101001", "p",
  42,	"101010", "q",
  43,	"101011", "r",
  44,	"101100", "s",
  45,	"101101", "t",
  46,	"101110", "u",
  47,	"101111", "v",
  48, "110000", "w",
  49, "110001", "x",
  50, "110010", "y",
  51, "110011", "z",
  52, "110100", "0",
  53, "110101", "1",
  54, "110110", "2",
  55, "110111", "3",
  56, "111000", "4",
  57, "111001", "5",
  58, "111010", "6",
  59, "111011", "7",
  60, "111100", "8",
  61, "111101", "9",
  62, "111110", "+",
  63, "111111", "/")

#' Finds the digital root (tvaersum in Danish) of a number
#'
#' @aliases digital_root
#'
#' @param x A number
#' @param warnDecimal Logical. If TRUE, a warning will be issued if x contains a decimal
#'
#' @return An integer
#'
#' @examples
#' tvaersum(91256015)
#' tvaersum("(844) 448-1212)")
#'
#' @export
tvaersum <- function(x, warnDecimal=TRUE) {
  if (str_detect(x, "\\.") & warnDecimal) {
    warning(paste0("x contains a decimal point. Digits after a decimal point ",
                   "will be treated as ordinary digits."))
  }
  if (is.integer(x)) if (x < 10) return(x)
  x %>%
    as.character() %>%
    str_split("") %>%
    unlist() %>%
    `[`(. %in% 1:9) %>%
    as.integer() %>%
    sum() %>%
    tvaersum()
}

#' @export
digital_root <- tvaersum

#' Convert between GC codes and GC ids
#'
#' @param x either a valid GC code, or the id of the cache
#'
#' @return a list of conversions, each element being a list of length two.
#' The first element will be called \code{code} and will be the GC code.
#' The second element will be \code{id} and will be a character value of the equivalent id
#'
#' @examples
#' convert_gc("12345")
#' convert_gc(54321)
#' convert_gc("GC3NWRQ")
#'
#' @export
convert_gc <- function(x) {
  lapply(x, convert_gc_single)
}

convert_gc_single <- function(x) {
  if (is.numeric(x)) {
    if (x < 1) {
      warning("geocacheR::convert_gc() -- x cannot be a numeric less than 1")
      return(list(code=NA_character_, id=NA_character_))
    }
    if (x %% 1 != 0) {
      warning("geocacheR::convert_gc() -- if x is a numeric it must be a whole number")
      return(list(code=NA_character_, id=NA_character_))
    }
    x %<>% as.character()
  }
  if (!is.character(x)) {
    warning("geocacheR::convert_gc() -- x must be a character or numeric")
    return(list(code=NA_character_, id=NA_character_))
  }
  x %<>% toupper() %>%
    #str_replace_all("[^0-9QWERTYPADFGHJKZXCVBNM]", "")
    str_replace_all("[^0-9A-Z]", "")

  if (str_sub(x, 1, 2)=="GC") {
    ## GC code
    code <- x
    x %<>% str_replace("GC(0*)", "")
    if (nchar(x) <= 3) {
      ## 0-9A-F only
      if (str_detect(x, "[^0-9A-F]")) {
        warning("geocacheR::convert_gc() -- GC codes of ")
        return(list(code=NA_character_, id=NA_character_))
      }
      x %<>%
        str_split("") %>%
        unlist() %>%
        match(c(0:9, LETTERS[1:6])) %>%
        subtract(1)
      powers <- 16^(seq(length(x))-1) %>% rev()
      id <- (x * powers) %>% sum() %>%
        as.character()
    } else if (nchar(x) == 4) {
      ## 0-9A-Z minus ILOSU unless the first char is <= F
      x %<>%
        str_split("") %>%
        unlist()
      if (x[1] <= "F") {
        ## 0-9A-F only
        if (any(str_detect(x, "[^0-9A-F]"))) {
          warning("geocacheR::convert_gc() -- GC codes of ")
          return(list(code=NA_character_, id=NA_character_))
        }
        x %<>%
          match(c(0:9, LETTERS[1:6])) %>%
          subtract(1)
        powers <- 16^(seq(length(x))-1) %>% rev()
        id <- (x * powers) %>% sum() %>%
          as.character()
      } else {
        x %<>%
          match(c(0:9, LETTERS)[-c(19, 22, 25, 29, 31)]) %>%
          subtract(1)
        powers <- 31^(seq(length(x))-1) %>% rev()
        id <- (x * powers) %>% sum() %>%
          subtract(411120) %>%
          as.character()
      }
    } else {
      ## 0-9A-Z minus ILOSU
      x %<>%
        str_split("") %>%
        unlist() %>%
        match(c(0:9, LETTERS)[-c(19, 22, 25, 29, 31)]) %>%
        subtract(1)
      powers <- 31^(seq(length(x))-1) %>% rev()
      id <- (x * powers) %>% sum() %>%
        subtract(411120) %>%
        as.character()
    }
  } else if (str_detect(x, "[^0-9]")) {
    ## error, non-numbers in id detected
    warning("geocacheR::convert_gc() -- x must be a valid GC code or an id containing only numbers")
    return(list(code=NA_character_, id=NA_character_))
  } else {
    ## valid id
    id <- x
    bs <- 16
    dg <- c(0:9, LETTERS[1:6])
    if (as.numeric(x) > 65535) {
      x %<>% as.numeric() %>% add(411120)
      bs <- 31
      dg <- c(0:9, LETTERS)[-c(19, 22, 25, 29, 31)]
    } else {
      x %<>% as.numeric()
    }

    pows <- floor(log(x, base = bs)):0 + 1
    code <- c("GC", dg[1 + (x %% bs^pows %/% bs^(pows-1))]) %>%
      paste(collapse="") %T>% print() %>%
      str_replace("^GC0*", "GC")
  }
  return(list(code=code, id=id))
}
