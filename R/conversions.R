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
#' @export
word_score <- function(x) {
  matches <- x %>% str_split("")
  matches %>% sapply(function(q) {
    (1:26)[q %>% toupper() %>% match(LETTERS)] %>%
      sum(na.rm = TRUE)
  })
}

#' A helper table for base64 conversion and looup
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
