#' Populate a vector into a matrix in a spiral patter
#'
#' @param x A vector to spiralise
#' @param r An integer; the number of rows
#' @param c An integer; the number of columns
#'
#' @return A matrix with r rows and c columns
#'
#' @examples
#' spiralise(1:48, 4, 12)
#'
#' @export
spiralise <- function(x, r, c) {

  m <- matrix(nrow = r, ncol = c)

  i <- 1
  j <- 1
  s <- 1
  m[i, j] = s
  repeat {

    ##  left to right
    idx <- tail(which(is.na(m[i, ])), 1)
    m[i, j:idx] <- x[s + (0:(idx-j))]
    s <- s + idx - j
    j <- idx
    if (all(!is.na(m))) break

    ##  top to bottom
    idx <- tail(which(is.na(m[, j])), 1)
    m[i:idx, j] <- x[s + (0:(idx-i))]
    s <- s + idx - i
    i <- idx
    if (all(!is.na(m))) break

    ## right to left
    idx <- head(which(is.na(m[i, ])), 1)
    m[i, j:idx] <- x[s + (0:(j-idx))]
    s <- s + j - idx
    j <- idx
    if (all(!is.na(m))) break

    ## bottom to top
    idx <- head(which(is.na(m[, j])), 1)
    m[i:idx, j] <- x[s + (0:(i-idx))]
    s <- s + i - idx
    i <- idx
    if (all(!is.na(m))) break
  }
  m
}

spiralise(letters, 13, 2)
