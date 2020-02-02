#' Parse Coordinates into Numeric Format
#'
#' parseCoordinates takes a variety of string inputs for coordinates in the
#' following formats:
#' - N00 00.000 W000 00.000
#' - N00 00 00 W000 00 00
#' - N00.0000 W00.0000
#' and converts them into a numeric vector of length 2
#'
#' @param x A string for the coordinates to be converted
#'
#' @return A numeric vector holding the n(orth) and e(ast) coordinates
#'
#' @examples
#' parseCoordinates("N55 55.555 W003 14.159")
#' parseCoordinates("N 55 55.555   E003 14.159")
#' parseCoordinates("N55.92592 W3.23598")
#' @export
parseCoordinates <- function(x) {
  if (missing(x)) {
    warning("x is missing. Returning origin [0, 0]")
    return(c(north=0, east=0))
  }
  if (length(x)==0) {
    warning("x has no elements. Returning origin [0, 0]")
    return(c(north=0, east=0))
  }
  if (length(x) > 2) {
    warning("x has too many elements to understand. Returning origin [0, 0]")
    return(c(north=0, east=0))
  }
  if (length(x)==2) {
    x %<>% paste(collapse=" ")
  }

  ## regex patterns
  if (1) {
    DMm <- paste0(
      "([\\s]*)",        ## 1 leading whitespace
      "([NS-]{0,1})",    ## 2 N, S or minus sign
      "([\\s]*)",        ## 3 whitespace
      "([0-9]{1,2})",    ## 4 north degrees
      "([\\s\ub0]*)",    ## 5 whitespace and or degree symbol
      "([0-9]{1,2})",    ## 6 north minutes
      "(\\.)",           ## 7 decimal point
      "([0-9]{3})",      ## 8 north decimal minutes
      "([\\s']{0,1})",   ## 9 whitespace or minutes symbol
      "([\\s,;\\|/]+)",  ## 10 separator
      "([EW-]{0,1})",    ## 11 E, W or minus sign
      "([\\s]*)",        ## 12 whitespace
      "([0-9]{1,3})",    ## 13 east degrees
      "([\\s\ub0]*)",    ## 14 whitespace and or degree symbol
      "([0-9]{1,2})",    ## 15 east minutes
      "(\\.)",           ## 16 decimal point
      "([0-9]{3})",      ## 17 east decimal minutes
      "([\\s']{0,1})",   ## 18 whitespace or minutes symbol
      "([\\s]*)$"        ## 19 trailing whitespace
    )
    Dd <- paste0(
      "([\\s]*)",                    ## 1  leading whitespace
      "([NS-]{0,1})",                ## 2  N, S or minus sign
      "([\\s]*)",                    ## 3  whitespace
      "([0-9]{1,2})",                ## 4  north degrees
      "(([\\.]{0,1})([0-9]+){0,1})", ## 5  decimal point plus at least one digit
      "([\\s\ub0]{0,1})",            ## 6  whitespace or degrees symbol
      "([\\s,;\\|/]+)",              ## 7  separator
      "([EW-]{0,1})",                ## 8  E, W or minus sign
      "([\\s]*)",                    ## 9  whitespace
      "([0-9]{1,3})",                ## 10 north degrees
      "(([\\.]{0,1})([0-9]+){0,1})", ## 11 decimal point plus at least one digit
      "([\\s\ub0]{0,1})",            ## 12 whitespace or degrees symbol
      "([\\s]*)$"                    ## 13 trailing whitespace
    )
    DMSs <- paste0(
      "([\\s]*)",                    ## 1  leading whitespace
      "([NS-]{0,1})",                ## 2  N, S or minus sign
      "([\\s]*)",                    ## 3  whitespace
      "([0-9]{1,2})",                ## 4  north degrees
      "([\\s\ub0]+)",                ## 5  whitespace and or degree symbol
      "([0-9]{1,2})",                ## 6  north minutes
      "([\\s']+)",                   ## 7  whitespace and or minute symbol
      "([0-9]{1,2})",                ## 8  north decimal seconds
      "(([\\.]{0,1})([0-9]+){0,1})", ## 9  decimal point plus at least one digit
      "([\\s\"]*)",                  ## 10 whitespace and or second symbol
      "([\\s,;\\|/]+)",              ## 11 separator
      "([EW-]{0,1})",                ## 12 E, W or minus sign
      "([\\s]*)",                    ## 13 whitespace
      "([0-9]{1,3})",                ## 14 east degrees
      "([\\s\ub0]+)",                ## 15 whitespace and or degree symbol
      "([0-9]{1,2})",                ## 16 east minutes
      "([\\s']+)",                   ## 17 whitespace and or degree symbol
      "([0-9]{1,2})",                ## 18 east decimal seconds
      "(([\\.]{0,1})([0-9]+){0,1})", ## 19 decimal point plus at least one digit
      "([\\s\"]*)",                  ## 20 whitespace and or second symbol
      "([\\s]*)$"                    ## 21 trailing whitespace
    )
  }
  if (str_detect(x, DMm)) {
    #print("DMm")
    mtch <- stringr::str_match(x, pattern=DMm)
    ch <- mtch[c(3, 5, 7, 9, 12, 14, 16, 18)] ## cardinal points, degs, mins
    nm <- suppressWarnings(as.numeric(ch))

    return(c(
      n = ((ch[1] %in% c("N", "")) * 2 - 1) * (nm[2] + nm[3]/60 + nm[4]/6e4),
      e = ((ch[5] %in% c("E", "")) * 2 - 1) * (nm[6] + nm[7]/60 + nm[8]/6e4)
    ))
  }

  if (str_detect(x, Dd)) {
    #print("Dd")
    mtch <- stringr::str_match(x, pattern=Dd)
    ch <- mtch[c(2, 4, 5, 10, 12, 13)+1] ## cardinal points and degrees
    nm <- suppressWarnings(as.numeric(ch))

    return(c(
      n = ((ch[1] %in% c("N", "")) * 2 - 1) * sum(nm[2], nm[3], na.rm=TRUE),
      e = ((ch[4] %in% c("E", "")) * 2 - 1) * sum(nm[5], nm[6], na.rm=TRUE)
    ))
  }

  if (str_detect(x, DMSs)) {
    #print("DMSs")
    mtch <- stringr::str_match(x, pattern=DMSs)
    ch <- mtch[c(2, 4, 6, 8, 9, 14, 16, 18, 20, 21)+1] ## cardinal points and degrees
    nm <- suppressWarnings(as.numeric(ch))

    return(c(
      n = ((ch[1] %in% c("N", "")) * 2 - 1) * sum(nm[2], nm[3]/60, nm[4]/3600, nm[5]/3600, na.rm=TRUE),
      e = ((ch[6] %in% c("E", "")) * 2 - 1) * sum(nm[7], nm[8]/60, nm[9]/3600, nm[10]/3600, na.rm=TRUE)
    ))
  }

  warning("I do not understand the format of x. Returning origin [0, 0]")
  return(c(north=0, east=0))
}

#' Express Decimal Coordinates in Other (text) Formats
#'
#' Designed to convert into Geocaching-style style coordinates, but future styles may be accomodated.
#'
#' @param x A numeric vector of length 2
#' @param style placeholder for future development if requirements emerge
#'
#' @return A character of length 1 with an alternative expression of the coordinates
#'
#' @examples
#' expressCoordinates(c(55.9327, -3.25103))
#'
#' @export
expressCoordinates <- function(x, style="GC") {
  if (!is.numeric(x)) {
    warning("The input to geocacheR::expressCoordinates(), ",
            "was not numeric; attempting to parse...")
    x <- parseCoordinates(x)
  } else if(!length(x)==2) {
    stop("The numeric input to geocacheR::expressCoordinates() ",
         "was not of length 2")
  }

  n <- x[1]
  ns <- sign(n)
  nc <- ifelse(ns < 0, "S", "N")
  nd <- abs(n) %/% 1
  nm <- abs(n) %% 1 * 60
  e <- x[2]
  es <- sign(e)
  ec <- ifelse(es < 0, "W", "E")
  ed <- abs(e) %/% 1
  em <- abs(e) %% 1 * 60

  paste0(
    nc, sprintf("%02d", nd), " ",
    sprintf("%006.3f", nm), " ",
    ec, sprintf("%03d", ed), " ",
    sprintf("%006.3f", em)
  )
}

#' What 3 Words wrapper
#'
#' @param x A vector, or list, of words. Strings with dots in them will be split.
#' After splitting, there must be a multiple of three words.
#' Either a vector of words, for a single latitude/longitude pair, or
#' a list of vectors for vectorised operations. This wrapper also accepts a
#' single string of three words separated by full stops.
#'
#' @return a numeric vector of length 2, consisting of lat(itude) and lon(gitude)
#'
#' @examples
#' \dontrun{
#' w3w("president.always.lying")
#' w3w("unseen.academicals.football") ## returns NAs
#' w3w(list("special.tools.required", "cliffs.falling.rocks",
#'          "available.during.winter", "ultraviolet.light.required"))
#' w3w(c("protests", "memo", "consoles"))
#' }
#' @export
w3w <- function(x) {
  x %<>%
    stringr::str_split("\\.") %>%
    unlist()
  if (length(x) %% 3 != 0) stop("Error: geocacheR::w3w  Unknown input format")
  x %<>%
    matrix(ncol=3, byrow=TRUE) %>%
    as.data.frame()
  to_return <- apply(x, 1, function(q) {
    tryCatch(expr = {
      threewords::from_words(Sys.getenv("W3WAPIKey"), words=q)$position
    }, error = function(e) {
      c(latitude=NA, longitude=NA)
    })
  }) %>%
    t()
  colnames(to_return) <- c("lat", "lon")
  to_return
}
