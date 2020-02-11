library(jpeg)
library(RCurl)
library(magrittr)

if (0) {
  download.file(
    mode = "wb",
    destfile = "nobuild/ef7d83c64986f65d86ca83a3191377cc.png",
    url = paste0("https://s3.amazonaws.com/gs-geo-images/",
                 "2080e978-7059-453f-a005-eadbc1346ef6.png"
    ))

  maze <- readJPEG("nobuild/ef7d83c64986f65d86ca83a3191377cc.png")

  mazebw <- apply(maze, 1:2, sum)
  colnames(mazebw) <- 1:ncol(mazebw)
  rownames(mazebw) <- 1:nrow(mazebw)
  #mazebw %>% str()
  break1 <- mazebw %>%
    apply(1, sum) %>%
    `<`(2300) %>%
    which()
  break2 <- mazebw %>%
    apply(2, sum) %>%
    `<`(3000) %>%
    which()
}

split_matrix <- function(m, v1, v2) {
  v11 <- c(1, v1)
  v12 <- c(v1, dim(m)[2])
  v21 <- c(1, v2)
  v22 <- c(v2, dim(m)[1])
  #browser()

  lapply (1:length(v11), function(i1) {
    lapply (1:length(v21), function(i2) {
      m[v21[i2]:v22[i2], v11[i1]:v12[i1], drop=FALSE]
    })
  })
}

count_borders <- function(m, threshold=1) {
  tb <- m %>% apply(1, mean)
  lr <- m %>% apply(2, mean)
  t <- tb[1]
  b <- rev(tb)[1]
  l <- lr[1]
  r <- rev(lr)[1]
  sum(c(t, b, l, r) < threshold)
}

mazel <- split_matrix(mazebw, break2, break1)
#testlet <- test[[7]][[3]]
#testlet
#testlet %>% image()
#testlet %>% count_borders()

i <- 0

lapply(mazel, function(qx) {
  lapply(qx, function(qy) {
    borders <- qy %>% count_borders()
    #browser()
    rn <- qy %>% rownames() %>% as.numeric()
    cn <- qy %>% colnames() %>% as.numeric()
    if (borders >= 3) {
      mazebw[rn, cn] <- 0
      mazebw <<- mazebw
      i <<- i + 1
    }
  })
  cat(".")
  return(invisible(NULL))
})
#image(mazebw)
