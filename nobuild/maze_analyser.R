library(jpeg)
library(RCurl)
library(magrittr)

download.file(
  mode = "wb",
  destfile = "ef7d83c64986f65d86ca83a3191377cc.png",
  url = paste0("https://s3.amazonaws.com/gs-geo-images/",
               "2080e978-7059-453f-a005-eadbc1346ef6.png"
  ))

maze <- readJPEG("ef7d83c64986f65d86ca83a3191377cc.png")
maze %>% str()

break1 <- maze %>%
  apply(1, sum) %>%
  `<`(2300) %>%
  which()

break2 <- maze %>%
  apply(2, sum) %>%
  `<`(3000) %>%
  which()


