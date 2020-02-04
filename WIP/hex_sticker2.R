library(ggplot2)
library(hexSticker)
library(png)
library(grid)
library(ggforce)
library(magrittr)

img <- readPNG("nobuild/sticker/trad_cache.png")
g <- rasterGrob(img, interpolate=TRUE)

move_towards <- function(start, target, perc) {
  MEAN <- start + (target - start) * perc
  rnorm(1, MEAN, max(abs(target-start)/7, 0.15))
}

tree <- ggplot() +
  coord_cartesian(xlim=c(0, 10), ylim=c(0, 11))

nbuds <- 85
labls <- rep("(...)", nbuds)
labls <- rep("()", nbuds)
#labls[75] <- "geocacheR"

set.seed(156)
d <- rnorm(nbuds, 2, sd=1.1)
a <- runif(nbuds, 0, 2*pi)

buds <- data.frame(x=5+d*cos(a),
                   y=7+d*sin(a))

for (i in 1:nrow(buds)) {
  branch <- data.frame(fromx=NA, fromy=NA, tox=5,
                       toy=c(seq(8, 0, length.out=8), 0, 0, 0),
                       perc=c(0.2, 0.2, rep(0.55, 6), 0.6, 0.8, 0.9))
  branch[1, 1:2] <- c(buds$x[i], buds$y[i])
  for (rw in 2:11) {
    branch[rw, 1] <- move_towards(branch[rw-1, 1], branch[rw-1, 3], branch[rw-1, 5])
    branch[rw, 2] <- move_towards(branch[rw-1, 2], branch[rw-1, 4], branch[rw-1, 5])
  }
  #branch
  tree <- tree +
    geom_path(aes(x=fromx, y=fromy), size=0.3, colour="#805030", alpha=0.2, data=branch)
  tree <- tree + geom_text(aes(x, y), label=labls[i], colour="#40cc40", alpha=0.7, size=6,
                           data=buds[i, ])
}
tree <- tree + annotation_custom(g, xmin=3.7, xmax=4.7, ymin=-0.2, ymax=0.8) +
  theme_void()
tree %>% print()

sticker(tree, package="geocacheR", p_size=20, s_x=1, s_y=.75,
        h_color = "#40cc40", p_color = "#40cc40", h_fill="#f4fff4",
        s_width=1.3, s_height=1.2,
        filename="nobuild/sticker/geocacheR.png")

