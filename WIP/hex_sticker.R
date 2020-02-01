library(ggplot2)
library(hexSticker)
library(png)
library(magrittr)
library(grid)
library(ggforce)

img <- readPNG("C:/Users/alunh/Dropbox/Blog/twitter/emojis/trad_cache.png")
g <- rasterGrob(img, interpolate=TRUE)

beziers <- data.frame(
  x = c(40, 45, 50, 52),
  y = c(50, 55, 52, 10)#,
  #type = rep(c('cubic', 'quadratic'), c(0, 4)),
  #point = c('end', 'control', 'control', 'end'),
  #colour = "#807060a0" #letters[1:3]
)
p <- ggplot() +
  geom_path(aes(x = x, y = y), size=2, colour="#a08060", alpha=0.2,
               data = beziers) +
  coord_cartesian(xlim=c(0, 100), ylim=c(0, 100))

  #p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
#p <- ggplot()
p <- p +
  theme_transparent() + theme_no_axes() +
  annotation_custom(g, xmin=45, xmax=55, ymin=0, ymax=10)
p %>% print()

#sticker(p, package="hexSticker", p_size=20, s_x=1, s_y=.75,
#        s_width=1.3, s_height=0.8,
#        filename="C:/Users/alunh/Dropbox/Blog/twitter/emojis/geocacheR.png")

