library(ggplot2)

library(galamm)
library(hexSticker)

dat <- subset(cognition, domain == 3 & item == "31")

p <- ggplot(dat, aes(x = x, y = y, group = id)) +
  geom_point(size = .1) +
  geom_line(linewidth = .1) +
  theme_void() +
  theme_transparent()

sticker(p,
        package="galamm", p_size=20, s_x=1, s_y=1.1, s_width=1.7,
        s_height = 1.3, p_y = .5,
        filename="inst/figures/galamm.png")
