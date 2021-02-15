library(gapminder)
library(tidyverse)
data(gapminder)

ggplot(data = gapminder,
       aes(x = gdpPercap, y = lifeExp)) +
  geom_path(aes(group = country, color = continent)) +
  geom_point(aes(color = continent, size = pop)) +
  scale_color_brewer(palette = 'Dark2') + scale_x_log10() +
  annotation_logticks(sides = 'b') + 
  facet_wrap( ~ continent, scales = 'free', nrow = 1) +
  scale_size_area() +
  theme(legend.position = 'bottom',
        legend.box = 'vertical')