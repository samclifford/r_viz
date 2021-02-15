library(gapminder)
library(tidyverse)

data(gapminder)
head(gapminder)

group_by(gapminder, continent, year) %>% 
    summarise(lifeExp   = weighted.mean(lifeExp, pop),
              gdpPercap = weighted.mean(gdpPercap, pop),
              pop       = sum(pop)) %>%
    ggplot(data=., aes(x = year, y = gdpPercap)) +
    geom_line(aes(color = continent))

library(WDI)
?WDI

# do some joins on different income bands to see how LMICs, etc. change over time
# do we want to focus on specific countries only?