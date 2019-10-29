
library(mosaicData)
library(tidyverse)
library(magrittr)
library(lubridate)

data(Weather)

ggplot(data = Weather,
       aes(x = date, y = avg_temp)) +
    geom_line() +
    facet_grid(rows = vars(year), cols = vars(city), scales = "free_x")

Weather %<>% mutate(yday = yday(date))

Weather %<>% mutate(avg_temp_ = ifelse(year == 2016, -avg_temp, avg_temp))


ggplot(data = ungroup(Weather),
       aes(x = day, y = 5/9*(avg_temp - 32))) +
    geom_col(aes(fill = factor(city))) +
    facet_grid(year ~ month)

ggplot(data = Weather, aes(x = yday, y = avg_temp)) +
    geom_line(aes(color = factor(year))) +
    geom_ribbon(aes(ymin = low_temp, ymax = high_temp, 
                    fill = factor(year),
                    group = factor(year)),
                alpha = 0.5) +
    facet_grid(.~city)

mutate(Weather, month = floor_date(date, "month")) %>%
    group_by(city, month, year) %>%
    summarise_at(.vars = vars(contains("temp")), .funs = list(mean))  %>%
    ggplot(data = ., aes(x = month, y = avg_temp)) +
    geom_line() +
    geom_ribbon(aes(ymin = low_temp, ymax = high_temp),
                alpha = 0.25) + theme_bw() +
    xlab("Date") + ylab(expression(Temperature~(degree*F))) +
    ggtitle("Monthly average of daily mean, min and max temperatures") +
    facet_grid(city ~ year, scales = "free") +
    scale_x_date(date_labels = "%b")
