# Activity 2a
# **Answer:** There are many ways to do this, but a scatter plot which is static in time and shows the variability across space can help us avoid showing too much. Essentially we run out of things to reasonably change to show all dimensions of the data.

# 1. Reduce number of variables shown
# 2. Remove faceting, relying on colour to show difference
# 3. Clarity around units of variables
# 4. Human friendly axis labels

p <- ggplot(data = filter(gapminder, year == 2007),
            aes(x = gdpPercap/1000, y = lifeExp)) +
    geom_point(aes(group  = country,#
                   size   = pop*1e-6,
                   color  = continent),
               alpha = 0.75) +
    scale_color_brewer(palette = "Dark2", name = "Continent") +
    scale_x_log10() +
    scale_size_area(name = "Population (millions)") +
    theme_bw() + theme(legend.position = "bottom", legend.box = "vertical") +
    xlab("GDP per capita, adjusted for inflation (1000 USD)") +
    ylab("Life expectancy at birth (years)") +
    theme(panel.grid.minor.x = element_blank()) +
    annotation_logticks(sides = "bt")

p

# Another solution is to choose two dates and show the change over time

# 1. Reduce number of years shown
# 2. Add extra detail about subregions from another package (sneaky, I know!)
#     - additionally, we have combined Northern America (USA, Canada) and Australia and New Zealand as they each only contain two countries and have similar GDP and life expectancy. Conveniently, they are part of a group of nations with a name, CANZUS
#     - we have to make some exceptions around the People's Republic of China, Taiwan
# 3. Facet on subregions but color by continent for quick visual clues
# 4. Arrows to show direction of increasing time
# 5. Title and subtitle on plot
# 6. Changes to axis to add meaning but reduce clutter

filter(gapminder, year %in% c(1997, 2007)) %>%
    mutate(
        continent = countrycode(iso3n, "iso3n", destination = "un.region.name"),
        region    = ifelse(iso3n == 158, "Eastern Asia", region),
        continent = ifelse(iso3n == 158, "Asia", continent),
        region    = fct_collapse(region,
                                 `CANZUS` = c("Northern America",
                                              "Australia and New Zealand"))) %>%
    arrange(continent, region) %>%
    mutate(region = fct_inorder(region)) %>%
    ggplot(data = .,
           aes(x = gdpPercap/1000, y = lifeExp)) +
    geom_line(aes(group = country,
                  color = continent),
              arrow = arrow(type = 'closed',
                            length = unit(0.1, "cm"))) +
    facet_wrap(~region, ncol = 4) +
    scale_x_log10() +
    theme_bw() + theme(legend.position  = "bottom",
                       legend.box       = "vertical",
                       panel.grid.minor = element_blank()) +
    xlab("GDP per capita, adjusted for inflation (1000 USD)") +
    ylab("Life expectancy at birth (years)") +
    annotation_logticks(sides = "bt") +
    scale_colour_brewer(palette = "Dark2", name = "Continent") +
    ggtitle(label    = "National GDP and life expectancy at birth",
            subtitle = "Change from 1997-2007")
