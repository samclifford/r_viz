gapminder %>%
    mutate(iso3c = countrycode::countrycode(country,
                                            origin = "country.name",
                                            destination = "iso3c"),
           region = countrycode::countrycode(iso3c, origin = "iso3c",
                                             destination = "region")) %>%
    arrange(continent, region) %>%
    mutate(region = fct_inorder(region)) %>%
    ggplot(data = ., aes(x=year, y=lifeExp)) +
    geom_line(aes(group = country,
                  color = continent,
                  size  = gdpPercap),
              lineend = "round",
              linejoin = "bevel") + 
    facet_wrap(~region, ncol = 3) +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.box = "vertical") +
    scale_size_continuous(range = c(0.25, 3),
                          name = "GDP per capita (USD, adjusted for inflation)") 


gapminder_to_plot <- gapminder %>%
    mutate(country = as.character(country),
           country = ifelse(country == "Korea, Dem. Rep.",
                             "North Korea",
                             country)) %>%
    mutate(iso3c = countrycode::countrycode(country,
                                            origin = "country.name",
                                            destination = "iso3c"),
           region = countrycode::countrycode(iso3c, origin = "iso3c",
                                             destination = "region")) %>%
    arrange(continent, region) %>%
    mutate(region = fct_inorder(region)) %>%
    mutate(short.name = countrycode::countrycode(country,
                                                 origin = "country.name",
                                                 destination = "iso.name.en"))

library(ggrepel)

gapminder_to_plot %>%
    ggplot(data = ., aes(x=gdpPercap/1000, y=lifeExp)) +
    geom_path(aes(color = continent,
                  group = country,
                  alpha = year)) +
    geom_point(aes(color = continent,
                   alpha = year)) + 
    facet_wrap(~region, ncol = 3) +
    scale_color_brewer(palette = "Dark2") +
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.box = "vertical") +
    scale_x_log10() +
    xlab("GDP per capita (1000 USD, adjusted for inflation)") +
    scale_size_continuous(range = c(1,3)) +
    geom_label_repel(data = group_by(gapminder_to_plot, country) %>%
                         filter(year == max(year)) %>%
                         group_by(region) %>%
                         filter(lifeExp == max(lifeExp)),
               aes(label = short.name), force = 0,
               size = 2, nudge_x = 2) +
    theme(panel.grid.minor.x = element_blank()) +
    annotation_logticks(sides = "bt")
