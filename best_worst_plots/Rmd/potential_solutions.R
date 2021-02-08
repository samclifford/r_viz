################################################################
# Attempt at a worse plot (less easily interpretable)
################################################################

library(gapminder)
library(tidyverse)
data(gapminder)

library(countrycode) # to get information about subregions
library(ggthemes)
library(extrafont)
loadfonts()

subregion_plot <- filter(gapminder, year %in% c(1997, 2007)) %>%
    # iso3n code is a numeric code for country
    mutate(iso3n = countrycode(sourcevar = country,
                               origin = "country.name",
                               destination = "iso3n", origin_regex = T)) %>%
    # we now want to get the corresponding UN region and subregion names
    # iso3n=158 corresponds to Taiwan, which is not in the UN system but is in Eastern Asia
    mutate(
        continent = countrycode(iso3n, "iso3n", destination = "un.region.name"),
        region    = countrycode(iso3n, "iso3n", destination = "un.regionsub.name"),
        region    = ifelse(iso3n == 158, "Eastern Asia", region),
        continent = ifelse(iso3n == 158, "Asia", continent),
        # Northern America and Australia and New Zealand correspond to a political,
        # rather than geographical grouping called CANZUS
        # so we collapse these appropriately
        region    = fct_collapse(region,
                                 `CANZUS` = c("Northern America",
                                              "Australia and New Zealand"))) %>%
    arrange(continent, region) %>%
    mutate(region = fct_inorder(region)) %>%
    ggplot(data = .,
           aes(x = gdpPercap/1000, y = lifeExp)) + # plot GDP as thousands of USD
    geom_line(aes(group = country,
                  color = continent),
              # arrows indicate which way the change is occurring
              arrow = arrow(type = 'closed',
                            length = unit(0.1, "cm"))) +
    facet_wrap(~region, ncol = 4) +
    scale_x_log10(labels = function(x){sprintf("%g",x)}) + # remove trailing .0
    theme_bw() +
    theme(legend.position  = "bottom",
          legend.box       = "vertical",
          panel.grid.minor = element_blank(),
          text = element_text(family = "Roboto Condensed"),
          axis.title.x = element_text(hjust = 1)) +
    xlab("GDP per capita, adjusted for inflation (1000 USD)") +
    ylab("Life expectancy at birth (years)") +
    annotation_logticks(sides = "bt") +
    scale_colour_brewer(palette = "Dark2", name = "Continent") +
    ggtitle(label    = "National GDP and life expectancy at birth",
            subtitle = "Change from 1997-2007")

ggsave(filename = "subregion_plot.png", plot = subregion_plot, dpi = 300,
       width = 10, height = 7, units = "in")

################################################################
# Attempt at a worse plot (less easily interpretable)
################################################################

fix_cut_labels <- function(x){
    # custom function to convert numbers from exponential to regular numbers
    levs <- levels(x)
    
    new_levs <- 
        strsplit(levs, split = ",") %>%
        map(parse_number) %>%
        map_chr(~paste0("(", paste(.x, collapse = ", "), "]"))
    
    fct_recode(x, !!!set_names(levs, new_levs))
    
}


gap <- gapminder %>%
    group_by(continent, year) %>%
    summarise(GDP     = weighted.mean(gdpPercap,pop),
              lifeExp = weighted.mean(lifeExp, pop)) %>%
    ungroup %>%
    mutate(GDP_cut = cut(GDP, breaks = 8)) %>%
    mutate(GDP_cut = fix_cut_labels(GDP_cut)) 

green_plot <- gap %>%
    ggplot(data = ,
           aes(x = factor(year), y = lifeExp)) +
    geom_col(aes(fill = GDP_cut),
             color = "black") +
    theme_minimal() +
    theme(axis.text.x   = element_text(angle = 90),
          title         = element_text(family = "Copperplate"), 
          plot.subtitle = element_text(color  = "grey70"),
          strip.text    = element_text(family = "Copperplate",
                                       hjust  = 1, 
                                       color  = "grey70"),
          panel.grid    = element_blank()) +
    scale_fill_brewer(palette = "Greens", name = "Mean GDP ($)") +
    facet_wrap(~continent, ncol = 1) +
    theme(panel.grid.major.y = element_line(linetype  = 2, color = "grey70")) +
    labs(title    = "Life expectancy and GDP in each continent 1952-2007",
         subtitle = "Population-weighted means calculated for each year and continent",
         x        = "Time (Y)",
         y        = "Mean life expectancy (Y)")


ggsave(filename = "green_plot.png", plot = green_plot, dpi = 300,
       width = 7, height = 5, units = "in")


# Here we've made the following changes:
# 
# 1. Rotated the $x$ axis tick labels too far
# 2. Coloured bars by GDP, requiring us to mentally map the relationship between bar colour and bar height
# 3. Let `cut()` choose cut points for GDP that are meaningless (evenly spaced across range)
# 4. Unnecessary level of precision in GDP
# 5. Choice of Copperplate font doesn't aid readability
#     - moreover, the font families don't match well across the font
# 6. Put life expectancy's axis label as "Y", same as for time
#     - year isn't actually measured in years the same way life expectancy is
# 7. Many countries use dollars, which dollars do we mean?
# 8. No indication of variability within continent and year for GDP or life expectancy, both of which have been averaged (weighted by population)
# 9. Sequential colour scheme is good, but has too many divisions to be useful.
# 10. Annotations are too pale (subtitle, horizontal grid lines, and facet headings for each continent)
# 11. Bar plots *must* go to zero (and they do here) but because the values of life expectancy go from 40-80 we find it difficult to tease out trends in life expectancy