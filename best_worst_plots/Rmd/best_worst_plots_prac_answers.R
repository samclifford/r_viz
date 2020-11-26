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

# Activity 2b

# **Answer:** While the graph is already quite bad, we can make it worse by removing the focus on the relationship between GDP and LE, shifting one of them out of the $x$ and $y$ variables.

p_bad <- ggplot(data = gapminder,
                aes(x = year - min(year), y = gdpPercap*pop)) +
    geom_point(aes(fill = lifeExp, shape = continent,
                   group = continent),
               position = position_dodge(width = 2.5)) +
    #facet_wrap(~country, scales = "free_y") +
    scale_y_log10() +
    scale_shape_manual(values = 21:25) +
    theme_dark() +
    theme(legend.position = "left") +
    scale_fill_gradientn(colours = rainbow(7), name = "Years") +
    ylab("GDP") +
    xlab("Time") +
    geom_smooth(aes(color = continent), se=FALSE)

p_bad


# Here we've made the following changes:
# 
# 1. Put time since first observation on $x$ axis without units
# 2. Put GDP on the $y$ axis, an interesting choice given we want to look chiefly at how life expectancy changes over time
# 3. Put coloured points and coloured lines on the graph, but the colours aren't related
# 4. Continent is mapped to both shape and line colour
# 5. Put life expectancy's legend label as "Years"
# 6. Whitespace is added on the left of the plot around the legend, a waste of space
# 7. No context for what GDP is (it's total national GDP, equal to per capita GDP multiplied by total population), and USD `1e13` is hard to contextualise (it should be increasing in steps of $10^3$, e.g. a million, billion, trillion for the major breaks)
# 8. The position changes in continent help make it a little clearer that there are multiple continents within a year, but it ends up looking like measurements were taken a year earlier or later than they were.
# 9. The dark colour scheme is too similar in intensity to some of the blue and purple points, obscuring them from our vision
# 10. Chosen a rainbow colour scheme, which is notorious for having poor properties that visually focus on yellows, are not colourblind friendly and do not convert to greyscale easily on account of multiple regions having the same intensity.

# or alternatively

ggplot(data = gapminder,
       aes(x = continent, y = gdpPercap)) +
    geom_point(aes(color = lifeExp),
               alpha = 0.25,
               position = position_jitter(width = 0.15, height = 0)) +
    scale_color_gradientn(colours = rainbow(7),
                          name = "Life Exp.",
                          limits = c(0, NA)) +
    theme_minimal() +
    facet_wrap(~year) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "left") +
    scale_y_continuous(labels = scales::dollar_format(accuracy = 0.01)) +
    xlab("Continent") +
    ylab("GDP")

# 1. GDP has no explanation and there's no need to have the cents listed in the dollar sign prices.
# 2. There are probably too many facets to make comparisons from year to year easy and we can't track individual countries across time
# 3. Linear scale starting at 0 obscures the smaller values, we can only see that there are some very large ones. This means there's a lot of extra white space in this plot.
# 4. The colour scale for life expectancy is difficult to interpret, as the points are green to purple via blue. Rainbow colour schemes are very rarely the correct choice.
# 5. The colour scale for life expectancy starts at 0, which is unnecessary as we are comparing colour rather than height above an axis. No countries have such low life expectancy - the lowest observed is 23.6.