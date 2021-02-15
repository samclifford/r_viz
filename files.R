
library(tidyverse)
library(magrittr)
library(broom)

# want to show filter, arrange, group_by, summarise, pivoting, select
# if you feel comfortable using the pipe, feel free to do so

library(mosaicData)
data(Gestation)

# how many observations in the data frame?
count(Gestation)

# how many observations for each race?
count(Gestation, race)

# how many for each race and educational attainment?
count(Gestation, race, ed)

# that's a bit messy, so let's widen it into a table
pivot_wider(count(Gestation, race, ed),
            names_from = race,
            values_from = n)

# or for pipe users
Gestation %>% 
    count(race, ed) %>%
    pivot_wider(names_from = race,
                values_from = n)

# those NA values are really 0s because there are zero cases recorded
pivot_wider(count(Gestation, race, ed),
            names_from = race,
            values_from = n,
            values_fill = 0)

# what's the mean age in the data set?
summarise(Gestation, age = mean(age, na.rm = T))

# what's the mean age for each race group?
summarise(group_by(Gestation, race),
          mean = mean(age, na.rm = T))

# what's the mean and SD of age for each race group?
summarise(group_by(Gestation, race),
          mean = mean(age, na.rm = T),
          sd   = sd(age, na.rm = T)) 

# or, alternatively
summarise_at(group_by(Gestation, race),
             .vars = vars(age),
             .funs = list(Mean = mean, SD = sd), 
             na.rm = T)

# let's reorder in order of frequency
Gestation <- mutate(Gestation,
                    race2 = factor(race), 
                    race2 = fct_explicit_na(f        = race2, 
                                            na_level = "unknown")) 

count(Gestation, race2)

# to make even more friendly, copy and paste the following
Gestation <- mutate(Gestation, 
                    race2 = stringr::str_to_title(race2),
                    race2 = forcats::fct_infreq(race2),
                    race2 = forcats::fct_relevel(race2, "Unknown", after = Inf))

# we would probably recognise Mexican as Hispanic these days in US data sources

Gestation <- mutate(Gestation,
                    race2 = forcats::fct_recode(race2, "Hispanic" = "Mex"))

count(Gestation, race2)

Gestation_age_race_unknown <- lm(data = Gestation, age ~ race2)
tidy(Gestation_age_race_unknown)
glance(Gestation_age_race_unknown)

Gestation_age_race <- lm(data = Gestation, age ~ race)
tidy(Gestation_age_race)
glance(Gestation_age_race)

# our conclusion changes based on whether or not we include the 'unknown' group

# an alternative way to fit this would be to filter out all `Unknown` so that we still have human-friendly labels

Gestation %>%
    filter(race2 != "Unknown") %>%
    lm(data = ., formula = age ~ race) %>%
    glance


## we can use summary statistics that require more than one variable so long as they return a scalar, e.g. the sample correlation

Gestation %>%
    group_by(race2) %>%
    summarise(`r(wt, age)` = cor(wt, age, use = "pairwise.complete.obs"))

## What if we want to look at more than one correlation

Gestation %>%
    group_by(race2) %>%
    summarise(r = cor(wt, age, gestation, use = "pairwise.complete.obs")) # will fail

## we need to get a 

Gestation %>%
    select(wt, age, gestation) %>%
    cor(use = "pairwise") %>%
    as.data.frame %>%
    tibble::rownames_to_column(var = "row") %>%
    pivot_longer(-row, names_to = "col")

# fancy version!!
Gestation %>%
    select(race2, wt, age, gestation) %>%
    nest(data = -race2) %>%
    mutate(C = map(data, cor, use = "pairwise")) %>%
    mutate(C_ = map(C, upper.tri)) %>%
    mutate(C_long = map(C, function(x){data.frame(x) %>%
            tibble::rownames_to_column("row") %>%
            pivot_longer(-row, "col")})) %>%
    unnest(C_long) %>%
    select(-data, -C) %>% 
    filter(!(row == col))

# do a model for birth weight as a function of smoking

Gestation %<>%
    filter(!is.na(race)) %>%
    replace_na(list(number = "non-zero")) %>%
    mutate(number = ifelse(number == "never", "0 per day", number)) %>%
    mutate(number_ = stringr::str_extract(number, pattern = "^[0-9]{1,2}"),
           number_ = parse_number(number_)) %>%
    mutate(number = fct_reorder(number, number_, min)) 

Gestation %>%
    glm(data = ., wt ~ number, family = Gamma) %>%
    tidy(conf.int = T) %>%
    mutate(term = sub(pattern = "^number", replacement = "", x = term)) %>%
    mutate(term = sub("(Intercept)", "Never", term, fixed = T)) 


Gestation %>%
    lm(data = ., wt ~ number) %>%
    {bind_cols(distinct(Gestation, number), 
               predict(object = .,
                       newdata = distinct(Gestation, number),
                       interval = "conf") %>% data.frame)} %>%
    ggplot(data = ., aes(x = number, y = fit)) +
    geom_pointrange(aes(ymin = lwr,
                        ymax = upr))




### 

Gestation %>%
    mutate(smoke = str_to_sentence(smoke)) %>%
    mutate(smoke = fct_explicit_na(f = smoke, "Smoking unknown")) %>%
    mutate(smoke = factor(smoke,
                          levels = c("Never", "Once did, not now", 
                                     "Until current pregnancy",
                                     "Now",
                                     "Smoking unknown"))) %>%
    group_by(race2, `Smoking status` = smoke) %>%
    summarise(Mean = mean(wt, na.rm = T)) %>%
    pivot_wider(names_from = "race2", values_from = "Mean")
