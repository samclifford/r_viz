
library(tidyverse)
library(mosaicData)
data(Gestation)


## ----------------------------------------------------------------------------------------
count(Gestation)


## ----------------------------------------------------------------------------------------
count(Gestation, race)


## ----------------------------------------------------------------------------------------
Gestation_n_race_ed <- count(Gestation, race, ed)


## ----------------------------------------------------------------------------------------
pivot_wider(Gestation_n_race_ed, 
            names_from  = race,
            values_from = n)


## ----------------------------------------------------------------------------------------
pivot_wider(Gestation_n_race_ed, 
            names_from  = race,
            values_from = n,
            values_fill = 0)


## ----------------------------------------------------------------------------------------
summarise(Gestation, mean(age, na.rm = T))


## ----------------------------------------------------------------------------------------
summarise(Gestation, `Mean age` = mean(age, na.rm = T))


## ----------------------------------------------------------------------------------------
summarise(Gestation,
          `Mean age` = mean(age, na.rm = T),
          `Mean wt`  = mean(wt,  na.rm = T)) 


## ----------------------------------------------------------------------------------------
Gestation <- mutate(Gestation,
                    race2 = factor(race), 
                    race2 = fct_explicit_na(f        = race2, 
                                            na_level = "unknown")) 


## ----------------------------------------------------------------------------------------
Gestation <- mutate(Gestation, 
                    race2 = str_to_title(race2),
                    race2 = fct_infreq(race2))

count(Gestation, race2)


## ----------------------------------------------------------------------------------------
Gestation <- mutate(Gestation,
                    race2 = fct_recode(race2, 
                                       "Hispanic"    = "Mex",
                                       "Multiracial" = "Mixed"))


## ----------------------------------------------------------------------------------------
Gestation_mean_wt_by_race_smoke <- 
    Gestation %>%
    # change case
    mutate(smoke = str_to_sentence(smoke)) %>%
    # replace NA
    mutate(smoke = fct_explicit_na(f = smoke, "Smoking unknown")) %>%
    # change levels to be related to how recently mother smoked
    mutate(smoke = fct_relevel(smoke,
                               "Never",
                               "Once did, not now", 
                               "Until current pregnancy",
                               "Now",
                               "Smoking unknown")) %>%
    # group by human-friendly race variable and smoking status
    group_by(race2, smoke) %>%
    # calculate mean birth weight
    summarise(Mean = mean(wt, na.rm = T)) %>%
    # make wider so that race is in columns
    pivot_wider(names_from = "race2", values_from = "Mean") %>%
    # rename to be a more human friendly column heading
    rename("Smoking status" = smoke) 

Gestation_mean_wt_by_race_smoke


## ----eval=FALSE--------------------------------------------------------------------------
## write_csv(x    = Gestation_mean_wt_by_race_smoke,
##           path = "Gestation_mean_wt_by_race_smoke.csv")


## ----------------------------------------------------------------------------------------
options(knitr.kable.NA = '-')

knitr::kable(Gestation_mean_wt_by_race_smoke,
             digits = 1,
             caption = "Mean birth weight (lbs.) by race and smoking status.")

