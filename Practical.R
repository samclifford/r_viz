##########################################################
# Processing outputs                                     #
##########################################################

# Consider an SIR model. Infected individuals pass the infection on to 
# Susceptibles with beta = 1.56756 and eventually die with gamma = 0.36508
#
# 100 simulations have been performed in order to understand the disease's
# prevalence and incidence in the population. A CSV file has been provided for
# each of the four populations in the model:
#  S  - susceptible
#  I - first stage of infection
#  R  - deceased
#
# The model used to simulate the disease is
f_sir <- function(time, state, parameters) {
    
    with(as.list(c(state, parameters)), {
        
        infections1 <- beta*S*I1
        deaths      <- gamma*I1
        
        dS <- -infections1
        dI <-  infections1 - deaths
        dR <-                deaths
        
        return(list(c(dS, dI, dR)))
    })
}


#############################
# A. Reading in data
#############################

# A.1 Read in the CSV files and convert them to a long format data frame
# Ensure you have made `State` into a factor variable
library(tidyverse)

all_dat <- read_csv("beta_1.56756_gamma_0.36508.csv")

all_dat_long <- ???
all_dat_long$State <- ???

#############################
# B. Plotting the simulation
#############################

# B.1 With your long data frame, make a plot that shows how the proportion of 
# the population in each state changes over time. You may choose any plotting
# geometry you consider appropriate from the documentation, and look at using
# aesthetics such as group, colour or fill, or faceting, to show variation
# across the various states. You might find the following link useful
#
# https://ggplot2.tidyverse.org/reference/index.html

ggplot(data = all_dat_long, aes(x = ???,
                                y = ???)) +
    geom_??? + 
    ???


#############################
# C. Multiple simulations
#############################

# C.1 Read in the 100 simulation data set, gather it to long format and make
# State into a factor variable

all_dat_100 <- read_csv("100_simulations_wide.csv")

all_dat_100_long <- ???
all_dat_100_long$State <- ???

# C.2 Plot all 100 simulations from the SIR model 
# Hint: you will need to use the group aesthetic with your line and may choose
# to set the lines to be semi-transparent

ggplot(data = all_dat_100_long,
           aes(x = ???, 
               y = ???)) +
    geom_??? +
    ???

# C.3 Use the group_by and summarise approach to calculate the median and 95%
# interval for the simulated SIR output

all_dat_100_long_grouped <- group_by(all_dat_100_long,
                                     ???) 

all_dat_100_long_summarised <- 
    summarise(all_dat_100_long_grouped,
              ???)


# C.4 Use geom_ribbon, or, optionally, geom_line three times, to plot the median
# and 95% interval for the summarised values. geom_ribbon requires a ymin and 
# ymax and can be coloured and filled 

ggplot(data = all_dat_100_long_summarised,
       aes(x = ???)) +
    geom_ribbon(aes(ymin = ???, ymax = ???)) + ???

# C.5 Explore using facet_wrap() by simulation to plot small multiples of all 
# 100 simulations. You may choose to use any geometry. Ensure that the simulations

ggplot(data = all_dat_100_long,
       aes(x = time)) +
    geom_??? +
    facet_wrap( ~ sim) +     # repeat for each simulation
    ???
    