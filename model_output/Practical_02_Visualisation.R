##########################################################
# Visualising outputs                                    #
##########################################################

# Consider an SIR model. 
#
# A CSV file has been provided for each of the three populations in the model, 
# for each of 100 simulations:
#  S  - susceptible
#  I - first stage of infection
#  R  - deceased
#
# The model used to simulate the disease is
f_sir <- function(time, state, parameters) {
    
    with(as.list(c(state, parameters)), {
        
        infections  <- beta*S*I1
        deaths      <- gamma*I1
        
        dS <- -infections 
        dI <-  infections - deaths
        dR <-               deaths
        
        return(list(c(dS, dI, dR)))
    })
}


#############################
# 2. Multiple simulations
#############################

library(tidyverse)

# 2.1 Read in the 100 simulation data set, gather it to long format and make
# state into a factor variable

all_dat_100 <- read_csv("100_simulations_wide.csv")

all_dat_100_long <- gather(???)

# 2.2 Plot all 100 simulations from the SIR model 
# Hint: you will need to use the group aesthetic with your line and may choose
# to set the lines to be semi-transparent

ggplot(data = all_dat_100_long,
       aes(???)) +
    geom_???(???)

# 2.3 Use the group_by and summarise approach to calculate the median
# and 95% interval for each state at each time point

all_dat_100_long_grouped <- group_by(all_dat_100_long, ???) 

all_dat_100_long_summarised <- 
    summarise(all_dat_100_long_grouped,
              ???)


# 2.4 Use geom_ribbon, or, optionally, geom_line three times, to plot the median
# and 95% interval for the summarised values. geom_ribbon requires a ymin and 
# ymax and can be coloured and filled. You may choose to use any colours and/or
# fills you wish for the ribbon and the line. Ensure you label the axes
# appropriately

ggplot(data = all_dat_100_long_summarised,
       aes(???)) +
    geom_???


# 2.5 If you have time left, copy and paste the above code and investigate 
# alternative geometries, replacing the ribbon. You may also wish to investigate
# the options for changing colour schemes, transparency, theming, faceting, etc.

