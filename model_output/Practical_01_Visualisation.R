##########################################################
# Visualising outputs                                    #
##########################################################

# Consider an SIR model. Infected individuals pass the infection on to 
# Susceptibles with beta = 1.56756 and eventually die with gamma = 0.36508
#
# A CSV file has been provided for each of the three populations in the model:
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


#####################################
# 1 Reading in data and plotting
#####################################

# First, let's do all our pre-processing of the data so that our plots look okay
# from the get-go

# A.1 Read in the CSV files and convert them to a long format data frame
# Ensure you have made `state` into a factor variable
library(tidyverse)

all_dat <- read_csv("beta_1.56756_gamma_0.36508.csv")

all_dat_long <- gather(???)

all_dat_long$state <- ???

# 1.2 With your long data frame, make a plot that shows how the proportion of 
# the population in each state changes over time. You may choose any plotting
# geometry you consider appropriate from the documentation, and look at using
# aesthetics such as group, colour or fill, or faceting, to show variation
# across the various states. You might find the following link useful
#
# https://ggplot2.tidyverse.org/reference/index.html

ggplot(data = all_dat_long,
       aes(???)) +
    geom_???
