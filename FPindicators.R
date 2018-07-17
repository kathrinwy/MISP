### This code will estimate subnational popualtion by age and sex with WPP and household survey data for Burkina Faso in 2014 ###

# Set up ------------------------------------------------------------------

rm(list = ls())

#install.packages(c("tidyverse", "survey", "foreign", "data.table", "plyr", "dplyr", "magrittr", "ggplot2", "reshape2", "readr", "xlsx"))

#library(dplyr)
#library(magrittr)
#library(tidyverse)
#library(survey)
#library(foreign)
#library(data.table)
#library(ggplot2)
#library(reshape2)
#library(readr)
#library(xlsx)

#options(scipen = 999)  # disable scientific notation in R

#options(survey.lonely.psu = "adjust")

setwd("C:/Users/Kathrin Weny/Documents/")

# Read in survey data -----------------------------------------------------

raw_data          <- read.spss("./Household survey data/BFIR62FL.SAV", to.data.frame = TRUE) 
a <- as.data.frame(attr(raw_data, "variable.labels"))

raw_data <- raw_data %>%
  select(c("V005", "V021","V022", "V024", 
           "V312", "V313")) %>%
  mutate(id = 1)

# Bring in complex survey format ------------------------------------------

survey <- svydesign(id      = raw_data$V021,         # Primary sampling
                    strata  = raw_data$V022,         # Strata used for standard errors
                    weights = raw_data$V005/1000000, # HH sample weight
                    data    = raw_data)               # Data



# Method mix --------------------------------------------------------------


# national level
svymean(~V312, survey)


# estimate share of sex/age groups by region
output <- as.data.frame(svyby(~V312, ~V024,  survey, svymean))

output <- output[2:19]

names(output) <- substring(names(output), 5)

write.xlsx(WRA, "./Results/method_mix.xlsx")



# Type of method ----------------------------------------------------------


# national level
svymean(~V313, survey)


# estimate share of sex/age groups by region
output <- as.data.frame(svyby(~V313, ~V024,  survey, svymean))

output <- output[2:5]

names(output) <- substring(names(output), 5)

write.xlsx(WRA, "./Results/type_of_FP_method.xlsx")
