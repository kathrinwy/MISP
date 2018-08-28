

setwd("C:/Users/weny/Documents/DHS/Angola/Standard DHS 2015-16")

library(dplyr)
library(magrittr)
library(tidyverse)
library(survey)
library(foreign)
library(data.table)
library(ggplot2)
library(reshape2)
library(readr)
library(xlsx)
library(httr)
library(gridExtra)
library(devtools)
library(remotes)
library(DemoTools)
library(scales)
library(ipumsr)
library(stringr)
library(regex)
library(lodown)
options(scipen=999) # disable scientific notation

# DHS ---------------------------------------------------------------------

if(survey == "dhs"){
  
  # Read in survey data -----------------------------------------------------
  
  raw_data <- 
    readRDS( 
      file.path( "AOBR71FL.rds" )
    )
  
  # convert the weight column to a numeric type
  raw_data$weight <- as.numeric(raw_data$v005 )

   raw_data <- raw_data %>%
    select(c("weight", "v021","v022", "v024", 
             "v312", "v313")) %>%
    mutate(id = 1)
  
  # Bring in complex survey format ------------------------------------------
  
  survey <- svydesign(id      = raw_data$v021,         # Primary sampling
                      strata  = raw_data$v022,         # Strata used for standard errors
                      weights = raw_data$weight/1000000, # HH sample weight
                      data    = raw_data)               # Data
  
  
  
  # Method mix --------------------------------------------------------------
  
  a <-  as.data.frame(attr(raw_data, "variable.labels"))
  # national level
  svymean(~v312, survey)
  
  
  # estimate share of sex/age groups by region
  output <- as.data.frame(svyby(~v312, ~v024,  survey, svymean))
  
  output <- output[2:19]
  
  names(output) <- substring(names(output), 5)
  
  write.xlsx(WRA, "./Results/method_mix.xlsx")
  
  
  
  # Type of method ----------------------------------------------------------
  
  
  # national level
  svymean(~v313, survey)
  
  
  # estimate share of sex/age groups by region
  output <- as.data.frame(svyby(~V313, ~V024,  survey, svymean))
  
  output <- output[2:5]
  
  names(output) <- substring(names(output), 5)
  
  write.xlsx(WRA, "./Results/type_of_FP_method.xlsx")
  
  
  # Sex and age structure at Admin1 -----------------------------------------
  
  
}



# MICS --------------------------------------------------------------------


if(survey == "mics"){
  
  
  
}
}

