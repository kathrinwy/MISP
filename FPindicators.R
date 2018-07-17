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

setwd("C:/Users/weny/Google Drive/2018/Humanitarian/Summer Workshops/WCARO/Data/2- Sexual and Reproductive Health")

# Read in survey data -----------------------------------------------------

raw_data          <- read.spss("./BFIR62FL.SAV", to.data.frame = TRUE) 
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

# estimate share of sex/age groups by region
national <- as.data.frame(svymean(~V312, survey)%>% prop.table(1))

svytable(~V024+V312, survey)

svyby(~V024+V312,survey, svymean)

b <- as.data.frame(svytable(~V024, ~V313, survey))
svymean(~V312, survey)
method <- c <- as.data.frame(svyby(~V024, ~V312, survey, svymean))

%>% prop.table(1))
names(method) <- c("region", "method", "proportion")

type   <- as.data.frame(svytable(~V024+V313, survey)%>% prop.table(2))
names(type) <- c("region", "type", "proportion")

# calculate population numbers per region ---------------------------------

full_data <- full_data %>%
  mutate(regional_population = as.numeric(percentage)*as.numeric(national_population)) 

WRA <- full_data %>%
  select(age, sex, region, regional_population) %>%
  filter(sex == "Female" & (15 <= as.numeric(as.character(age)) & as.numeric(as.character(age)) < 50)) 

WRA <- WRA %>%
  arrange(region) %>%
  mutate(running.sum.regional_population = cumsum(regional_population)) %>%
  select(region, running.sum.regional_population)%>%
  group_by(region)%>%
  summarize(target=last(running.sum.regional_population))%>%
  select(c("region", "target"))

names(WRA) <- c("region", "WRA, in '000")

write.xlsx(WRA, "./Results/Data/women_repr_age.xlsx")
write.xlsx(full_data, "./Results/Data/subnational_population_data.xlsx")









