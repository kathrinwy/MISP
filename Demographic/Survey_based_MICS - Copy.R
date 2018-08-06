### This code will estimate subnational popualtion by age and sex with WPP and household survey data for 
### the country an year you indicate during "Set up"

# Set up ------------------------------------------------------------------

rm(list = ls())

#install.packages(c("tidyverse", "survey", "foreign", "data.table", "plyr", "dplyr", "magrittr", "ggplot2", "reshape2", "readr", "xlsx", "RCurl"))

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

options(scipen = 999)  # disable scientific notation in R

options(survey.lonely.psu = "adjust")

# Set the link bellow to the folder where you save the data

setwd("C:/Users/weny/Desktop/Exercise")


# Set up ------------------------------------------------------------------

name <- "Central African Republic"

latest_year <- 2010

# Read in WPP population data and select  ---------------------------------

national_data_female <- read.csv("./Input_data/UN modelled estimates/WPP female_annual population by age_national level.csv",
                                 check.names = FALSE, skip = 11)

national_data_female <- national_data_female  %>%
  filter(country == name & year == latest_year)%>%
  mutate(sex = "Female")

national_data_male    <- read.csv("./Input_data/UN modelled estimates/WPP male_annual population by age_national level.csv"
                                  , check.names = FALSE, skip = 11)

national_data_male <- national_data_male%>%
  filter(country == name & year == latest_year) %>%
  mutate(sex = "Male")

population_data <- bind_rows(national_data_male, national_data_female) %>%
  gather(key = "age", value = "national_population", -country, - year, -sex)

# Read in survey data -----------------------------------------------------

filename          <- paste("./Input_data/Household surveys/MICS/", name, ".sav", sep ="")

raw_data          <- read.spss(filename, to.data.frame = TRUE) 
a <- as.data.frame(attr(raw_data, "variable.labels"))

raw_data <- raw_data %>%
  select(c("HH2","hhweight", "HH1","HH6", "HH7", "HL6", "HL4")) 

survey_data <- raw_data %>%
  mutate(strata = paste(HH7, HH6, sep =""))

# Bring in complex survey format ------------------------------------------

survey <- svydesign(id      = survey_data$HH1,          # Primary sampling unit
                    strata  = survey_data$strata,       # Strata 
                    weights = survey_data$hhweigt,      # Household sample weight
                    data    = survey_data)              # Data set used

# estimate share of sex/age groups by region

data <- as.data.frame(svytable(~HH7+HL6+HL4, survey))# prop.table(2))

names(data) <- c("region", "age", "sex", "frequency")

data$age <- as.numeric(as.character(data$age))

data <- data %>%
  filter(!is.na(age))

data <- data %>%
  dplyr::group_by(age,sex) %>%
  mutate(total_age = sum(frequency)) %>%
  mutate(percentage = ifelse( frequency != 0, frequency/total_age, 0))

# Standardizing the sex labels
data$sex <- ifelse(data$sex == "Homme" | data$sex == "Masculin" | data$sex == "Masculino" | data$sex == "Male", "Male",
                   ifelse(data$sex == "Femme" | data$sex == "Feminin" | data$sex == "Femenino" | data$sex == "Female" | data$sex == "Feminino"
                          | data$sex == "Féminin", "Female", 
                          ifelse(data$sex == "Missing" | data$sex == "DK", NA, data$sex)))

data$age <- ifelse(data$age == 94, NA, data$age)


# Merge data --------------------------------------------------------------

full_data <- merge(data, population_data, by = c("age", "sex"))

# calculate population numbers per region ---------------------------------

full_data <- full_data %>%
  mutate(regional_population = as.numeric(percentage)*as.numeric(full_data$national_population)*1000) %>%
  select(c(age, sex, region, regional_population))

full_data$age <- as.numeric(as.character(full_data$age))


full_data$age_group <-  ifelse(0 <= full_data$age & full_data$age < 5, "0-4",
                               ifelse(5 <= full_data$age & full_data$age  < 10, "5-9", 
                                      ifelse(10 <= full_data$age & full_data$age  < 15, "10-14", 
                                             ifelse(15 <= full_data$age & full_data$age < 20, "15-19", 
                                                    ifelse(20 <= full_data$age & full_data$age < 25, "20-24", 
                                                           ifelse(25 <= full_data$age & full_data$age < 30, "25-29",
                                                                  ifelse(30 <= full_data$age & full_data$age < 35, "30-34",
                                                                         ifelse(35 <= full_data$age & full_data$age < 40, "35-39", 
                                                                                ifelse(40 <= full_data$age & full_data$age < 45, "40-44",
                                                                                       ifelse(45 <= full_data$age & full_data$age < 50, "45-49", 
                                                                                              ifelse(50 <= full_data$age & full_data$age < 55, "50-54", 
                                                                                                     ifelse(55 <= full_data$age & full_data$age < 60, "55-59",
                                                                                                            ifelse(60 <= full_data$age & full_data$age < 65, "60-64", 
                                                                                                                   ifelse(65 <= full_data$age & full_data$age < 70, "65-69", 
                                                                                                                          ifelse(70 <= full_data$age & full_data$age < 75, "70-74", 
                                                                                                                                 ifelse(75 <= full_data$age & full_data$age < 80, "75-79", 
                                                                                                                                        ifelse(80 <= full_data$age, "80+", NA)))))))))))))))))



# 5 year age groups -------------------------------------------------------

# Women


women <- full_data %>%
  filter(sex == "Female") 

women <- aggregate(women[,sapply(women,is.numeric)],women[c("age_group", "region")],sum)

women <- women %>%
  select(-age) %>%
  mutate(Sex = "Female")

names(women) <- c("age.group","region",  "population.estimate", "sex")



# Men

men <- full_data %>%
  filter(sex == "Male") 

men <- aggregate(men[,sapply(men,is.numeric)],men[c("age_group", "region")],sum)

men <- men %>%
  select(-age) %>%
  mutate(Sex = "Male")

names(men) <- c("age.group", "region", "population.estimate", "sex")


# Total

population_estimate <- rbind(men, women)

# Export data -------------------------------------------------------------

write.xlsx(as.data.frame(population_estimate), paste("./results/", name,"estimate_5year.xlsx", sep = ""), row.names = F)

# visualize with pop pyramid ----------------------------------------------


regions <- unique(full_data$region)

population_estimate <- population_estimate %>%  
  mutate(age.group = factor(x = age.group, levels = unique(age.group)))

population_estimate$age.group = factor(population_estimate$age.group,levels(population_estimate$age.group)[c(1,10,2:9, 11:18)])

for(i in regions){
  
  temp <- population_estimate %>%
    filter(region == i) 
  
  g <- ggplot(temp, mapping = aes(x = age.group, y = population.estimate, fill = sex, 
                                  label=round(population.estimate, digits = 2)))+
    geom_bar(stat = "identity", mapping = aes(y = ifelse(temp$sex == "Male", -population.estimate, population.estimate)))+
    
    coord_flip() +
    scale_fill_manual(values=c("darkorange", "dodgerblue3"))+
    
    scale_y_continuous(labels = abs, limits = c(-max(temp$population.estimate), max(temp$population.estimate)))+
    scale_x_discrete(labels = c("0-4","5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                                "50-54", "55-59", "60-64", "56-69", "70-74", "75-79", "80-85", "85+")) +
    
    labs(x = "Age", y = "Population", fill = "Sex")+
    
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill="aliceblue"), axis.line = element_line(colour="gray59"), 
          strip.text = element_text(size=15),
          strip.background =element_rect(fill="white"),
          panel.spacing.x = unit(3,"lines"))
  filename <- paste(name, i, ".pdf", sep="")
  
  ggsave(filename, device="pdf", path="./Results/Population pyramids", g, scale= .5, width = 18, height = 14, units="in")
  
}
















