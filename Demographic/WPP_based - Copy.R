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

# Set the link bellow to the folder where you save the data

setwd("C:/Users/weny/Desktop/Exercise")

# Set up ------------------------------------------------------------------

name <- "Cabo Verde"

latest_year <- 2018

# Read in WPP population data and select  ---------------------------------


national_data_female <- read.csv("./Input_data/UN modelled_estimates/WPP female_annual population by age_national level.csv",
                                 check.names = FALSE, skip = 11)

national_data_female <- national_data_female  %>%
  filter(country == name & year == latest_year)%>%
  mutate(sex = "Female")

national_data_male    <- read.csv("./input_data/un_modelled_estimates/WPP male_annual population by age_national level.csv"
                                  , check.names = FALSE, skip = 11)

national_data_male <- national_data_male%>%
  filter(country == name & year == latest_year) %>%
  mutate(sex = "Male")

full_data <- bind_rows(national_data_male, national_data_female) %>%
  gather(key = "age", value = "national_population", -country, - year, -sex) %>%
  select(-year)

full_data$age <- as.numeric(as.character(full_data$age))


# age group ---------------------------------------------------------------


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
                                                                                                                                        ifelse(80 <= full_data$age & full_data$age < 85, "80-85",
                                                                                                                                               ifelse(85 <= full_data$age, "85+", NA))))))))))))))))))




# Figure ------------------------------------------------------------------

population_data <- aggregate(full_data[,sapply(full_data,is.numeric)],full_data[c("age_group", "sex")],sum)

population_data <- population_data %>%  
  mutate(age_group = factor(x = age_group, levels = unique(age_group)))

population_data$age_group = factor(population_data$age_group,levels(population_data$age_group)[c(1,10,2:9, 11:18)])

g <- ggplot(population_data, mapping = aes(x = age_group, y = national_population, fill = sex, 
                                label=round(national_population, digits = 2)))+
  geom_bar(stat = "identity", mapping = aes(y = ifelse(population_data$sex == "Male", -national_population, national_population)))+
  
  coord_flip() +
  scale_fill_manual(values=c("darkorange", "dodgerblue3"))+
  
  scale_y_continuous(labels = abs, limits = c(-max(population_data$national_population), max(population_data$national_population)))+
  scale_x_discrete(labels = c("0-4","5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
                              "50-54", "55-59", "60-64", "56-69", "70-74", "75-79", "80-85", "85+")) +
  
  
  labs(x = "Age", y = "Population", fill = "Sex")+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill="aliceblue"), axis.line = element_line(colour="gray59"), 
        strip.text = element_text(size=15),
        strip.background =element_rect(fill="white"),
        panel.spacing.x = unit(3,"lines"))

ggsave("WPP.pdf", device="pdf", path="./Results/Population Pyramids", g, scale= .5, width = 18, height = 14, units="in")
