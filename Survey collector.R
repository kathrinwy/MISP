
# Install and load packages -----------------------------------------------

#install.packages( "devtools" , repos = "http://cran.rstudio.com/" )
#install_github( "ajdamico/lodown" , dependencies = TRUE )
#install.packages("digest")
#install.packages( "convey" , repos = "http://cran.rstudio.com/" )
#install.packages( "srvyr" , repos = "http://cran.rstudio.com/" )

library(devtools)
library(srvyr)
library(lodown)
library(convey)
library(dplyr)
library(magrittr)
library(tidyverse)
library(survey)
library(foreign)
library(reshape2)
library(data.table)
library(ggplot2)
library(lme4)
library(geepack)
library(gridExtra)
library(lattice)
library(sjstats)
library(TMB)
library(readr)

setwd("C:/Users/weny/Google Drive/2018/Humanitarian/MISP/")

# Executive statements ----------------------------------------------------

# run over night, currently scopes through all years countries but we only need latest
# downloads all but Nepal for which a special permission was needed

lodown( "dhs" , output_dir = file.path( path.expand( "~" ) , "surveys" ) , 
        your_email = "weny@unfpa.org" , 
        your_password = "Ichbininny123!" , 
        your_project = "Minimum Initial Service Package" )

# Source: http://asdfree.com/demographic-and-health-surveys-dhs.html

# MICS --------------------------------------------------------------------

# lodown scraper not finalized: https://github.com/ajdamico/lodown/blob/master/R/mics.R

# Select latest year ------------------------------------------------------

# Create dataframe with available DHS, including year

dhs <- as.data.frame(matrix(NA, nrow = 0, ncol = 3))

for(country in country_list){
 
  tmp             <- as.data.frame(as.character(list.dirs(path = paste("C:/Users/weny/Documents/DHS/", country, sep = ""),
                                                    recursive = F)))
  if(nrow(tmp) > 0){ # if there is any data for a country
    
  names(tmp)      <- "link"
  
  tmp$year        <- substr(tmp$link, (nchar(as.character(tmp$link)) + 1)-2, nchar(as.character(tmp$link))) # due to inconsistent formatting, only last 2 digits can be used to recreate year
  
  tmp$year_latest <- ifelse(tmp$year > 80, paste(19, tmp$year, sep = ""), paste(20,tmp$year, sep = "")) # reconstruct full year
                       
  tmp$country     <- country
  
  tmp$survey      <- "dhs"
  
  tmp <- tmp %>%
         select(c("country", "year_latest", "survey"))
  
  dhs <- rbind(dhs, tmp) # append to full dataset
  
  } 
}

# Import the data catalogue from MICS and add to data availability from DHS

mics <- read_csv("C:/Users/weny/Google Drive/2018/Humanitarian/MISP/MISP-Rproject/surveys_catalogue_mics.csv") %>%
        filter(status == "Completed" & datasets == "Available") %>%
        select(c("country", "year")) %>%
        mutate(survey = "mics")

mics$year_latest <- substr(mics$year, (nchar(as.character(mics$year)) + 1)-4, nchar(as.character(mics$year)))

mics <- mics %>%
  select(-year)

data_availability <- rbind(dhs, mics)

# group by country and then select max

latest_dataset <- data_availability %>%
                  group_by(country) %>%
                  slice(which.max(year_latest))
 
# surveys to be downloaded manually ---------------------------------------

mics_latest <- latest_dataset %>%
               filter(survey == "mics")

dhs_latest  <- latest_dataset %>%
              filter(survey == "dhs")





