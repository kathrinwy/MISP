

setwd("C:/Users/weny/Google Drive/2018/Humanitarian/MISP/HDX data/uniform")

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
options(scipen=999) # disable scientific notation

listfiles      <- dir(pattern = "*.csv")

# Create country list
countries  <- vector(mode="character", length=0)

for(i in 1:length(listfiles)){
  countries[i]  <- str_extract(listfiles[i], ".+?(?=_)")
}

# Extract indicators

df <- data.frame(matrix(ncol = 6, nrow = 0))
names(df) <- c("country", "year", "admin.level", "admin.name", "wra", "total.population")

for(i in 1:length(countries)){
  
  # read in data
    tmp <- read.csv(listfiles[i])
    
  # Total population per admin boundary
  
    # to be written when final file structure ready
    
  # Percentage of women of reproductive age
    
    # to be written when final file structure ready
    
  # others columns
    
    tmp <- tmp %>%
      mutate(country = countries[i]) %>%
      mutate(year = substr(listfiles[i], nchar(listfiles[i])-7,  nchar(listfiles[i])-4))
}










  