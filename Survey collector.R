#install.packages( "devtools" , repos = "http://cran.rstudio.com/" )
library(devtools)
#install_github( "ajdamico/lodown" , dependencies = TRUE )
#install.packages("digest")
#install.packages( "convey" , repos = "http://cran.rstudio.com/" )
#install.packages( "srvyr" , repos = "http://cran.rstudio.com/" )


library(srvyr)
library(lodown)
library(convey)

# run over night, currently scopes through all years countries but we only need latest
# downloads all but Nepal for which a special permission was needed

setwd("C:/Users/weny/Google Drive/2018/Humanitarian/MISP/")

lodown( "dhs" , output_dir = file.path( path.expand( "~" ) , "surveys" ) , 
        your_email = "weny@unfpa.org" , 
        your_password = "Ichbininny123!" , 
        your_project = "Minimum Initial Service Package" )

# http://asdfree.com/demographic-and-health-surveys-dhs.html

# look for latest data

# extrct year - last 4 digits and if double year, last 7 digits

# MICS --------------------------------------------------------------------




# Select latest year ------------------------------------------------------

# Create dataframe wtih available DHS
for(country in countries){
  
  tmp              <- data.frame(matrix(ncol = , nrow = 0))
  names(tmp)       <- c("country", "survey")
  
  tmp$country      <- country
  tmp$survey       <- list.dirs(path = paste("C:/Users/weny/Documents/DHS/", country))
  
  
  tmp <- tmp  %>%
    mutate(year = substr(survey[i], -2)) # Extract last two digits
  
  tmp <- tmp %>% # filter data from the 90s - too old
    filter(year < 89)
  
  tmp <- tmp %>%
    filter(max(year))
  
}

# Add dataframe with available MICS

# group by country and then select max










