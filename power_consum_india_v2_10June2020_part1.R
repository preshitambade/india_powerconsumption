# India Power Consumption during COVID-19 period
# preshit ambade
#preshitambade@gmail.com
# date: 10th June 2020
# Data source:  {https://www.kaggle.com/twinkle0705/state-wise-power-consumption-in-india}
# Animation inspiration:{https://kelseygonzalez.github.io/portfolio/COVID-19%20Population%20Mobility/}
# International energy consumption: https://medium.com/@aarushidave/electricity-consumption-during-covid-19-e6fa5ea91dbb

#creating maps: http://data-analytics.net/wp-content/uploads/2014/09/geo2.html
#https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
#https://kelseygonzalez.github.io/portfolio/COVID-19%20Population%20Mobility/
#https://github.com/kelseygonzalez/covid_mobility
# map animate: https://ditheringdata.netlify.app/2018/01/01/gganimate/
# create beutiful maps: https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
# mobility data: https://www.cuebiq.com/visitation-insights-covid19/
# india maps in R link-1: https://rpubs.com/abhaypadda/plotting-maps-in-R
# india maps in R link-2: https://rpubs.com/ss4ever92/353936
# india maps in R link-3: https://rstudio-pubs-static.s3.amazonaws.com/435552_5656a7fe6636474fb0f8d4222d79db2c.html
# india maps in R link-4: https://stackoverflow.com/questions/22997276/an-r-package-for-india
# india maps in R link-5: http://visual.yantrajaal.com/2015/05/using-r-for-maps-of-india-state.html




############
# Start Code
############
rm(list=ls())

# to detach all packages at the start: https://rdrr.io/github/jasongraf1/JGmisc/man/detachAllPackages.html
#devtools::install_github("jasongraf1/JGmisc")
library(JGmisc)
detachAllPackages(keep = NULL)

# to turn global warnings off
# link: https://stackoverflow.com/questions/16194212/how-to-suppress-warnings-globally-in-an-r-script
#options(warn=-1)


#gganimate
getwd()
setwd("/Users/preshitambade/Downloads/power_consum_india")
# install.packages("gganimate")
library(gganimate)
#install.packages(c("gifski","av"))
library(gifski)
library(av)
# install.packages("scales")
library(scales)
library(ggplot2)
library(tidyverse)

# this dataset is downloaded from Kaggle.com: 
# https://www.kaggle.com/twinkle0705/state-wise-power-consumption-in-india

#install.packages("readxl")
library(readxl)

power <- read_xlsx("1_Data/long_data_copy.xlsx") %>% 
  mutate(Date = as.Date(Date, format = "%m-%d-%Y"),
         States = as.factor(States),
         Regions = as.factor(Regions))


# Note: in original .csv data file dates are not in correct format. So if you open that file and change class of the dates then it doesn't work properly.
# check the link for related discussion. https://stackoverflow.com/questions/15566875/as-date-returning-na-while-converting-from-ddmmmyyyy
# So I have to create date column manually. Accordingly I have created new date column and saved all the dates in correct order and format.
# then I have saved the new file in excel which I am using now.
#power <- power[rowSums(is.na(power)) == 0,] #removing any NA and empty cells which causes problem later

#check dataset
glimpse(power)
summary(power)
unique(power$Date)

####################################
#creating other useful sub-datasets
####################################
#1. Load dataset
power_total_long3 <- power %>%
  filter(States != "ALL INDIA TOTAL") %>%  #  filter(States != "ALL INDIA TOTAL" & Date == as.Date("2019-10-28")) %>% 
  select(Date, States, Usage) # , longitude, latitude: don't need long and lat as it is already in shape file

# power_total_long3 <- power %>%
#   filter(States != "ALL INDIA TOTAL") %>% 
#   select(Date, States, Usage) # , longitude, latitude: don't need long and lat as it is already in shape file



# change state names in dataframe
library(plyr)
library(dplyr)

power_total_long3 <- power_total_long3 %>% 
  mutate(States=revalue(States, c("HP"="Himachal Pradesh", "DNH"="Dadara & Nagar Havelli",
                                  "J&K"="Jammu and Kashmir", "MP"="Madhya Pradesh",
                                  "NER Meghalaya"="Meghalaya", "Delhi"="NCT of Delhi",
                                  "Pondy"="Puducherry","NR UP"="Uttar Pradesh")))

# above step is done to match the state names with the shape files used in spatial analysis

#check dataset
glimpse(power_total_long3)
summary(power_total_long3)
unique(power_total_long3$Date)

