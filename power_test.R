############
# Start Code
############
rm(list=ls())

# to detach all packages at the start: https://rdrr.io/github/jasongraf1/JGmisc/man/detachAllPackages.html
#devtools::install_github("jasongraf1/JGmisc")
library(JGmisc)
detachAllPackages(keep = NULL)

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

#power_total_long3$States <- as.character(power_total_long3$States)

#check dataset
glimpse(power_total_long3)
summary(power_total_long3)
unique(power_total_long3$Date)

#############################

#install.packages("raster")
library(sp)
library(raster) # raster is dependant on sp package

india01_test <- getData('GADM', country='IND', level=1)

head(india01_test@data, n=2)

names(india01_test)
unique(india01_test$NAME_1)
#plot(india01_test)

##################################
# Steps for plotting spatial data
##################################
#Below I followed exact steps from : https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf 


# changing name of the state identifier column name to match the power dataset
#names(india01_test)[names(india01_test) == "NAME_1"] <- "States" 

# merge data with the shape file

library(dplyr)
head(india01_test$NAME_1)
head(power_total_long3$States)

#head(left_join(india01_test@data, power_total_long3, by = c("NAME_1" = "States"))) # to check how data will look after joining 

india01_test@data <- (left_join(india01_test@data, power_total_long3, by = c("NAME_1" = "States"))) 

# library(tmap)
# qtm(india01_test, "Usage")
# qtm(shp = india01_test, fill = "Usage", fill.pallette = "-Blues")

library(ggplot2)

p <- ggplot(india01_test@data, aes(Date, Usage), na.omit = TRUE)
p
p+geom_point(aes(colour = Usage))+
  geom_text(size = 2 , aes(label = NAME_1))

# convert spatial data into dataframe
library(rgeos)
india01_test_f <- fortify(india01_test)
head(india01_test_f, n =2) # peak at the fortified data

#allocate id variable to the shape file as fortified data have id variable
#india01_test$id <- row.names(india01_test)
india01_test@data$id <- rownames(india01_test@data)

head(india01_test@data, n = 2)  #peak at the shape file before join

#join fortified data and data from shape file
india01_test_f <- left_join(india01_test_f, india01_test@data) # it automatically joins by "id"

# plot the data

# map <- ggplot(india01_test_f, aes(long, lat, group = group, fill = Usage)) + 
#   geom_polygon() + 
#   coord_equal() +
#   labs(x = "latitide", y = "longitude",
#        fill = "Power\nConsumption(MU)") + 
#   ggtitle("India Power Consumption")
# 
# 
# map
############################
# Animate the spatial data
###########################


library(tidyverse)
library(gganimate)
library(ggthemes)
library(choroplethr)
library(maps)
library(htmltools)
library(mapproj)
#require(transformr)

theme_set(theme_minimal())

theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      legend.title = element_blank(), 
      panel.border = element_blank(),
      legend.position = "none",
      plot.caption = element_text(hjust = 1, face= "italic"),
      plot.title.position = "plot",
      plot.caption.position =  "plot"
    )
}

ggplot(power_total_long3, aes(map_id = States, frame = Date))+
  geom_map(aes(fill = Usage),
           map = india01_shape_df) +
  expand_limits(x = india01_shape_df$long, y = india01_shape_df$lat) +
  coord_fixed(.96) +
  scale_fill_gradient(low="thistle2", high="darkred", 
                      guide="colorbar", na.value="white") +
  labs(title = "India Power Consumption in 2019-20", x = element_blank(), y = element_blank(), 
       fill='Usage') +   
  theme_classic() +
  theme(legend.position = "bottom") +
  transition_states(
    states = date,
    transition_length = 1.5,
    state_length = 1
  )


b <- ggplot(data = india01_test_f,
            aes(frame = Date)) +
  geom_polygon(aes(
    x = long,
    y = lat,
    group = group,
    fill = Usage
  ),
  color = "#8c8c8c") +
  theme_map() +
  scale_fill_gradient(low="thistle2", high="darkred", 
                      guide="colorbar", na.value="white") +
  coord_map() + #"albers",
            #lat0 = 30,
            #lat1 = 40) +
  labs(title = "State Level Daily Power Consumption in India \n(1 January 2019 to 23 May 2020", x = element_blank(), y = element_blank(), 
       subtitle = "Week of: {frame_along}",
       fill='Usage(MU)',
       caption = "Power Consumption data from Kaggle\nChart by @preshitambade") +
  theme_classic() +
  theme(legend.position = "bottom") +
  transition_states(
    states = date,
    transition_length = 1.5,
    state_length = 1
  )

animate(b, height = 600, width = 1000, end_pause = 10)

anim_save(filename = "3_output/power_consumption_indiamap_animate.gif") 









