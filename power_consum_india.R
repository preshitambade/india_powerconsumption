# India Power Consumption during COVID-19 period
# preshit ambade
#preshitambade@gmail.com
# date: 8th June 2020
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



#####################
#1. making basic plot
#####################

#power_baseplot <- ggplot(data = subset(power, !is.na(Positive)), #excluding NA in positive column
power_baseplot <- ggplot(data = power,
                           mapping = aes(x=Date, y = Usage, color = States))+
  scale_y_continuous(labels = scales::comma_format()) +
  #  scale_x_date(date_breaks = "1 week", limits = c(as.Date("2020-04-01"), as.Date("2020-06-05"),expand=c(0,0)))+
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d", expand=c(0,0)) +
  geom_point(aes(size = `Usage`), alpha = .6) 

power_baseplot


################
#2. refine graph
################

power_baseplot2 <- power_baseplot+
  theme_classic() +
  theme(legend.position = "none")


power_baseplot2



#######################
#3. make transition GIF
#######################

#Note: the destination folder needs to be given write permission before running this.
# otherwise gets error: Warning message:
#file_renderer failed to copy frames to the destination directory
#https://community.rstudio.com/t/warning-message-file-renderer-failed-to-copy-frames-to-the-destination-directory/45261

power_baseplot3 <- power_baseplot2+
  labs(title = "Daily Power Consumption by states in India",
       subtitle = "Date: {frame_time}") + 
  transition_time(Date) +
  shadow_wake(wake_length = 0.2)

power_baseplot3


################################
#4. Animate for selected States
###############################

#we want to plot total number of powers done by each states in India 
#so filter the main dataset accordingly in long format

power_total_long <- power %>%
  filter(States != "ALL INDIA TOTAL") %>% 
  select(Date, States, Usage)


glimpse(power_total_long)

interesting <- c(
  "Tamil Nadu", "Andhra Pradesh", "Karnataka", #"Telangana", - telangana data is inconsistent so excluded
  "Maharashtra", "Delhi", "Gujarat")#, "ALL INDIA TOTAL")


power_total_long <-
  power_total_long %>% mutate(
    colors = ifelse(States %in% interesting, "interesting", "not"), # it creates binary variable named colors where interesting states take value = interesting, otherwise "not"
    lab = ifelse(States %in% interesting, str_to_upper(States), "") # this creates label variable where names of the states are all in caps if they are from interesting category
    #we can capitalize state names in lab variable directly when we define iteresting and can skip the above line of code.
  )

# we want only abbreviated forms of the interesting state names so change the row values in lab variable as follows
power_total_long$lab[power_total_long$lab == "DELHI"] <- "DL"
power_total_long$lab[power_total_long$lab == "MAHARASHTRA"] <- "MH"
power_total_long$lab[power_total_long$lab == "TAMIL NADU"] <- "TN"
power_total_long$lab[power_total_long$lab == "GUJARAT"] <- "GJ"
power_total_long$lab[power_total_long$lab == "ANDHRA PRADESH"] <- "AP"
power_total_long$lab[power_total_long$lab == "KARNATAKA"] <- "KA"
#power_total_long$lab[power_total_long$lab == "ALL INDIA TOTAL"] <- "IND"

#interesting <- c("tn", "mh", "dl", "gj") # we are interested in Tamilnadu(tn), Maharashtra(mh), Delhi(dl), and Gujarat(gj)

# objects for lockdown dates
lock1startdate <- as.Date("2020-03-24") #lockdown-1 start date
lock1enddate <- as.Date("2020-04-14") #lockdown-1 end date
lock2enddate <- as.Date("2020-05-03") #lockdown-3 start date
lock3enddate <- as.Date("2020-05-17") #lockdown-3 end date

a <- ggplot(power_total_long, aes(x = Date,
                                    y = Usage,
                                    group = States)) +
  geom_line(aes(color = colors),
            show.legend = FALSE,
            alpha = 0.7) +
  scale_color_manual(values = c('black', 'grey80')) +
  geom_point(
    data = power_total_long %>% filter(colors != "interesting"),
    shape = 21,
    colour = 'grey80',
    fill = 'grey80',
    alpha = 0.7,
    size = 2
  ) +  # chunk upto this point creates line graph where black lines are for interesting states and gray for non-interesting
  geom_vline(xintercept = lock1startdate, col = "red", lty = 2) + #add vertical lines for lockdown dates
  geom_vline(xintercept = lock1enddate, col = "green", lty = 2) +
  geom_vline(xintercept = lock2enddate, col = "orange", lty = 2) +
  geom_vline(xintercept = lock3enddate, col = "black", lty = 2) +
    geom_point(
    data = power_total_long %>% filter(colors == "interesting"),
    aes(fill = Usage),
    shape = 21,
    colour = "black",
    alpha = 0.7,
    size = 4,
    stroke = 2
  ) + # this specific code block creates black circles around blue colered dots for each datapoint for interested states
  #  scale_fill_gradient(low = "white", high = "forestgreen", labels = comma) + # how to change scientific notation on labels: https://stackoverflow.com/questions/40072971/how-to-change-scientific-notation-on-legend-labels-in-ggplot2
  scale_fill_gradientn(colours = rainbow(5), labels = comma) +
  geom_text(
    aes(label = lab),
    show.legend = FALSE,
    hjust = 1.8, # for height adjustment see:https://www.gl-li.com/2017/08/18/place-text-at-right-location/ 
    vjust = 0.5,
    size = 3 # this sets size of the label
    #check_overlap = TRUE, # this checks for the overlap
  ) +  # this code block changes colors of the data to green to white and includes labels for interesting states which are stored in the variable called "lab"
  xlab("") +
  ylab("Daily Power Consumed in Mega Units (MU)") +
  scale_x_date(date_breaks = "1 week",
               date_labels = "%m-%d") +
  scale_y_continuous(labels = scales::comma_format()) + #this code block respecifies x and y axis for better visualization
  transition_reveal(along = Date) +
  ggtitle("Daily Power Consumption by Selected States in India \n(28 October 2019 to 23 May 2020)",
          subtitle = "Week of: {frame_along}") +
  labs(caption = "(based on data from weekly energey reports published by Power System Operation Corporation Limited (POSOCO))") + # this adds caption at bottom right corner
  view_follow(fixed_y = T)  # this code block animates the plot with suitable title



animate(a, height = 600, width = 1000, end_pause = 10)

anim_save(filename = "3_output/power_consumption_state_india_animate.gif") 

#Data source: Weekly energey reports from Power System Operation Corporation Limited (POSOCO)

############################################
#5. Creating Static Plot for selected states
############################################

# Just plot the graph without animation for selected states
power_total_long2 <- power_total_long %>%
  filter(States =="Tamil Nadu" | States == "Andhra Pradesh" | States =="Karnataka" | #"Telangana", - telangana data is inconsistent so excluded
           States =="Maharashtra" | States =="Delhi" | States =="Gujarat") %>% 
  select(Date, States, Usage, lab)



b <- ggplot(power_total_long2, aes(x = Date,
                                  y = Usage,
                                  colour = lab)) +
  geom_line(position = "identity") +
  geom_vline(xintercept = lock1startdate, col = "red", lty = 1) + #add vertical lines for lockdown dates
  geom_vline(xintercept = lock1enddate, col = "green", lty = 1) +
  geom_vline(xintercept = lock2enddate, col = "orange", lty = 1) +
  geom_vline(xintercept = lock3enddate, col = "black", lty = 1) +
  xlab("") +
  ylab("Daily Power Consumed in Mega Units (MU)") +
  scale_x_date(date_breaks = "1 week",
               date_labels = "%m-%d") +
  theme(legend.position = "right", axis.text.x = element_text(angle = 90))+
  ggtitle("Daily Power Consumption by Selected States in India \n(28 October 2019 to 23 May 2020)") +
  labs(caption = "(based on data from weekly energey reports published by Power System Operation Corporation Limited (POSOCO))") # this adds caption at bottom right corner
b

ggsave(filename = "3_output/power_consumption_state_india_static.tiff")

########################
#6. Geo Spatial Analysis
########################

# Steps to follow to download shape files for India:
#1. go to GADM Website and search India: https://gadm.org/download_country_v3.html
#2. download R(sp)/ R(sf)/ Geopackage files for level0(statelevel) which can be open in R
# Discription of these file types is given at: https://gadm.org/formats.html

#Alternatively I can open shape file using rgdal::readOGR function
# The state level shape file is downloaded from forked GitHub repository: 
# HindustanTimesLabs/shapefiles : https://github.com/HindustanTimesLabs/shapefiles

# working with spatial data guide: https://cmerow.github.io/RDataScience/04_Spatial.html


library(ggplot2)
#install.packages("rgeos")
library(rgeos)
#install.packages("rgdal")
library(rgdal)
library(maptools)
#install.packages("ggmap")
library(ggmap)
library(scales)
library(RColorBrewer)

#install.packages("sp")
library(sp)

#india_map <- readRDS("1_data/india_shape_files/gadm36_IND_0_sp.rds")

# Step1 : Load shape file, link: Plot Spatial Data / Shapefiles in R | Gun Violence in Chicago: https://www.youtube.com/watch?v=uZtto0cYjZM
india_shape <- rgdal::readOGR(dsn = "1_data/india_shape_files/india_state", layer="India-States")

plot(india_shape)

names(india_shape) #check shape file attributes
print(india_shape$ST_NM) #check shape file attributes

# Step2: get ready data file : data file is already loaded

# Step3: fortify shape into dataframe
india_shape_fortify <- fortify(india_shape, region = "ST_NM")


# Step4: merge shapefile with csv file
merge.shp.coef<-merge(shp.f,imr, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]

# Step5: create map
ggplot()+
  geom_polygon(data = final.plot,
               aes(x = long, y = lat, group = group, fill = count),
               color = "black", size = 0.25) + coord_map()


#ggsave("India_IMR_2013_BLUE.png",dpi = 300, width = 20, height = 20, units = "cm")
#############################################################################################

#Creating and ploting spatial object
#Link: https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf

#1. Load dataset
power_total_long3 <- power %>%
  filter(States != "ALL INDIA TOTAL" & Date == as.Date("2019-10-28")) %>% 
  select(Date, States, Usage) # , longitude, latitude: don't need long and lat as it is already in shape file

# power_total_long3 <- power %>%
#   filter(States != "ALL INDIA TOTAL") %>% 
#   select(Date, States, Usage) # , longitude, latitude: don't need long and lat as it is already in shape file

#2. load shape file
india_shape <- rgdal::readOGR(dsn = "1_data/india_shape_files/india_state", layer="India-States")

#str(india_shape)

plot(india_shape)

names(india_shape) # to know where state names are stored in the shape file

print(india_shape$ST_NM) # this shows what state names are stored

nrow(india_shape)

#3. merging shape file with data
#india_shape_merge <- merge(india_shape, power_total_long3,  by="States", all.x=TRUE)

#comparing names of the states before merge
india_shape$ST_NM %in% power_total_long3$States

# return rows where names do not match
india_shape$ST_NM[!india_shape$ST_NM %in% power_total_long3$States]

# to know unique names from both the files
unique(india_shape$ST_NM)

unique(power_total_long3$States)

# change state names in dataframe

#power_total_long3$States <- as.character(power_total_long3$States)

library(plyr)
library(dplyr)

power_total_long3 <- power_total_long3 %>% 
  mutate(States=revalue(States, c("HP"="Himachal Pradesh", "DNH"="Dadara & Nagar Havelli",
                                 "J&K"="Jammu & Kashmir", "MP"="Madhya Pradesh",
                                 "NER Meghalaya"="Meghalaya", "Delhi"="NCT of Delhi",
                                 "Pondy"="Puducherry","NR UP"="Uttar Pradesh")))

#since it is factor variable we have to use revalue funtion to change levels
# link- https://stackoverflow.com/questions/40064324/use-revalue-in-data-manipulation-with-dplyr

india_shape$ST_NM[india_shape$ST_NM=="Arunanchal Pradesh"]<- "Arunachal Pradesh" #correct spelling in shape file


# recheck by returning rows where names do not match
india_shape$ST_NM[!india_shape$ST_NM %in% power_total_long3$States]

india_shape@data$id <- rownames(india_shape@data)

# merge files by left join
#names(india_shape)[names(india_shape) == "ST_NM"] <- "States" # changing name of the state identifier column name to match the power dataset
#india_shape@data <- left_join(india_shape@data, power_total_long3, by = "States")

india_shape@data <- left_join(india_shape@data, power_total_long3, by = c("ST_NM" = "States"))

india_shape_df <- fortify(india_shape) #alternative broom::tidy(india_shape, region = "States")

india_shape_df <-join(india_shape_df,india_shape@data, by="id")


ggplot(india_shape_df, aes(x=long, y = lat, group=ST_NM))+
  geom_polygon(aes(fill=Usage), color = "black", size = 0.25)+
  coord_fixed()+
  scale_fill_gradient(name= "Power\nConsumption\n(MU)", low = "white", high = "darkred")

#############################
# using GADM shape file
#############################

#install.packages("raster")
library(sp)
library(raster) # raster is dependant on sp package

#IN0 <- getData('GADM', country='IND', level=0)
india01 <- getData('GADM', country='IND', level=1)
#IN2 <- getData('GADM', country='IND', level=2)

names(india01)
unique(india01$NAME_1)

#load("IND_adm1.RData")
#comparing names of the states before merge
india01$NAME_1 %in% power_total_long3$States

# return rows where names do not match
india01$NAME_1[!india01$NAME_1%in% power_total_long3$States]

# to know unique names from both the files
unique(india_shape$ST_NM)

unique(power_total_long3$States)

# change state names in dataframe

#power_total_long3$States <- as.character(power_total_long3$States)

library(plyr)
library(dplyr)

power_total_long3 <- power_total_long3 %>% 
  mutate(States=revalue(States, c("HP"="Himachal Pradesh", "DNH"="Dadara & Nagar Havelli",
                                  "J&K"="Jammu and Kashmir", "MP"="Madhya Pradesh",
                                  "NER Meghalaya"="Meghalaya", "Delhi"="NCT of Delhi",
                                  "Pondy"="Puducherry","NR UP"="Uttar Pradesh")))

#recheck state name mismatch
india01$NAME_1[!india01$NAME_1%in% power_total_long3$States]

india01@data$id <- rownames(india01@data)

india01@data <- left_join(india01@data, power_total_long3, by = c("NAME_1" = "States"))

india01_shape_df <- fortify(india01) #alternative broom::tidy(india_shape, region = "States")
#india01_shape_df <- fortify(india01, region = "NAME_1") 
india01_shape_df_fortify  <- left_join(india01_shape_df,india01@data, by="id")


ggplot()+
  geom_map(data = power_total_long3, aes(map_id = States, fill = Usage),
           map = india01_shape_df) +
  expand_limits(x = india01_shape_df$long, y = india01_shape_df$lat) +
  coord_fixed(.96) +
  scale_fill_gradient(low="thistle2", high="darkred", 
                      guide="colorbar", na.value="white") +
  labs(title = "India Power Consumption (MU), 2020", x = element_blank(), y = element_blank(), 
       fill='Usage') +   theme(legend.position = "bottom") 






# fortify shape file
india <- fortify(india01, region = "GID_1")


ggplot()+
  geom_map(data = power_total_long3, aes(map_id = States, fill = Usage),
           map = india) +
  expand_limits(x = india$long, y = india$lat) +
  coord_fixed(.96) +
  scale_fill_gradient(low="thistle2", high="darkred", 
                      guide="colorbar", na.value="white") +
  labs(title = "India Power Consumption (MU), 2020", x = element_blank(), y = element_blank(), 
       fill='Usage') +   theme(legend.position = "bottom") 











#install.packages("tmap")
library(tmap)

qtm(shp = india_shape, fill = "Usage", fill.palette = "-Blue")

#################################################################

# Step3: fortify shape into dataframe
india_shape_fortify <- fortify(india_shape)

head(india_shape_fortify, n = 2)

india_shape_fortify$id <- row.names(india_shape)

india_shape_fortify <- left_join(india_shape_fortify, india_shape@data)

india_shape_fortify <- left_join(india_shape_fortify, power_total_long3, by = c("ST_NM" = "States"))



install.packages("maps")

library(maps)

us <- map_data("state")






india_shape_fortify2 <- fortify(india_shape, region = "ST_NM")


names(india_shape)

print(india_shape$ST_NM)








# FOllowing code works from youtube: 
#make data spatial
coordinates(power_total_long3) = c("longitude", "latitude")
crs.geo1 <- CRS("+proj=longlat")
proj4string(power_total_long3) = crs.geo1


plot(power_total_long3, pch = 20, col = "steelblue")

plot(india_shape)
points(power_total_long3, pch=20, col = "orange")

ggplot() +
  # polygons
  geom_polygon(data = power_total_long3, aes(x = longitude, 
                                     y = latitude, 
                                     group = States, 
                                     fill = Usage),
               color = "gray20")







  