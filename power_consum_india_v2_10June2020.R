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

#power_total_long3$States <- as.character(power_total_long3$States)

#check dataset
glimpse(power_total_long3)
summary(power_total_long3)
unique(power_total_long3$Date)


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

# power_baseplot3 <- power_baseplot2+
#   labs(title = "Daily Power Consumption by states in India",
#        subtitle = "Date: {frame_time}") + 
#   transition_time(Date) +
#   shadow_wake(wake_length = 0.2)
# 
# power_baseplot3


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

# ############################################
# #5. Creating Static Plot for selected states
# ############################################
# 
# # Just plot the graph without animation for selected states
# power_total_long2 <- power_total_long %>%
#   filter(States =="Tamil Nadu" | States == "Andhra Pradesh" | States =="Karnataka" | #"Telangana", - telangana data is inconsistent so excluded
#            States =="Maharashtra" | States =="Delhi" | States =="Gujarat") %>% 
#   select(Date, States, Usage, lab)
# 
# 
# b <- ggplot(power_total_long2, aes(x = Date,
#                                   y = Usage,
#                                   colour = lab)) +
#   geom_line(position = "identity") +
#   geom_vline(xintercept = lock1startdate, col = "red", lty = 1) + #add vertical lines for lockdown dates
#   geom_vline(xintercept = lock1enddate, col = "green", lty = 1) +
#   geom_vline(xintercept = lock2enddate, col = "orange", lty = 1) +
#   geom_vline(xintercept = lock3enddate, col = "black", lty = 1) +
#   xlab("") +
#   ylab("Daily Power Consumed in Mega Units (MU)") +
#   scale_x_date(date_breaks = "1 week",
#                date_labels = "%m-%d") +
#   theme(legend.position = "right", axis.text.x = element_text(angle = 90))+
#   ggtitle("Daily Power Consumption by Selected States in India \n(28 October 2019 to 23 May 2020)") +
#   labs(caption = "(based on data from weekly energey reports published by Power System Operation Corporation Limited (POSOCO))") # this adds caption at bottom right corner
# b
# 
# ggsave(filename = "3_output/power_consumption_state_india_static.tiff")

############################################################################################
###########################GEO-SPATIAL ANALYSIS & PLOTTING##################################
############################################################################################

# Steps to follow to download shape files for India:
#1. go to GADM Website and search India: https://gadm.org/download_country_v3.html
#2. download R(sp)/ R(sf)/ Geopackage files for level0(statelevel) which can be open in R
# Discription of these file types is given at: https://gadm.org/formats.html

#Alternatively I can open shape file using rgdal::readOGR function
# The state level shape file is downloaded from forked GitHub repository: 
# HindustanTimesLabs/shapefiles : https://github.com/HindustanTimesLabs/shapefiles

# working with spatial data guide: https://cmerow.github.io/RDataScience/04_Spatial.html

##############################################################################
# useful links to follow on how to plot data merging shape file and dataframe:
##############################################################################
#1. India shape file loading issues: https://stackoverflow.com/questions/47617480/why-is-plot-of-gadm-spatialpolygonsdataframe-not-loading-in-r
#2. GADM website for shape files: https://gadm.org/index.html
#3. how to call GADM shape file directly in R: https://www.researchgate.net/post/Can_someone_help_me_to_find_a_shape_file_for_Indian_districts
#4. Test shape files by dragging to this website: https://mapshaper.org
#5. Geospatial Data analysis in R Tutorial: http://mazamascience.com/WorkingWithData/?p=1277
#6. How to join shape file and dataframe for ggplot: https://gis.stackexchange.com/questions/110183/join-csv-file-to-shapefile
#7. How to keep information in shape file after fortify() : https://stackoverflow.com/questions/22096787/how-keep-information-from-shapefile-after-fortify
#8. Introduction to GIS using R: Tutorial: https://mgimond.github.io/Spatial/introGIS.html
#9. Introduction to visualising spatial data in R:https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
#10. Using spatial data in R: tutorial: https://cengel.github.io/R-spatial/
#11. Beautiful thematic maps with ggplot2 (only) : https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
#12. Animating maps with  gganimate: https://ditheringdata.netlify.app/2018/01/01/gganimate/
#13. Polygons from a reference map: https://ggplot2.tidyverse.org/reference/geom_map.html

#########################
#1. Loading shape file
########################

#install.packages("raster")
library(sp)
library(raster) # raster is dependant on sp package

india01_test <- getData('GADM', country='IND', level=1)

head(india01_test@data, n=2)

names(india01_test)
unique(india01_test$NAME_1)
#plot(india01_test)

###################################
#2. Steps for plotting spatial data
###################################
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

#############################
#3. Animate the spatial data
#############################
#sources:
#1. https://kelseygonzalez.github.io/portfolio/COVID-19%20Population%20Mobility/
#2. https://ditheringdata.netlify.app/2018/01/01/gganimate/
#3. https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/


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


c <- ggplot(data = india01_test_f,
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
  coord_map() + #"albers", #?mapproj::mapproject()
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

library(gganimate)

animate(c, height = 600, width = 1000, end_pause = 10, renderer = gifski_renderer())

anim_save(filename = "3_output/power_consumption_indiamap_animate.gif") 


######################
#4. Combine Animations
######################

# code to combine gifs from https://github.com/thomasp85/gganimate/wiki/Animation-Composition
a_mgif <- magick::image_read("3_output/power_consumption_state_india_animate.gif")
c_mgif <- magick::image_read("3_output/power_consumption_indiamap_animate.gif")
new_gif <- magick::image_append(c(a_mgif[1], c_mgif[1]))
for (i in 2:100) {
  combined <- magick::image_append(c(a_mgif[i], c_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif
# anim_save("together.gif")

anim_save(filename = "3_output/combined_animate.gif") 


# turn global warnings on
#options(warn=0)











