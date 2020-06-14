# India Power Consumption during COVID-19 period
# preshit ambade
#preshitambade@gmail.com
# date: 10th June 2020
# Data source:  {https://www.kaggle.com/twinkle0705/state-wise-power-consumption-in-india}
# Animation inspiration:{https://kelseygonzalez.github.io/portfolio/COVID-19%20Population%20Mobility/}
# International energy consumption: https://medium.com/@aarushidave/electricity-consumption-during-covid-19-e6fa5ea91dbb


source("power_consum_india_v2_10June2020_part1.R")

############################################################################################
###########################GEO-SPATIAL ANALYSIS & PLOTTING##################################
############################################################################################
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
#14. in gganimate for transition_states() use {closest_state} and not other options- https://stackoverflow.com/questions/60820794/using-gganimate-and-getting-all-kind-of-errors
#15. Bug while trying to animate: https://github.com/thomasp85/gganimate/issues/81
#16. gganimate main details: https://rdrr.io/github/dgrtwo/gganimate/f/README.md
#17. plot animated plots: https://d4tagirl.com/2017/05/how-to-plot-animated-maps-with-gganimate


###############################
# A. Shape file with whole j&k
##############################
# Step1 : Load shape file, link: Plot Spatial Data / Shapefiles in R | Gun Violence in Chicago: https://www.youtube.com/watch?v=uZtto0cYjZM
india01_test <- rgdal::readOGR(dsn = "1_data/india_shape_files/india_state", layer="India-States")

#str(india_shape)

#plot(india01_test)

names(india01_test) # to know where state names are stored in the shape file

print(india01_test$ST_NM) # this shows what state names are stored

nrow(india01_test)

#########################################################################################################
library(rgeos)
rgeos::gIsValid(india01_test) #returns TRUE but it still needs pre-processing. Otehrwise gets weird results
spydf_india <- gBuffer(india01_test, byid=TRUE, width=0) # this is required for fortify with region option to work
rgeos::gIsValid(india01_test) #returns TRUE
# this is a pre-processing step before fortifying to correct/pre-process the shape file.
# without this step I get the weird results. And not able to fortify with region option
#link: https://stackoverflow.com/questions/46003060/error-using-region-argument-when-fortifying-shapefile
#link- https://gis.stackexchange.com/questions/215607/cannot-fortify-rworldmaps-default-world-map-with-a-region-argument
india01_test_f <- fortify(spydf_india, region = "ST_NM")

rm(india01_test)
rm(spydf_india)

# matching the state names in both the files
#2.1. comparing names of the states before merge
unique(india01_test_f$id %in% power_total_long3$States)

#2.2. return rows where names do not match
unique(india01_test_f$id[!india01_test_f$id %in% power_total_long3$States])

#2.3. to know unique names from both the files
unique(india01_test_f$id)
unique(power_total_long3$States)

#2.4 changing state names in fortified data
india01_test_f$id[india01_test_f$id=="Arunanchal Pradesh"]<- "Arunachal Pradesh" #correct spelling in shape file
india01_test_f$id[india01_test_f$id=="Jammu & Kashmir"]<- "Jammu and Kashmir"

#2.5. recheck by returning rows where names do not match
unique(india01_test_f$id[!india01_test_f$id %in% power_total_long3$States]) # only UTs are not matched
#india01_test$ST_NM[!india01_test$ST_NM %in% power_total_long3$States] # only UTs are not matched

#2.6. merging fortified data with original dataset. Remember left_join mactches rows from x(power_total_long3) with y(india01_test_f)
# and only includes rows from y which matches to x
final.df <- left_join(power_total_long3, india01_test_f, by = c("States"="id"))
# differences in various types of merge options: https://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
#link- https://dplyr.tidyverse.org/reference/join.html

# test dataset comment this out once you get functional animation code
#final.df <- final.df[1:100000, ]
##########################################################################################################

#########################
#1. Loading shape file
########################

# #install.packages("raster")
# library(sp)
# library(raster) # raster is dependant on sp package
# 
# india01_test <- getData('GADM', country='IND', level=1)
# 
# head(india01_test@data, n=2)
# 
# names(india01_test)
# unique(india01_test$NAME_1)
# #plot(india01_test)

###################################
#2. Steps for plotting spatial data
###################################
#Below I followed exact steps from : https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf 


# changing name of the state identifier column name to match the power dataset
#names(india01_test)[names(india01_test) == "NAME_1"] <- "States" 

# merge data with the shape file
# 
# library(dplyr)
# head(india01_test$ST_NM)
# #head(india01_test$NAME_1) # this is for GADM shape file
# head(power_total_long3$States)
# 
# #head(left_join(india01_test@data, power_total_long3, by = c("NAME_1" = "States"))) # to check how data will look after joining 
# 
# 
# india01_test@data <- (left_join(india01_test@data, power_total_long3, by = c("ST_NM" = "States"))) 
# #india01_test@data <- (left_join(india01_test@data, power_total_long3, by = c("NAME_1" = "States"))) 
# # library(tmap)
# # qtm(india01_test, "Usage")
# # qtm(shp = india01_test, fill = "Usage", fill.pallette = "-Blues")
# 
# # library(ggplot2)
# # 
# # p <- ggplot(india01_test@data, aes(Date, Usage), na.omit = TRUE)
# # p
# # p+geom_point(aes(colour = Usage))+
# #   geom_text(size = 2 , aes(label = NAME_1))
# 
# # convert spatial data into dataframe
# library(rgeos)
# india01_test_f <- fortify(india01_test)
# head(india01_test_f, n =2) # peak at the fortified data
# 
# #allocate id variable to the shape file as fortified data have id variable
# #india01_test$id <- row.names(india01_test)
# india01_test@data$id <- rownames(india01_test@data)
# 
# head(india01_test@data, n = 2)  #peak at the shape file before join
# 
# #join fortified data and data from shape file
# india01_test_f <- left_join(india01_test_f, india01_test@data) # it automatically joins by "id"
# 
# 
# usage <- left_join(power_total_long3, india01_test_f, by = c("States"="ST_NM"))


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

#detaching packages which masks gganimate
#link:https://stackoverflow.com/questions/6979917/how-to-unload-a-package-without-restarting-r

detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}

detach_package("rgeos", TRUE)

detach_package("raster", TRUE)

detach_package("sp", TRUE)

detach_package("sp", TRUE)

# above packages needs to be remove as they mask gganimate. Sequence of detaching is important as first library is dependent on
# second and third. Second is dependent on third.


# detachAllPackages(keep = NULL)


library(tidyverse)
library(gganimate)
library(ggthemes)
library(choroplethr)
library(maps)
library(htmltools)
library(mapproj)
library(viridis)
require(transformr)

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



# working code
c <- ggplot(data = final.df,
            aes(frame = Date)) +
  geom_polygon(aes(
    x = long,
    y = lat,
    group = group,
    fill = Usage
  ),
  color = "#8c8c8c") +
  theme_map() +
  coord_map("albers", #?mapproj::mapproject()
            lat0 = 30,
            lat1 = 40) +
  scale_fill_viridis(option = "magma",
                     direction = -1,
                     name = "Daily Usage(MU)",
                     guide= guide_colorbar(
                       direction = "horizontal",
                       barheight = unit(2, units = "mm"),
                       barwidth = unit(50, units = "mm"),
                       draw.ulim = F,
                       title.position = 'top',
                       title.hjust = 0.5,
                       label.hjust = 0.5
                     ), na.value="grey50") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.line=element_blank(), # borrowed from: https://stackoverflow.com/questions/6528180/ggplot2-plot-without-axes-legends-etc
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank()) +
  transition_states(
    states = Date,
    transition_length = 1.5,
    state_length = 1
  )+
  #ease_aes("linear") +
  labs(title = "State Level Power Consumption in India",
      subtitle = "on: {closest_state}", #"Week of: {frame_along}"
      caption = "Power Consumption data from Kaggle\nChart by @preshitambade")

##"Week of: {frame_along}" or other type doesn't work with transition_states()
# link- https://stackoverflow.com/questions/60820794/using-gganimate-and-getting-all-kind-of-errors
# can add view steps for transition_states()
#link- https://stackoverflow.com/questions/61041371/r-ggplot2-animation-along-date-variable-gives-error-if-i-add-view-step
# transition_state example: https://github.com/thomasp85/gganimate/issues/137
# Transition_state documentation: https://rdrr.io/github/thomasp85/gganimate/man/transition_states.html

animate(c, nframes = 2*length(unique(final.df$Date)), height = 600, width = 1000, end_pause = 10, renderer = gifski_renderer())
#nframes needs to be defined to plot all dates. Link: https://stackoverflow.com/questions/52332967/problem-with-many-50-states-in-gganimate
anim_save(filename = "3_output/power_consumption_indiamap_animate.gif") 

#some issues with animations using map:https://community.rstudio.com/t/animating-maps-issues-and-questions/55170
#

# # working code
# c <- ggplot(data = india01_test_f,
#             aes(frame = Date)) +
#   geom_polygon(aes(
#     x = long,
#     y = lat,
#     group = group,
#     fill = Usage
#   ),
#   color = "#8c8c8c") +
#   theme_map() +
#   coord_map("albers", #?mapproj::mapproject()
#   lat0 = 30,
#   lat1 = 40) +
#   scale_fill_viridis(option = "magma",
#                      direction = -1,
#                      name = "Daily Usage(MU)",
#                       guide= guide_colorbar(
#                         direction = "horizontal",
#                         barheight = unit(2, units = "mm"),
#                         barwidth = unit(50, units = "mm"),
#                         draw.ulim = F,
#                         title.position = 'top',
#                         title.hjust = 0.5,
#                         label.hjust = 0.5
#                       ), na.value="grey50") +
#   ggtitle("State Level Power Consumption in India: ") +
#   labs(caption = "Power Consumption data from Kaggle\nChart by @preshitambade") +
#   theme_classic() +
#   theme(legend.position = "bottom") +
# transition_states(
#   states = Date,
#   transition_length = 1.5,
#   state_length = 1
# )

  
# animate(c, height = 600, width = 1000, end_pause = 10, renderer = gifski_renderer())
# 
# anim_save(filename = "3_output/power_consumption_indiamap_animate.gif") 


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

###########################################################################################################################################################################################
###########################################################################################################################################################################################
# turn global warnings on
#options(warn=0)




# c <- ggplot(data = india01_test_f,
#             aes(frame = Date)) +
#   geom_polygon(aes(
#     x = long,
#     y = lat,
#     group = group,
#     fill = Usage
#   ),
#   color = "#8c8c8c") +
#   coord_map() + #"albers", #?mapproj::mapproject()
#   #lat0 = 30,
#   #lat1 = 40) +
#   theme_map() +
#   labs(x = element_blank(), 
#        y = element_blank(),
#        title = "State Level Power Consumption in India: ",
#        caption = "Power Consumption data from Kaggle\nChart by @preshitambade") +
#   theme(legend.position = "bottom") +
#   scale_fill_viridis(option = "magma",
#                      direction = -1,
#                      name = "Daily Usage(MU)",
#                      guide= guide_colorbar(
#                        direction = "horizontal",
#                        barheight = unit(2, units = "mm"),
#                        barwidth = unit(50, units = "mm"),
#                        draw.ulim = F,
#                        title.position = 'top',
#                      ), na.value="grey50") +
#   theme_classic() + 
#   transition_states(
#     states = Date,
#     transition_length = 1.5,
#     state_length = 1
#   )

#Not working
# c <- ggplot(data = india01_test_f,
#             aes(frame = Date)) +
#   geom_polygon(aes(
#     x = long,
#     y = lat,
#     group = group,
#     fill = Usage
#   ),
#   color = "#8c8c8c") +
#   theme_map() +
#     scale_fill_gradient(low = "forestgreen",
#                       high = "white") +
#   coord_map("albers", #?mapproj::mapproject()
#             lat0 = 30,
#             lat1 = 40) +
#   ggtitle("State Level Power Consumption in India",
#           subtitle = "Week of: {frame_along}") +
#   labs(caption = "Power Consumption data from Kaggle\nChart by @preshitambade") +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   transition_states(
#     states = Date,
#     transition_length = 1.5,
#     state_length = 1
#   )
# 

# c <- ggplot(data = india01_test_f,
#             aes(frame = Date)) +
#   geom_polygon(aes(
#     x = long,
#     y = lat,
#     group = group,
#     fill = Usage
#   ),
#   color = "#8c8c8c") +
#   theme_map() +
#   coord_map("albers", #?mapproj::mapproject()
#             lat0 = 30,
#             lat1 = 40) +
#   scale_fill_viridis(option = "magma",
#                      direction = -1,
#                      name = "Daily Usage(MU)",
#                      guide= guide_colorbar(
#                        direction = "horizontal",
#                        barheight = unit(2, units = "mm"),
#                        barwidth = unit(50, units = "mm"),
#                        draw.ulim = F,
#                        title.position = 'top',
#                        title.hjust = 0.5,
#                        label.hjust = 0.5
#                      ), na.value="grey50") +
#   ggtitle("State Level Power Consumption in India",
#           subtitle = "Week of: {frame_along}") +
#   labs(caption = "Power Consumption data from Kaggle\nChart by @preshitambade") +
#   theme_classic() +
#   theme(legend.position = "bottom") +
#   transition_states(
#     states = Date,
#     transition_length = 1.5,
#     state_length = 1
#   )
#     #wrap = FALSE  # got this from: https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
#   
# animate(c, height = 400, end_pause = 10)
# map <- ggplot(data = usage,
#               aes(long, lat, group = group), color = "#ffffff20") + #"#8c8c8c" color = "#ffffff20",
#   geom_polygon() +
#   coord_equal()
# 
# map








