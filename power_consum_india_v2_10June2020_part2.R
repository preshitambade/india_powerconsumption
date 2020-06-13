# India Power Consumption during COVID-19 period
# preshit ambade
#preshitambade@gmail.com
# date: 10th June 2020
# Data source:  {https://www.kaggle.com/twinkle0705/state-wise-power-consumption-in-india}
# Animation inspiration:{https://kelseygonzalez.github.io/portfolio/COVID-19%20Population%20Mobility/}
# International energy consumption: https://medium.com/@aarushidave/electricity-consumption-during-covid-19-e6fa5ea91dbb


source("power_consum_india_v2_10June2020_part1.R")

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
