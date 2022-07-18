# Create an actogram to visualise the vertical activity pattern of eels
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be
# Base code by Chris Griffiths (christopher.griffiths@slu.se)


# Load packages 
library(tidyverse)
require(lubridate)
require(ggpubr)
require(viridis)


# 1. Create actogram based on depth data ####

# Load data
data <- read_csv("./data/interim/data_circadian_tidal_moon_sun_5min.csv",
                 na = "", 
                 col_types = list(sunrise = col_datetime(),
                                  previous_sunset = col_datetime(),
                                  next_sunrise = col_datetime(),
                                  next_sunmoment = col_datetime(),
                                  U = col_double(),
                                  V = col_double(),
                                  speed = col_double(),
                                  direction = col_double()),          # set direction as numeric
                 guess_max = 100000)

data$...1 <- NULL
data$ID <- factor(data$ID)

# Select 1 eel
data_1eel <- filter(data, ID == "16031")

# Arrange data set according datetime
data_1eel <-
  data_1eel %>%
  arrange(datetime)

# Calculate depth relative to max depth
data_max_depth <- data_1eel %>%
  group_by(ID, Date) %>%
  summarise(max_depth = min(corrected_depth))
data_1eel <- left_join(data_1eel, data_max_depth, by = c("ID","Date"))
data_1eel$rel_depth <- data_1eel$corrected_depth / data_1eel$max_depth

# Calculate distance from seabed
data_1eel$dist_from_seabed <- data_1eel$corrected_depth - data_1eel$max_depth


# Add month label option for plots
data_1eel$month_abb <- month(data_1eel$datetime, label = TRUE, abbr = TRUE) 

# Extract hours of the day (0 - 24)
data_1eel$hour <- hour(data_1eel$datetime) 

# Extract year day (0-365)
data_1eel$yday <- yday(as.Date(data_1eel$datetime,"%Y-%m-%d")) 

data_1eel$datehour <- lubridate::floor_date(data_1eel$datetime, "hour")  
data_1eel$numericdatehour <- as.numeric(data_1eel$datehour)              
data_1eel$day_number <- as.numeric(data_1eel$Date)

# Create duplicate for double plot actogram
data_1eel2 <- data_1eel
data_1eel2 <- filter(data_1eel2, datehour > "2018-12-10 00:00:00")
data_1eel2$hour <- 24+(data_1eel2$hour)
data_1eel2$day_number <- data_1eel2$day_number -1

data_1eel <- rbind(data_1eel, data_1eel2)

# Just for visualisation purpose, add +1 hour
data_1eel$hour <- 1+(data_1eel$hour)

# Remove the single record at 2019-02-13 00:00:00 which results in a single cell on top of the plot
data_1eel <- filter(data_1eel, day_number != "17940")


#data_1eel2 <- filter(data_1eel, day_number < "17920")


# Create actogram
a1 <- ggplot(data_1eel_summary, aes(x=as.factor(hour), y=day_number, fill = average_depth))+ # where time is hours of the day (so, 0 to 24)
  geom_tile()+
  coord_equal() +
  scale_fill_viridis(discrete=FALSE, name = 'Frequency of activity', option = 'viridis')+
  ylab('day of year')+
  xlab('hour of day')+
  ylim(17870, 17940) +
  theme_bw()
a1





# 2. Create actogram based on depth difference between subsequent measurements ####

# Load data
data <- read_csv("./data/interim/data_circadian_tidal_moon_sun_5min.csv",
                 na = "", 
                 col_types = list(sunrise = col_datetime(),
                                  previous_sunset = col_datetime(),
                                  next_sunrise = col_datetime(),
                                  next_sunmoment = col_datetime(),
                                  U = col_double(),
                                  V = col_double(),
                                  speed = col_double(),
                                  direction = col_double()),          # set direction as numeric
                 guess_max = 100000)

data$...1 <- NULL
data$ID <- factor(data$ID)

# Select 1 eel
data_1eel <- filter(data, ID == "16031")

# Arrange data set according datetime
data_1eel <-
  data_1eel %>%
  arrange(datetime)

# Calculate rate of up and down vertical movement
activity_threshold <- 2.5
data_1eel$rate <- ((abs(data_1eel$corrected_depth - lag(data_1eel$corrected_depth, 1)))/60)*100
data_1eel <- data_1eel %>%
  mutate(active = if_else(data_1eel$rate > activity_threshold,
                          1,
                          0))

data_1eel$datehour <- lubridate::floor_date(data_1eel$datetime, "hour")         

data_1eel <- data_1eel %>%
  group_by(datehour) %>%
  summarise(frequency = sum(active))

data_1eel$numericdate <- as.numeric(data_1eel$datehour)   
data_1eel$hour <- hour(data_1eel$datehour) # extract hours of the day (0 - 24)
data_1eel$Date <- as.Date(data_1eel$datehour)
data_1eel$day_number <- as.numeric(data_1eel$Date)

# Create duplicate for double plot actogram
data_1eel2 <- data_1eel
data_1eel2 <- filter(data_1eel2, datehour > "2018-12-10 00:00:00")
data_1eel2$hour <- 24+(data_1eel2$hour)
data_1eel2$day_number <- data_1eel2$day_number -1

data_1eel <- rbind(data_1eel, data_1eel2)

# Just for visualisation purpose, add +1 hour
data_1eel$hour <- 1+(data_1eel$hour)

# Remove the single record at 2019-02-13 00:00:00 which results in a single cell on top of the plot
data_1eel <- filter(data_1eel, day_number != "17940")

# Create actogram
a2 <- ggplot(data_1eel, aes(x=as.factor(hour), y=day_number, fill = frequency))+ # where time is hours of the day (so, 0 to 24)
  geom_tile()+
  coord_equal() +
  scale_fill_viridis(discrete=FALSE, name = 'Frequency of activity', option = 'viridis')+
  ylab('day of year')+
  xlab('hour of day')+
  ylim(17870, 17940) +
  theme_bw()
a2





# 3. Create actogram based on depth amplitude ####

# Load data
data <- read_csv("./data/interim/data_depth_diff.csv",
                 na = "", 
                 guess_max = 100000)

data$...1 <- NULL
data$ID <- factor(data$ID)

# Select 1 eel
data_1eel <- filter(data, ID == "16031")

# Arrange data set according datetime
data_1eel <-
  data_1eel %>%
  arrange(datetime)

# Add month label option for plots
data_1eel$month_abb <- month(data_1eel$datetime, label = TRUE, abbr = TRUE) 

# Extract hours of the day (0 - 24)
data_1eel$hour <- hour(data_1eel$datetime) 

# Extract year day (0-365)
data_1eel$yday <- yday(as.Date(data_1eel$datetime,"%Y-%m-%d")) 

data_1eel$datehour <- lubridate::floor_date(data_1eel$datetime, "hour")  
data_1eel$numericdatehour <- as.numeric(data_1eel$datehour)              
data_1eel$day_number <- as.numeric(data_1eel$Date)

# Create duplicate for double plot actogram
data_1eel2 <- data_1eel
data_1eel2 <- filter(data_1eel2, datehour > "2018-12-10 00:00:00")
data_1eel2$hour <- 24+(data_1eel2$hour)
data_1eel2$day_number <- data_1eel2$day_number -1

data_1eel <- rbind(data_1eel, data_1eel2)

# Just for visualisation purpose, add +1 hour
data_1eel$hour <- 1+(data_1eel$hour)

# Remove the single record at 2019-02-13 00:00:00 which results in a single cell on top of the plot
data_1eel <- filter(data_1eel, day_number != "17940")

# Set as numeric
data_1eel$depth_change <- as.numeric(data_1eel$depth_change)

# Create actogram
a3 <- ggplot(data_1eel, aes(x=as.factor(hour), y=day_number, fill = depth_change))+ # where time is hours of the day (so, 0 to 24)
  geom_tile()+
  coord_equal() +
  scale_fill_viridis(discrete=FALSE, name = 'Frequency of activity', option = 'viridis')+
  ylab('day of year')+
  xlab('hour of day')+
  ylim(17870, 17940) +
  theme_bw()
a3






# 4. Create actogram based on depth data using summaries ####

# Load data
data <- read_csv("./data/interim/data_circadian_tidal_moon_sun_5min.csv",
                 na = "", 
                 col_types = list(sunrise = col_datetime(),
                                  previous_sunset = col_datetime(),
                                  next_sunrise = col_datetime(),
                                  next_sunmoment = col_datetime(),
                                  U = col_double(),
                                  V = col_double(),
                                  speed = col_double(),
                                  direction = col_double()),          # set direction as numeric
                 guess_max = 100000)

data$...1 <- NULL
data$ID <- factor(data$ID)

# Select 1 eel
data_1eel <- filter(data, ID == "16031")

# Arrange data set according datetime
data_1eel <-
  data_1eel %>%
  arrange(datetime)

# Calculate depth relative to max depth
data_max_depth <- data_1eel %>%
  group_by(ID, Date) %>%
  summarise(max_depth = min(corrected_depth))
data_1eel <- left_join(data_1eel, data_max_depth, by = c("ID","Date"))
data_1eel$rel_depth <- data_1eel$corrected_depth / data_1eel$max_depth

# Calculate distance from seabed
data_1eel$dist_from_seabed <- data_1eel$corrected_depth - data_1eel$max_depth

# 1 hour resolution
data_1eel$datehour <- lubridate::floor_date(data_1eel$datetime, "hour") 

# Calculate summary by grouping
data_1eel_summary <- data_1eel %>%
  group_by(datehour) %>%
  summarise(average_depth = mean(corrected_depth),
            max_depth = min(corrected_depth),
            average_dist_from_seabed = mean(dist_from_seabed),
            max_dist_from_seabed = max(dist_from_seabed))


data_1eel_summary$numericdate <- as.numeric(data_1eel_summary$datehour)   
data_1eel_summary$hour <- hour(data_1eel_summary$datehour) # extract hours of the day (0 - 24)
data_1eel_summary$Date <- as.Date(data_1eel_summary$datehour)
data_1eel_summary$day_number <- as.numeric(data_1eel_summary$Date)

# Create duplicate for double plot actogram
data_1eel2 <- data_1eel_summary
#data_1eel2 <- filter(data_1eel2, datehour > "2018-12-10 00:00:00")
data_1eel2 <- filter(data_1eel2, day_number > 17874)
data_1eel2$hour <- 24+(data_1eel2$hour)
data_1eel2$day_number <- data_1eel2$day_number -1

data_1eel_summary <- rbind(data_1eel_summary, data_1eel2)

# Just for visualisation purpose, add +1 hour
data_1eel_summary$hour <- 1+(data_1eel_summary$hour)

# Remove the single record at 2019-02-13 00:00:00 which results in a single cell on top of the plot
data_1eel_summary <- filter(data_1eel_summary, day_number != "17940")

# Create actogram
a4 <- ggplot(data_1eel_summary, aes(x=as.factor(hour), y=day_number, fill = average_depth))+ # where time is hours of the day (so, 0 to 24)
  geom_tile()+
  coord_equal() +
  scale_fill_viridis(discrete=FALSE, name = 'Frequency of activity', option = 'viridis')+
  ylab('day of year')+
  xlab('hour of day')+
  ylim(17870, 17940) +
  theme_bw()
a4



# 5. Create actogram based on depth data using summaries with 15 min resolution ####

# Load data
data <- read_csv("./data/interim/data_circadian_tidal_moon_sun_5min.csv",
                 na = "", 
                 col_types = list(sunrise = col_datetime(),
                                  previous_sunset = col_datetime(),
                                  next_sunrise = col_datetime(),
                                  next_sunmoment = col_datetime(),
                                  U = col_double(),
                                  V = col_double(),
                                  speed = col_double(),
                                  direction = col_double()),          # set direction as numeric
                 guess_max = 100000)

data$...1 <- NULL
data$ID <- factor(data$ID)

# Select 1 eel
data_1eel <- filter(data, ID == "15981")

# Arrange data set according datetime
data_1eel <-
  data_1eel %>%
  arrange(datetime)

# Calculate depth relative to max depth
data_max_depth <- data_1eel %>%
  group_by(ID, Date) %>%
  summarise(max_depth = min(corrected_depth))
data_1eel <- left_join(data_1eel, data_max_depth, by = c("ID","Date"))
data_1eel$rel_depth <- data_1eel$corrected_depth / data_1eel$max_depth

# Calculate distance from seabed
data_1eel$dist_from_seabed <- data_1eel$corrected_depth - data_1eel$max_depth

# Classify behaviour from seabed
data_1eel$activity <- ifelse(data_1eel$dist_from_seabed >= 10, 1, 0)

# 15 min resolution
data_1eel$datequarter <- lubridate::floor_date(data_1eel$datetime, "15 min")

# Calculate summary by grouping
data_1eel_summary <- data_1eel %>%
  group_by(datequarter) %>%
  summarise(average_depth = mean(corrected_depth),
            max_depth = min(corrected_depth),
            average_dist_from_seabed = mean(dist_from_seabed),
            max_dist_from_seabed = max(dist_from_seabed),
            average_temp = mean(temperature),
            max_temp = max(temperature),
            total_activity = sum(activity))

data_1eel_summary$numericdate <- as.numeric(data_1eel_summary$datequarter)   
data_1eel_summary$quarter <- sub(".*? ", "", data_1eel_summary$datequarter)   # extract quarters of the day
data_1eel_summary$Date <- as.Date(data_1eel_summary$datequarter)
data_1eel_summary$day_number <- as.numeric(data_1eel_summary$Date)
class(data_1eel_summary$quarter)
data_1eel_summary$fquarter <- factor(data_1eel_summary$quarter)
data_1eel_summary$quarter_numeric <- as.numeric(data_1eel_summary$fquarter)


# Create duplicate for double plot actogram
data_1eel2 <- data_1eel_summary
#data_1eel2 <- filter(data_1eel2, datehour > "2018-12-10 00:00:00")
#data_1eel2 <- filter(data_1eel2, day_number > 17874) # example for eel A16031; in next line write code more generally applicable
data_1eel2 <- filter(data_1eel2, day_number > min(day_number))
data_1eel2$quarter_numeric <- 96+(data_1eel2$quarter_numeric)   # add a day (24 hours = 96 quarters = 24 hour * 4 quarters in an hour)
data_1eel2$day_number <- data_1eel2$day_number -1

data_1eel_summary <- rbind(data_1eel_summary, data_1eel2)

# Just for visualisation purpose, add +1 hour
#data_1eel_summary$hour <- 1+(data_1eel_summary$hour)

# Remove the single record at 2019-02-13 00:00:00 which results in a single cell on top of the plot
#data_1eel_summary <- filter(data_1eel_summary, day_number != "17940") # example for eel A16031; in next line write code more generally applicable
data_1eel_summary <- filter(data_1eel_summary, day_number != max(day_number))

# Create actogram
png(file="./additionals/Figures/actograms/A15981_activity.png",
    width=1000, height=400)

#a5 <- ggplot(data_1eel_summary, aes(x=as.factor(quarter_numeric), y=day_number, fill = total_activity))+
a5 <- ggplot(data_1eel_summary, aes(x=quarter_numeric, y=day_number, fill = total_activity))+ # where time is quarter of the day (so, 0 to 96, times 2)
  geom_tile()+
  #coord_equal() +
  scale_fill_viridis(discrete=FALSE, name = 'Frequency of activity', option = 'viridis')+
  ylab('day of year')+
  xlab('quarter of day')+
  #ylim(17870, 17940) +
  theme_bw() +  
  theme(axis.text = element_text(size = 14),
                      axis.title = element_text(size = 16))
a5

dev.off()



# 6. Create actogram based on depth data using summaries with 15 min resolution for all eels ####

# Load data
data <- read_csv("./data/interim/data_circadian_tidal_moon_sun_5min.csv",
                 na = "", 
                 col_types = list(sunrise = col_datetime(),
                                  previous_sunset = col_datetime(),
                                  next_sunrise = col_datetime(),
                                  next_sunmoment = col_datetime(),
                                  U = col_double(),
                                  V = col_double(),
                                  speed = col_double(),
                                  direction = col_double()),          # set direction as numeric
                 guess_max = 100000)

data$...1 <- NULL
data$ID <- factor(data$ID)


# Nordic eels
data <- filter(data, ID == "15805" |
                  ID == "15981" |
                  ID == "17492_2" |
                  ID == "17499" |
                  ID == "17525_2")

# Channel eels
data <- filter(data, ID != "15805" ,
                 ID != "15981" ,
                 ID != "17492_2" ,
                 ID != "17499" ,
                 ID != "17525_2")


# Arrange data set according to ID and datetime
data <- data %>% 
  arrange(ID, datetime)

# Calculate depth relative to max depth
data_max_depth <- data %>%
  group_by(ID, Date) %>%
  summarise(max_depth = min(corrected_depth))
data <- left_join(data, data_max_depth, by = c("ID","Date"))
data$rel_depth <- data$corrected_depth / data$max_depth

# Calculate distance from seabed
data$dist_from_seabed <- data$corrected_depth - data$max_depth

# Classify behaviour from seabed
data$activity <- ifelse(data$dist_from_seabed >= 10, 1, 0)

# 15 min resolution
data$datequarter <- lubridate::floor_date(data$datetime, "15 min")

# Calculate summary by grouping
data_summary <- data %>%
  group_by(ID, datequarter) %>%
  summarise(average_depth = mean(corrected_depth),
            max_depth = min(corrected_depth),
            average_dist_from_seabed = mean(dist_from_seabed),
            max_dist_from_seabed = max(dist_from_seabed),
            average_temp = mean(temperature),
            max_temp = max(temperature),
            total_activity = sum(activity))

data_summary$numericdate <- as.numeric(data_summary$datequarter)   
data_summary$quarter <- sub(".*? ", "", data_summary$datequarter)   # extract quarters of the day
data_summary$Date <- as.Date(data_summary$datequarter)
data_summary$day_number <- as.numeric(data_summary$Date)
class(data_summary$quarter)
data_summary$fquarter <- factor(data_summary$quarter)
data_summary$quarter_numeric <- as.numeric(data_summary$fquarter)

# Add record of number of tracked days
data_summary <- data_summary %>% 
  #mutate(day_number = lubridate::ymd(Date)) %>% 
  group_by(ID) %>% 
  mutate(day_ordernumber = Date - first(Date))

data_summary$day_ordernumber <- as.numeric(data_summary$day_ordernumber) + 2 # + 2 to remove the 0 and be 1 day ahead of the duplicate


# Create duplicate for double plot actogram
data2 <- data_summary
#data_1eel2 <- filter(data_1eel2, datehour > "2018-12-10 00:00:00")
#data_1eel2 <- filter(data_1eel2, day_number > 17874) # example for eel A16031; in next line write code more generally applicable
data2 <- data2 %>%
  group_by(ID) %>%
  filter(day_number > min(day_number))
data2$quarter_numeric <- 96+(data2$quarter_numeric)   # add a day (24 hours = 96 quarters = 24 hour * 4 quarters in an hour)
data2$day_number <- data2$day_number -1
data2$day_ordernumber <- data2$day_ordernumber -1 # - 1 so eventually the day_ordernummer of the duplicate is one day lagging the original

data_summary <- rbind(data_summary, data2)

# Just for visualisation purpose, add +1 hour
#data_1eel_summary$hour <- 1+(data_1eel_summary$hour)

# Remove the single record at 2019-02-13 00:00:00 which results in a single cell on top of the plot
#data_1eel_summary <- filter(data_1eel_summary, day_number != "17940") # example for eel A16031; in next line write code more generally applicable
data_summary <- filter(data_summary, day_number != max(day_number))

# Create actogram
png(file="./additionals/Figures/actograms/channel_activity.png",
    width=1000, height=400)

#a5 <- ggplot(data_1eel_summary, aes(x=as.factor(quarter_numeric), y=day_number, fill = total_activity))+
a6 <- ggplot(data_summary, aes(x=quarter_numeric, y=day_ordernumber, fill = total_activity))+ # where time is quarter of the day (so, 0 to 96, times 2)
  geom_tile()+
  #coord_equal() +
  scale_fill_viridis(discrete=FALSE, name = 'Frequency of activity', option = 'viridis')+
  ylab('Post-release days')+
  xlab('Quarter of day')+
  #ylim(17870, 17940) +
  theme_bw() +  
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
a6

dev.off()



