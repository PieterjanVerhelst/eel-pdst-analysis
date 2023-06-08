# Analyse maximum daily migration speed relative to maximum daily current strengths
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Load packages
library(tidyverse) # To do datascience
library(lubridate)
library(MASS)
library(nlme)    # For the corAR1() functionality
#library(mgcv)
#library(lme4)
#library("blmeco") # To calculate overdispersion of GLMM
library(coefplot2)
library(ggeffects)


# 1. Read data ####
data <- read.csv("./data/interim/data_current_phases.csv")
data$ID <- factor(data$ID)
data$datetime <- ymd_hms(data$datetime)
data$night_day <- factor(data$night_day)
data$current_phase_x <- factor(data$current_phase_x)
data$current_phase_y <- factor(data$current_phase_y)
data$current_phase_p <- factor(data$current_phase_p)

# Remove DVM data from eel A17535
data <- data[!(data$ID == "17535" & data$datetime >= '2020-01-11 00:00:00'),]


# Arrange data set according to tag ID and datetime, so min and max are calculated accordingly
data <-
  data %>%
  arrange(ID, datetime)

# Check tidal signal
eel <- filter(data, ID == "16031")
eel <- filter(eel, datetime < "2019-01-05 00:00:00", datetime > "2019-01-01 00:00:00")
plot(eel$datetime, eel$corrected_depth)
plot(eel$datetime, eel$direction_x)

# Summarise max and min current strengths per day
data_west <- filter(data, current_phase_x == "westward")
data_east <- filter(data, current_phase_x == "eastward")
data_north <- filter(data, current_phase_y == "northward")
data_south <- filter(data, current_phase_y == "southward")
data_westsoutwest <- filter(data, current_phase_p == "favourable")
data_eastnortheast <- filter(data, current_phase_p == "non-favourable")


data_west_min_max <- data_west %>%
  group_by(ID, Date) %>%
  summarise(max_west = min(direction_x))

data_east_min_max <- data_east %>%
  group_by(ID, Date) %>%
  summarise(max_east = max(direction_x))

data_north_min_max <- data_north %>%
  group_by(ID, Date) %>%
  summarise(max_north = max(direction_y))

data_south_min_max <- data_south %>%
  group_by(ID, Date) %>%
  summarise(max_south = min(direction_y))

data_southwest_min_max <- data_westsoutwest %>%
  group_by(ID, Date) %>%
  summarise(max_southwest = min(p_parallel))

data_northeast_min_max <- data_eastnortheast %>%
  group_by(ID, Date) %>%
  summarise(max_northeast = max(p_parallel))


# Merge datasets
data_min_max <- left_join(data_west_min_max, data_east_min_max, by = c("ID", "Date"))
data_min_max <- left_join(data_min_max, data_north_min_max, by = c("ID", "Date"))
data_min_max <- left_join(data_min_max, data_south_min_max, by = c("ID", "Date"))
data_min_max <- left_join(data_min_max, data_southwest_min_max, by = c("ID", "Date"))
data_min_max <- left_join(data_min_max, data_northeast_min_max, by = c("ID", "Date"))


# Check data range
summary(data_min_max$max_east)
summary(data_min_max$max_west)
summary(data_min_max$max_north)
summary(data_min_max$max_south)
summary(data_min_max$max_southwest)
summary(data_min_max$max_northeast)


# Turn negative values into positive
data_min_max$max_west <- data_min_max$max_west * (-1)
data_min_max$max_south <- data_min_max$max_south * (-1)
data_min_max$max_southwest <- data_min_max$max_southwest * (-1)


# Remove redundant datasets
rm(data_west)
rm(data_east)
rm(data_north)
rm(data_south)
rm(data_westsoutwest)
rm(data_east)

rm(data_west_min_max)
rm(data_east_min_max)
rm(data_north_min_max)
rm(data_south_min_max)
rm(data_southwest_min_max)
rm(data_northeast_min_max)


# 10. Load trajectory dataset with coordinates ####
#tr_data <-  list.files(path = "./data/external/trajectory_data/",
#                       pattern = "*.csv", 
#                       full.names = T) %>% 
#  map_df(~read_csv(., col_types = cols(.default = "c"))) 

tr_data <- read_csv("./data/external/trajectory_data/eel_trajectories.csv")


# Select columns
tr_data <- dplyr::select(tr_data, ID, Date, MPL.Hori_Dist_km)
tr_data <- rename(tr_data, 
                  km_day = MPL.Hori_Dist_km)

# Process columns
tr_data$Date <- dmy(tr_data$Date)
tr_data$ID <- factor(tr_data$ID)

# Remove double dates per eel (ID)
#tr_data <- tr_data[!duplicated(tr_data[c('ID','Date')]),]
tr_data <- tr_data %>%     # Add ID number to duplicate dates
  group_by(ID, Date) %>%
  add_tally()

duplicates <- filter(tr_data, n == 2)   # Filter duplicate dates
duplicates <- duplicates %>%             # Add ID number to distinguish between first and second duplicate
  mutate(number_id = row_number())
duplicates <- filter(duplicates, number_id == 2)  # Filter second duplicates

tr_data <- filter(tr_data, n != 2)   # Remove duplicate dates from tracking dataset

# Bind 'duplicates' dataset with second duplicates to tracking dataset
tr_data <- ungroup(tr_data)
tr_data$n <- NULL
duplicates <- ungroup(duplicates)
duplicates$n <- NULL
duplicates$number_id <- NULL

tr_data <- rbind(tr_data, duplicates)


# Select relevant eels
tr_data <- filter(tr_data, ID == "15805" |
                    ID == "15730" |
                    ID == "15757" |
                    ID == "15700" |
                    ID == "15714" |
                    ID == "16031" |
                    ID == "15706" |
                    ID == "15981" |
                    ID == "15777" |
                    ID == "17443" |
                    ID == "17499" |
                    ID == "17513" |
                    ID == "17534" |
                    ID == "17526" |
                    ID == "17522" |
                    ID == "174922" |
                    ID == "17508" |
                    ID == "17536" |
                    ID == "17538" |
                    ID == "17537" |
                    ID == "17510" |
                    ID == "15789" |
                    ID == "17521" |
                    ID == "17535" |
                    ID == "17653" |
                    ID == "157302" |
                    ID == "157002" |
                    ID == "17646" |
                    ID == "17642" |
                    ID == "17658" |
                    ID == "175252" |
                    ID == "174922021" |
                    ID == "175182" |
                    ID == "17638" |
                    ID == "17634" |
                    ID == "17547" |
                    ID == "17635" |
                    ID == "17487" |
                    ID == "174992" |
                    ID == "17663" |
                    ID == "175132" |
                    ID == "17648" )
tr_data$ID <- factor(tr_data$ID) # rerun 'factor()' so the number of levels is set accurately

# Change IDs of some eels/PDSTs
tr_data$ID <- plyr::revalue(tr_data$ID, 
                            c("175132"="17513_2", 
                              "157002"="15700_2",
                              "174922"="17492",
                              "157302"="15730_2",
                              "175252"="17525_2",
                              "174922021"="17492_2",
                              "174992"="17499_2",
                              "175182"="17518_2"))



# Merge datasets ####
data_min_max$Date <- ymd(data_min_max$Date)
data_min_max <- left_join(x = data_min_max, y = tr_data, by=c("ID","Date"))




# Filter relevant eels
# Nordic eels
data_nordic <- filter(data_min_max, ID == "15805" |
                 ID == "15981" |
                 ID == "17492_2" |
                 ID == "17499" |
                 ID == "17525_2")

# Channel eels
data_channel <- filter(data_min_max, ID != "15805" ,
               ID != "15981" ,
               ID != "17492_2" ,
               ID != "17499" ,
               ID != "17525_2")

# Summary of the eel's speed
summary(data_nordic$km_day)
sd(data_nordic$km_day)
summary(data_channel$km_day)
sd(data_channel$km_day, na.rm = T)

summary_nordic <- data_nordic %>%
  group_by(ID) %>%
  summarise(mean = mean(km_day),
          sd = sd(km_day))

data_channel_no_na <- na.omit(data_channel)
summary_channel <- data_channel_no_na %>%
  group_by(ID) %>%
  summarise(mean = mean(km_day),
            sd = sd(km_day))

# Check correlation
cor.test(data_min_max$max_west, data_min_max$max_east)
cor.test(data_min_max$max_north, data_min_max$max_south)


# Run LMM for Channel eels
glm_model1 <- lme(km_day ~  max_southwest,
                  random = ~1|ID/Date,
                  correlation = corAR1(form = ~ 1|ID/Date),
                  data = data_channel, na.action = na.omit)

# Run LMM for Nordic eels
glm_model1 <- lme(log(km_day) ~ max_north,
                  random = ~1|ID/Date,
                  correlation = corAR1(form = ~ 1|ID/Date),
                  data = data_nordic, na.action = na.omit)


summary(glm_model1)


# Check model
plot(glm_model1)
par(mfrow=c(2,2))
qqnorm(resid(glm_model1, type = "n"))  # type = "n"   means that the normalised residues are used; these take into account autocorrelation
hist(resid(glm_model1, type = "n"))
plot(fitted(glm_model1),resid(glm_model1, type = "n"))
dev.off()

coefplot2(glm_model1)


par(mfrow=c(2,2))
plot(data_channel$km_day, data_channel$max_west)
plot(data_channel$km_day, data_channel$max_east)
plot(data_channel$km_day, data_channel$max_north)
plot(data_channel$km_day, data_channel$max_south)

