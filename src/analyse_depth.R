# Analyse depth pattern: distance from seabed in relation to circadian and current phases
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Load packages
library(tidyverse) # To do datascience
library(lubridate)
library(MASS)
library(nlme)
library(mgcv)
library(lme4)
library("blmeco") # To calculate overdispersion of GLMM
library(coefplot2)


# 1. Read data ####
data <- read.csv("./data/interim/data_current_phases.csv")
data$ID <- factor(data$ID)
data$datetime <- ymd_hms(data$datetime)
data$night_day <- factor(data$night_day)
data$current_phase_x <- factor(data$current_phase_x)
data$current_phase_y <- factor(data$current_phase_y)

# Remove DVM data from eel A17535
data <- data[!(data$ID == "17535" & data$datetime >= '2020-01-11 00:00:00'),]

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

# Arrange data set according to tag ID and datetime, so min and max are calculated accordingly
data <-
  data %>%
  arrange(ID, datetime)


# Calculate depth relative to max depth
data_max_depth <- data %>%
  group_by(ID, Date) %>%
  summarise(max_depth = min(corrected_depth))
data <- left_join(data, data_max_depth, by = c("ID","Date"))
data$rel_depth <- data$corrected_depth / data$max_depth

# Calculate distance from seabed
data$dist_from_seabed <- data$corrected_depth - data$max_depth


# Remove NA in circadian phase
data <- data[!is.na(data$night_day),]

# Remove NA in current_phase_x and current_phase_y
data <- data[!is.na(data$current_phase_x),]
# Remove NA in circadian phase
data <- data[!is.na(data$current_phase_y),]


# Calculate summary
## Circadian phases
aggregate(data$dist_from_seabed, list(data$night_day), mean, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$night_day), sd, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$night_day), median, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$night_day), min, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$night_day), max, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$night_day, data$ID), mean, na.rm = TRUE) # per eel

## Current phases
aggregate(data$dist_from_seabed, list(data$current_phase_x), mean, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$current_phase_x), sd, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$current_phase_x), median, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$current_phase_x), min, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$current_phase_x), max, na.rm = TRUE)
aggregate(data$dist_from_seabed, list(data$current_phase_x, data$ID), mean, na.rm = TRUE) # per eel


# Create plot
boxplot <- ggplot(data, aes(x=night_day, y=dist_from_seabed)) + 
  geom_boxplot() +
  theme_minimal() +
  ylab("Distance from seabed (m)") +
  xlab("Circadian phase") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 22)) #+
#ylim(0, 20)
boxplot


# Summarise data per hour
#data$date_hour <- floor(data$datetime/3600)
data$date_hour <- lubridate::floor_date(data$datetime, "hour")  
data_summary <- data %>%
  group_by(ID, date_hour, night_day, current_phase_x, current_phase_y) %>%
  summarise(mean_depth = mean(corrected_depth),
            mean_seabed = mean(dist_from_seabed),
            mean_temp = mean(temperature),
            mean_moon = mean(moon_fraction),
            mean_sun_altitude = mean(sun_altitude),
            mean_sun_azimuth = mean(sun_azimuth))
  




# Processing steps
summary(data_summary$mean_seabed) # all values need to be > 0

## set 0 to 0.00001 to apply Gamma distribution
data_summary$mean_seabed <- if_else(data_summary$mean_seabed == 0,
                      0.00001,
                      data_summary$mean_seabed)

## Check correlation
#data_no_na <- data %>% drop_na(direction_x)
#data_no_na <- data_no_na %>% drop_na(direction_y)
#cor(data_no_na$direction_x, data_no_na$direction_y)

## Add tracking day number
#data_summary$Date <- ymd(data_summary$date_hour)
data_summary$Date <- as.Date(data_summary$date_hour)
data_summary <- data_summary %>% 
  #mutate(day_number = lubridate::ymd(Date)) %>% 
  group_by(ID) %>% 
  mutate(day_ordernumber = Date - first(Date))
data_summary$day_ordernumber <- as.numeric(data_summary$day_ordernumber) + 1



# Check data distribution
# Create qqplot with qqline
qqnorm(data_summary$mean_seabed)
qqline(data_summary$mean_seabed)



### GLMM from MASS
glm_model <- MASS::glmmPQL(mean_seabed ~  night_day + current_phase_x + current_phase_y,
                           random = ~1|ID,
                           correlation = corAR1(form = ~ 1 | ID),
                           family = Gamma(link = "log"),
                           data = data_summary, na.action = na.omit)

glm_model2 <- MASS::glmmPQL(mean_seabed ~  night_day + current_phase_x + current_phase_y +
                              night_day:current_phase_x +
                              night_day:current_phase_y,
                            random = ~1|ID/Date,
                            correlation = corAR1(form = ~ 1|ID/Date),
                            family = Gamma(link = "log"),
                            data = data_summary, na.action = na.omit)

glm_model3 <- MASS::glmmPQL(mean_seabed ~  night_day + current_phase_x + current_phase_y,
                            random = ~1|ID/Date,
                            correlation = corAR1(form = ~ 1|ID/Date),
                            family = gaussian,
                            data = data_summary, na.action = na.omit)

# Best model with easiest interpretration
# Note it is not that different from glm_model2 (Gamma log link), but easier to interpret
glm_model4 <- MASS::glmmPQL(sqrt(mean_seabed) ~  night_day + current_phase_x + current_phase_y +
                              night_day:current_phase_x +
                              night_day:current_phase_y,
                            random = ~1|ID/Date,
                            correlation = corAR1(form = ~ 1|ID/Date),
                            family = gaussian,
                            data = data_summary, na.action = na.omit)

summary(glm_model4)

# Check model
plot(glm_model4)
par(mfrow=c(2,2))
qqnorm(resid(glm_model4, type = "n"))  # type = "n"   means that the normalised residues are used; these take into account autocorrelation
hist(resid(glm_model4, type = "n"))
plot(fitted(glm_model4),resid(glm_model4, type = "n"))
dev.off()

coefplot2(glm_model4)

