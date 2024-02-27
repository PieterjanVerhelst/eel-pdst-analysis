# Explore DVM pattern in eels 
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be


# Packages
library(tidyverse)
library(lubridate)
library(broom) # run regression per grouping variable (in 'Correct for depth drift')
library(zoo)


# 1. Load data ####
eel_A16031 <- read_csv("./data/interim/sensorlogs/sensor_A16031_08-11-2019.csv")
#eel_A17492 <- read_csv("./data/interim/sensorlogs/sensor_A17492_05-05-2020.csv")  # Only 1 day with DVM
eel_A17537 <- read_csv("./data/interim/sensorlogs/sensor_A17537_05-05-2020.csv")
eel_A17510 <- read_csv("./data/interim/sensorlogs/sensor_A17510_22-06-2020.csv")
eel_A15789 <- read_csv("./data/interim/sensorlogs/sensor_A15789_22-06-2020.csv")
eel_A17535 <- read_csv("./data/interim/sensorlogs/sensor_A17535_09-09-2020.csv")
#eel_A15730_2 <- read_csv("./data/interim/sensorlogs/sensor_A15730_09-03-2021.csv")  # Only 1 day with DVM
eel_A15730_2$track_tag_id <- "A15730_2"
eel_A15700_2 <- read_csv("./data/interim/sensorlogs/sensor_A15700_09-03-2021.csv")
eel_A15700_2$track_tag_id <- "A15700_2"
#eel_A17518_2 <- read_csv("./data/interim/sensorlogs/sensor_A17518_11-03-2021.csv")  # Only 1 day with DVM
eel_A17518_2$track_tag_id <- "A17518_2"
eel_A17487 <- read_csv("./data/interim/sensorlogs/sensor_A17487_15-04-2021.csv")
eel_A17648 <- read_csv("./data/interim/sensorlogs/sensor_A17648_25-06-2021.csv")

eel_A17463 <- read_csv("./data/interim/sensorlogs/sensor_A17463_09-10-2020.csv") # Danish eel
eel_A17471 <- read_csv("./data/interim/sensorlogs/sensor_A17471_08-10-2020.csv") # Danish eel
eel_A17476 <- read_csv("./data/interim/sensorlogs/sensor_A17476_09-03-2021.csv") # Danish eel

# Combine all datasets ####
data <- do.call("rbind", list(eel_A16031,
                             #eel_A17492,
                             eel_A17537,
                             eel_A17510,
                             eel_A15789,
                             eel_A17535,
                             #eel_A15730_2,
                             eel_A15700_2,
                             #eel_A17518_2,
                             eel_A17487,
                             eel_A17648,
                             eel_A17463,
                             eel_A17471,
                             eel_A17476
))

data <- data %>%
  rename(ID = track_tag_id)
data$ID <- factor(data$ID)

# Remove seperate files
rm(eel_A16031,
   #eel_A17492,
   eel_A17537,
   eel_A17510,
   eel_A15789,
   eel_A17535,
   #eel_A15730_2,
   eel_A15700_2,
   #eel_A17518_2,
   eel_A17487,
   eel_A17648,
   eel_A17463,
   eel_A17471,
   eel_A17476)


# 2. Read in parameter file ####
parameters <- read_csv("./data/external/parameters.csv")
parameters$start_datetime <-  dmy_hm(parameters$start_datetime)
parameters$end_datetime <-  dmy_hm(parameters$end_datetime)
parameters$bank_datetime <-  dmy_hm(parameters$bank_datetime)
parameters$popoff_datetime <-  dmy_hm(parameters$popoff_datetime)
#parameters$popoff_datetime15 <-  parameters$popoff_datetime + minutes(15)
parameters$UTC <-  factor(parameters$UTC)


# 3. Time zone correction ####
#all <- left_join(all, parameters, by = "ID") %>%
#  mutate(datetime = ifelse(UTC == "-1", (datetime - (60*60)), 
#                            ifelse(UTC == "-2", datetime - (2*60*60))))
#all$datetime <- as.POSIXct(all$datetime, origin='1970-01-01 00:00:00')
#all$time_diff <- all$datetime2 - all$datetime    # Check for time zone correction

# All Belgian eels in same time zone, so simply datetime - 1 hour for all data
data$datetime <- dmy_hms(data$datetime)
data$datetime_local_zone <- data$datetime
data$datetime <- data$datetime - 3600

# Check for time zone correction
data$time_diff <- data$datetime_local_zone - data$datetime
unique(data$time_diff)
data$time_diff <- NULL             # Remove redundant column
data$datetime_local_zone <- NULL   # Remove redundant column


# 4. Correct for depth drift ####

# Select rows with bank datetime and popoff datetime + 15 minutes (= when tag was at atmospheric pressure)
data <- left_join(data, parameters, by = "ID")
bank_popoff <- data %>%
  group_by(ID) %>%
  filter(datetime == bank_datetime | datetime == popoff_datetime,
         pressure_correction != 0) %>%
  mutate(numericdate = as.numeric(datetime))

# Apply linear regression per ID
# https://cran.r-project.org/web/packages/broom/vignettes/broom_and_dplyr.html
#model <- bank_popoff %>%
#  group_by(ID) %>%
#  do(fit_model = lm(pressure ~ datetime, data = .)) 
model <- bank_popoff %>%
  nest(data = -ID) %>% 
  mutate(
    fit = map(data, ~ lm(pressure ~ datetime, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied)

# Remove redundant columns
model <- dplyr::select(model, -data, -fit, -std.error, -statistic, -p.value)


# Extract model coefficients in a tidy dataframe
#model_coef <- tidy(model, fit_model) %>%
#  spread(key = term, value = estimate) %>%
#  select(-std.error, -statistic, -p.value) %>%
#  rename(intercept_val = `(Intercept)`,
#         datetime_val = datetime)

model_coef <- model %>%
  spread(key = term, value = estimate) %>%
  rename(intercept_val = `(Intercept)`,
         datetime_val = datetime)



# Join values to dataset
data <- left_join(data, model_coef, by = "ID")

# Calculate corrected depth
data$numericdate <- as.numeric(data$datetime)
data$regression <- (data$datetime_val  * data$numericdate)  + data$intercept_val
data$corrected_depth <- data$pressure - data$regression


# Reverse depth
data$corrected_depth <- data$corrected_depth * -1


# 5. Clean dataset  ####
data <- data %>%
  dplyr::select(ID, datetime, numericdate, corrected_depth, temperature) 


# 6. Filter for DVM data ####
data_dvm <- filter(data, datetime > "2021-01-01 00:00:00", datetime < "2021-01-14 05:25:00")
#plot(data_dvm$datetime, data_dvm$corrected_depth)


# 7. Create temperature and depth plot for DVM data ####

# Remove temperature NAs by replacing NA by previous non-NA value
data_dvm <- data_dvm[-(1:4), ]
#data_dvm <- data_dvm %>%
#  mutate(temperature_interpolation = na.approx(temperature))
data_dvm$temperature_no_na <- na.locf(data_dvm$temperature)

# Create line every 24 hours
midnight <-  seq.POSIXt(from = lubridate::floor_date(data_dvm$datetime[1], "day"), to= data_dvm$datetime[nrow(data_dvm)], by = 86400)
class(lubridate::floor_date(data_dvm$datetime[1], "day"))

# Create plot
ggplot(data_dvm, aes(x = datetime,
                     y = corrected_depth,
                     color = temperature_no_na)) +
  geom_line(linewidth = 1) +
  scale_color_gradient(low="blue", high="red") +
  geom_line(data = data_dvm[!is.na(data_dvm$temperature),], aes(x = datetime, y = temperature*50), linewidth = 0.5, alpha = 0.5, colour = "red") +
  #scale_y_continuous(breaks = seq(-1000, 600, by = 250)) +
  scale_y_continuous(breaks = seq(-1000, 0, by = 250), 
                     sec.axis = sec_axis(~./50, name = "Temperature (°C)", breaks = seq(-20, 20, by = 5))) +
  theme_minimal() +
  ylab("Depth (m)") +
  xlab("Date") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_x_datetime(date_breaks  ="1 day") +
  geom_vline(xintercept=midnight, color = "darkgray", size = 0.2) 



