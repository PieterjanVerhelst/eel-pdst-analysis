# Create plots for DVM data exploration
# By Pieterjan Verhelst and Damiano Oldoni
# Pieterjan.Verhelst@UGent.be & damiano.oldoni@inbo.be


# Setup
library(tidyverse) # To do datascience
#library(tidylog)  # To get infos about dplyr functions
library(lubridate)
#library(pracma)  # For the 'deg2rad()' function




# 1. Load data ####

# Import data
data_dvm <- read_csv("./data/interim/data_dvm_circadian_moon_sun.csv",
                 na = "", 
                 col_types = list(datetime = col_datetime(),
                                  sunset = col_datetime(),
                                  sunrise = col_datetime(),
                                  #next_sunrise = col_datetime(),
                                  #next_sunmoment = col_datetime(),
                                  direction_x = col_double(),
                                  moon_fraction = col_double(),
                                  sun_altitude = col_double(),
                                  sun_azimuth = col_double()),         
                 guess_max = 100000)


data_dvm$...1 <- NULL
data_dvm$ID <- factor(data_dvm$ID)


# 2. Create plot ####

data_dvm_subset <- filter(data_dvm, ID == "16031")
data_dvm_subset <- filter(data_dvm, ID == "17476", datetime > "2020-03-19 00:00:00", datetime < "2020-03-24 00:00:00")


data_dvm_subset <- data_dvm_subset[-(1:4), ]
data_dvm_subset <- data_dvm_subset %>%
  group_by(ID) %>%
  mutate(temperature_no_na = na.locf(temperature))

# Create line every 24 hours
midnight <-  seq.POSIXt(from = lubridate::floor_date(data_dvm_subset$datetime[1], "day"), to= data_dvm_subset$datetime[nrow(data_dvm_subset)], by = 86400)
class(lubridate::floor_date(data_dvm_subset$datetime[1], "day"))

# Create plot
ggplot(data_dvm_subset, aes(x = datetime,
                            y = corrected_depth,
                            color = temperature_no_na)) +
  geom_rect(data = data_dvm_subset %>% 
              filter(night_day == "night") %>%
              distinct(sunset, sunrise, night_day),
            inherit.aes = FALSE,
            mapping = aes(xmin = sunset,
                          xmax = sunrise,
                          ymin=-Inf,
                          ymax=+Inf), fill = "grey", alpha=0.5) +
  geom_line(linewidth = 1) +
  scale_color_gradient(low="blue", high="red") +
  #geom_line(data = data_dvm_subset, aes(x = datetime, y = 10000 * moon_fraction), linewidth = 1.0, alpha = 0.5, colour = "purple") +
  # geom_line(data = data_dvm_subset[!is.na(data_dvm_subset$temperature),], aes(x = datetime, y = temperature*50), linewidth = 0.5, alpha = 0.5, colour = "red") +
  scale_y_continuous(breaks = seq(-1000, 10, by = 100)) +
  #scale_y_continuous(breaks = seq(-1000, 10, by = 100), 
  #                   sec.axis = sec_axis(~./50, name = "Temperature (°C)", breaks = seq(-20, 20, by = 5))) +
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
  geom_vline(xintercept=midnight, color = "darkgray", linewidth = 0.2) 





