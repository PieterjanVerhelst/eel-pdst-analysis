# Create plots for data exploration
# By Pieterjan Verhelst and Damiano Oldoni
# Pieterjan.Verhelst@UGent.be & damiano.oldoni@inbo.be


# Setup
library(tidyverse) # To do datascience
library(tidylog)  # To get infos about dplyr functions
library(lubridate)


# Import data
data <- read_csv("./data/interim/data_circadian_tidal.csv",
                 na = "", 
                 col_types = list(sunrise = col_datetime(),
                                  previous_sunset = col_datetime(),
                                  next_sunrise = col_datetime(),
                                  next_sunmoment = col_datetime(),
                                  direction = col_double()),          # set direction as numeric
                 guess_max = 100000)


# Create subset per eel
subset <- filter(data,
                 ID == "16031")


# Create subset of several days for plot
subset <- filter(data,
                 ID == "16031",
                 datetime >= "2019-02-08 00:00:00", datetime <= "2019-02-13 00:00:00")

# Create line every 24 hours
gnu <-  seq.POSIXt(from = lubridate::floor_date(subset$datetime[1], "day"), to= subset$datetime[nrow(subset)], by = 86400)
class(lubridate::floor_date(subset$datetime[1], "day"))

# Create plot
fig_circadian_tidal <- ggplot(subset, aes(x = datetime,
                                y = corrected_depth), size = 1.0, alpha = 0.5) +
  geom_rect(data = subset %>% 
              filter(night_day == "night") %>%
              distinct(sunset, sunrise, night_day),
            inherit.aes = FALSE,
            mapping = aes(xmin = sunset,
                          xmax = sunrise,
                          ymin=-Inf,
                          ymax=+Inf), fill = "grey", alpha=0.5) +
  geom_line(binaxis='x', size=1.0, binwidth = 1, colour = "black") +
  geom_line(data = subset, aes(x = datetime, y = direction/2), size = 2.0, alpha = 0.5, colour = "purple") +
  #scale_y_continuous(breaks = seq(8.000, 12.000, by = 500)) +
  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Tidal direction (°)")) +
  theme_minimal() +
  ylab("Depth (m)") +
  xlab("Date") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_x_datetime(date_breaks  ="1 hour") +
  #geom_vline(xintercept=ymd_hms(release), colour="blue") + # Release date and time
  geom_vline(xintercept=gnu, color = "red", size = 1) +
  geom_hline(yintercept=0, linetype="dashed", color = "blue", size = 1) +
  geom_hline(yintercept=90, linetype="dashed", color = "green", size = 1) +
  geom_hline(yintercept=-90, linetype="dashed", color = "green", size = 1)
fig_circadian_tidal












