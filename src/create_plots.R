# Create plots for data exploration
# By Pieterjan Verhelst and Damiano Oldoni
# Pieterjan.Verhelst@UGent.be & damiano.oldoni@inbo.be


# Setup
library(tidyverse) # To do datascience
library(tidylog)  # To get infos about dplyr functions
library(lubridate)



# 1. Plot raw data ####

# Import data
data <- read_csv("./data/interim/data_current_phases.csv",
                 na = "", 
                 col_types = list(sunrise = col_datetime(),
                                  previous_sunset = col_datetime(),
                                  sunset = col_datetime(),
                                  sunrise = col_datetime(),
                                  next_sunrise = col_datetime(),
                                  next_sunmoment = col_datetime(),
                                  direction_x = col_double(),
                                  V = col_double(),
                                  direction_y = col_double(),
                                  direction = col_double()),          # set direction as numeric
                 guess_max = 100000)


data$...1 <- NULL
data$ID <- factor(data$ID)
data$current_phase_x <- factor(data$current_phase_x)
data$current_phase_y <- factor(data$current_phase_y)


data <-
  data %>%
  arrange(ID, datetime)


# Create subset of several days for plot
subset <- filter(data,
                 ID == "16031",
                 datetime >= "2019-02-03 21:00:00", datetime <= "2019-02-08 07:00:00")

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
  geom_line(size=1.0, binwidth = 1, colour = "black") +
  geom_line(data = subset, aes(x = datetime, y = direction_x*100), size = 1.0, alpha = 0.5, colour = "purple") +
  #scale_y_continuous(breaks = seq(8.000, 12.000, by = 500)) +
  scale_y_continuous(sec.axis = sec_axis(~./100, name = "Eastward velocity (m/s)")) +
  theme_minimal() +
  ylab("Depth (m)") +
  xlab("Date") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_x_datetime(date_breaks  ="1 day") +
  #geom_vline(xintercept=ymd_hms(release), colour="blue") + # Release date and time
  geom_vline(xintercept=gnu, color = "red", size = 1) #+
  #geom_hline(yintercept=0, linetype="dashed", color = "blue", size = 1) +
  #geom_hline(yintercept=90, linetype="dashed", color = "green", size = 1) +
  #geom_hline(yintercept=-90, linetype="dashed", color = "green", size = 1)
fig_circadian_tidal




# Plot raw data with eastward current volocity as factor
start_stop_x <- dplyr::select(subset, datetime, current_phase_x)
start_stop_x$change <- if_else(start_stop_x$current_phase_x != lag(start_stop_x$current_phase_x, 1) ,
                               1,
                               0)

start <- filter(start_stop_x, change == 1)

stop <- start
stop$datetime2 <- stop$datetime - (5*60)
stop$current_phase_x <- if_else(stop$current_phase_x == "westward",
                                "eastward",
                                "westward")
start$change <- NULL
stop$change <- NULL

start <- rename(start, datetime_start = datetime)
stop <- rename(stop, datetime_stop = datetime2)

start$id <- factor(c(1:18))
stop$id <- factor(c(0:17))

start_stop <- left_join(start, stop, by = 'id')

start_stop <- rename(start_stop, current_phase_x = current_phase_x.x)
start_stop$id <- NULL
start_stop$current_phase_x.y <- NULL



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
                          ymax=+Inf), fill = "grey", alpha=0.6) +
  geom_rect(data = start_stop %>% 
              filter(current_phase_x == "eastward") %>%
              distinct(datetime_start, datetime_stop, current_phase_x),
            inherit.aes = FALSE,
            mapping = aes(xmin = datetime_start,
                          xmax = datetime_stop,
                          ymin= -140,
                          ymax= -150), fill = "blue", alpha=0.3) +
  geom_line(size=1.0, binwidth = 1, colour = "black") +
  #geom_line(data = subset, aes(x = datetime, y = direction_x*100), size = 1.0, alpha = 0.5, colour = "purple") +
  #scale_y_continuous(breaks = seq(8.000, 12.000, by = 500)) +
  #scale_y_continuous(sec.axis = sec_axis(~./100, name = "Eastward velocity (m/s)")) +
  theme_minimal() +
  ylab("Depth (m)") +
  xlab("Date") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_x_datetime(date_breaks  ="1 day") +
  #geom_vline(xintercept=ymd_hms(release), colour="blue") + # Release date and time
  geom_vline(xintercept=gnu, color = "red", size = 1) #+
#geom_hline(yintercept=0, linetype="dashed", color = "blue", size = 1) +
#geom_hline(yintercept=90, linetype="dashed", color = "green", size = 1) +
#geom_hline(yintercept=-90, linetype="dashed", color = "green", size = 1)
fig_circadian_tidal






# 2. Plot difference in depth between minima and maxima ####
data_min_max <- read_csv("./data/interim/data_depth_diff.csv",
                 col_types = list(sunrise = col_datetime(),
                                  previous_sunset = col_datetime(),
                                  next_sunrise = col_datetime(),
                                  next_sunmoment = col_datetime(),
                                  direction = col_double()))          # set direction as numeric

# Create plot with day night #
# Create subsets of several days
subset <- filter(data_min_max,
                 ID == "16031",
                 datetime >= "2019-02-04 00:00:00", datetime <= "2019-02-08 00:00:00")

# Create line every 24 hours
gnu <-  seq.POSIXt(from = lubridate::floor_date(subset$datetime[1], "day"), to= subset$datetime[nrow(subset)], by = 86400)
class(lubridate::floor_date(subset$datetime[1], "day"))

# Create plot
fig_depth_diff_circadian_tidal <- ggplot(data = subset, aes(x = datetime, y = depth_change), size = 1.0, alpha = 0.5, colour = "black") +
  geom_rect(data = subset %>% 
              filter(night_day == "night") %>%
              distinct(sunset, sunrise, night_day),
            inherit.aes = FALSE,
            mapping = aes(xmin = sunset,
                          xmax = sunrise,
                          ymin=-Inf,
                          ymax=+Inf), fill = "grey", alpha=0.5) +
  geom_line(binaxis='x', size=1.0, binwidth = 1) +
  geom_line(data = subset, aes(x = datetime, y = 100*U), size = 1.0, alpha = 0.5, colour = "purple") +
  scale_y_continuous(breaks = seq(8.000, 12.000, by = 500)) +
  scale_y_continuous(sec.axis = sec_axis(~./100, name = "Eastward velocity (m/s)")) +
  theme_minimal() +
  ylab("Depth difference (m)") +
  xlab("Date") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_x_datetime(date_breaks  ="1 day") +
  #geom_vline(xintercept=ymd_hms(release), colour="blue") + # Release date and time
  geom_vline(xintercept=gnu, color = "red", size = 1) #+
  #geom_hline(yintercept=0, linetype="dashed", color = "blue", size = 1) +
  #geom_hline(yintercept=45, linetype="dashed", color = "green", size = 1) +
  #geom_hline(yintercept=-45, linetype="dashed", color = "green", size = 1)
fig_depth_diff_circadian_tidal


# Create plot with depth change per binned current direction
subset <- filter(data_min_max,
                 ID == "16031")

subset$degr_360 <- NA
subset <- subset[!is.na(subset$direction),]

for (i in 1:dim(subset)[1]){
  if (subset$direction[i] < 0){
    subset$degr_360[i] = subset$direction[i] + 360
  } else{
    subset$degr_360[i] = subset$direction[i]
  }}

subset$bins <- cut(subset$degr_360, breaks = 72)

ggplot(subset, aes(x = bins, y = depth_change)) +
  stat_summary(fun = "mean", geom = "bar") + 
  theme(axis.text.x = element_text(angle = 90, size = 14, vjust = 0.5, hjust=1))





