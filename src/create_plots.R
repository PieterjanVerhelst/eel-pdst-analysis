# Create plots for data exploration
# By Pieterjan Verhelst and Damiano Oldoni
# Pieterjan.Verhelst@UGent.be & damiano.oldoni@inbo.be


# Setup
library(tidyverse) # To do datascience
library(tidylog)  # To get infos about dplyr functions
library(suncalc)  # To get sunrise, sunset



# Import data
data <- read_csv("./data/interim/data_circadian_tidal.csv", na = "", guess_max = 100000)

# Create subset per eel
subset <- filter(data,
                 ID == "16031")

# Get ordered series of sunsets
series_sunsets <- unique(data$sunset)
series_sunsets <- series_sunsets[order(series_sunsets)]

# Transform to df (tibble)
series_sunsets <- tibble(sunset = series_sunsets)
# Add previous sunset
series_sunsets <-
  series_sunsets %>%
  mutate(previous_sunset = lag(sunset))


# Get ordered series of sunrises
series_sunrises <- unique(data$sunrise)
series_sunrises <- series_sunrises[order(series_sunrises)]

# Transform to df (tibble)
series_sunrises <- tibble(sunrise = series_sunrises)
# Add previous sunrise
series_sunrises <-
  series_sunrises %>%
  mutate(previous_sunrise = lag(sunrise))

# Bind datasets together
series_sunsets_sunrises <- cbind(series_sunsets, series_sunrises)

# Identify day and night periods by using series_sunsets
subset <-  subset %>%
  left_join(series_sunsets_sunrises,
            by = c("sunset"))

# Process dataset
subset <- rename(subset, sunrise = sunrise.x)
subset$sunrise.y <- NULL




# Create subset of several days for plot
subset2 <- filter(subset,
                 ID == "16031",
                 datetime >= "2019-02-08 00:00:00", datetime <= "2019-02-13 00:00:00")
subset2$direction <- as.numeric(subset2$direction)


# Create line every 24 hours
gnu <-  seq.POSIXt(from = lubridate::floor_date(subset2$datetime[1], "day"), to= subset2$datetime[nrow(subset2)], by = 86400)
class(lubridate::floor_date(subset2$datetime[1], "day"))

# Create plot
fig_circadian_tidal <- ggplot(subset2, aes(x = datetime,
                                y = corrected_depth), size = 1.0, alpha = 0.5) +
  geom_rect(aes(xmin=previous_sunset, xmax=sunrise, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3) +
  geom_line(binaxis='x', size=1.0, binwidth = 1, colour = "black") +
  geom_line(data = subset2, aes(x = datetime, y = direction/2), size = 2.0, alpha = 0.5, colour = "purple") +
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












