# Create an ACF plot to visualise the vertical activity pattern of eels
# By Pieterjan Verhelst
# pieterjan.verhelst@inbo.be



# Load packages 
library(tidyverse)
library(lubridate)
library(forecast)


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


# Create ACF plot
# Create an autocorrelation function plot to explore the cyclic signals in the data. The horizontal blue lines in the plot indicate the confidence interval in the correlogram.
forecast::Acf(data_1eel$corrected_depth, type = c("correlation"), lag.max=max(dim(data_1eel)), plot = TRUE)  

# For clarity, create the plot for the first 500 observations
forecast::Acf(data_1eel$corrected_depth, type = c("correlation"), lag.max=500, plot = TRUE)

#In the plot above, we see 2 cycles subsequently returning. The first is probably related to a tidal signal, as it occurs ca. every 12 hours at lag 144: (12 hours x 60 minutes) / 5 minutes = 144
#The second is probably related to a circadian rythm with a 24 h pattern at ca. lag 288: (24 hours x 60 minutes) / 5 minutes = 288
#To illustrate this, we added a green vertical line at lag 144 and a blue line at lag 288 to the plot.

vals_list <- forecast::Acf(data_1eel$corrected_depth, type = c("correlation"), lag.max=max(dim(data_1eel)), plot = FALSE)
plot(vals_list[[1]], xlim=c(0, 1000),
     ylab="Correlation value")
abline(v = 144, col = "darkgreen")
abline(v = 288, col = "darkblue")


