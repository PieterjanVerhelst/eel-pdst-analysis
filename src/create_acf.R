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

# Select 1 eel
data <- filter(data, ID == "17525_2")


# Arrange data set according to ID and datetime
data <- data %>% 
  arrange(ID, datetime)


# Create ACF plot
# Create an autocorrelation function plot to explore the cyclic signals in the data. The horizontal blue lines in the plot indicate the confidence interval in the correlogram.
forecast::Acf(data$corrected_depth, type = c("correlation"), lag.max=max(dim(data)), plot = TRUE)  

# For clarity, create the plot for the first 500 observations
forecast::Acf(data$corrected_depth, type = c("correlation"), lag.max=500, plot = TRUE)

#In the plot above, we see 2 cycles subsequently returning. The first is probably related to a tidal signal, as it occurs ca. every 12 hours at lag 144: (12 hours x 60 minutes) / 5 minutes = 144
#The second is probably related to a circadian rythm with a 24 h pattern at ca. lag 288: (24 hours x 60 minutes) / 5 minutes = 288
#To illustrate this, we added a green vertical line at lag 144 and a blue line at lag 288 to the plot.

vals_list <- forecast::Acf(data$corrected_depth, type = c("correlation"), lag.max=max(dim(data)), plot = FALSE)
plot(vals_list[[1]], xlim=c(0, 1000),
     ylab="Correlation value")
abline(v = 144, col = "darkgreen")
abline(v = 288, col = "darkblue")

# Create ggplot version of the plot
acf_values <- as.data.frame(vals_list[[1]])
acf_values <- rename(acf_values, acf_value = V1)
acf_values$index <- seq.int(nrow(acf_values))
acf_values <- head(acf_values, 1000)

acf <- ggplot(data = acf_values, aes(x = index, y = acf_value)) +
  geom_point() +
  theme_minimal() +
  ylab("ACF value") +
  xlab("Measurement number") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 22)) +
  geom_vline(xintercept = 144, linetype="solid", 
               color = "green", size=1.5) +
  geom_vline(xintercept = 288, linetype="solid", 
             color = "blue", size=1.5)
acf  
  
  
# Create plot for publication  
AutoCorrelation <- forecast::Acf(data$corrected_depth, type = c("correlation"), lag.max = 1000, plot = TRUE)

plot(AutoCorrelation)
abline(v = 144, col = "darkgreen", lwd = 5.0)
abline(v = 288, col = "darkblue", lwd = 5.0)




# 2. Create ACF based on dummy data to illustrate patterns ####

# function for creating sine wave
waves <- function(time_in, alpha = 0, beta = 1, freq = 24, phi = 0){
  
  # timestep per hour
  time_step <- 60 / unique(diff(time_in))
  
  # set phi as difference in hours from start of time_in
  phi <- min(time_in) + phi * 3600
  phi<- as.numeric(difftime(phi, min(time_in)))
  phi <- phi / time_step
  
  # get input values to cos func
  in_vals <- seq(0, length(time_in), length = length(time_in))
  in_vals <- in_vals / time_step
  in_vals <- 2 * pi * in_vals * 1 / freq
  
  # wave
  y <- alpha + beta * sin(in_vals + phi)
  
  return(y)
  
}



# input time series for two weeks, 15 minute time step
x <- as.POSIXct(c('2017-04-01', '2017-04-10'))
x <- seq(x[1], x[2], by = 60 * 15)

# get three sine waves
# a: default
# b: amplitude 0.5, 48 hour period
# c: amplitude 2, 12 hour period
a <- waves(x)
circadian <- waves(x, beta = 3, f = 24)
tidal <- waves(x, beta = 3, f = 12)


# get sum of all y values, combine to single object
yall <- rowSums(cbind(circadian, tidal))
dat <- data.frame(x, circadian, tidal, yall) %>% 
  gather('var', 'val', -x)

# plot
ggplot(dat, aes(x = x, y = val)) + 
  geom_line() + 
  facet_wrap(~var, ncol = 1) + 
  theme_bw()


# Combine dummy data into dataframe
dummy <- cbind(x, circadian)
dummy <- cbind(dummy, tidal)
dummy <- as.data.frame(dummy)


# Create ACF
AutoCorrelation <- forecast::Acf(dummy$tidal, type = c("correlation"), lag.max = 288, plot = TRUE)

plot(AutoCorrelation)
abline(v = 48, col = "darkgreen", lwd = 5.0)   # 12-h cycle: (12 * 60) / 15
abline(v = 96, col = "darkblue", lwd = 5.0)   # 24-h cycle: (24 * 60) / 15




