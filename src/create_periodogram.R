# Create periodogram to identify main periods in the depth (time series) data
# Pieterjan Verhelst
# pieterjan.verhelst@inbo.be

library(TSA)
library(zoo)  # To convert dataframe into time series
library(timeSeries)
library(imputeTS)  # For function 'na_remove()'



# Convert data to time series of either zoo class or ts class
# https://www.statology.org/r-convert-data-frame-to-time-series/
eel <- filter(data, ID == "16031" ,
              datetime > "2019-01-31 00:00:00" ,
              datetime < "2019-02-13 00:00:00")
eel <- dplyr::filter(data_summary, ID == "16031" ,
              date_hour> "2019-01-31 00:00:00" ,
              date_hour < "2019-02-13 00:00:00")
eel <- dplyr::filter(data_summary, ID == "16031")

#eel <- dplyr::select(eel, datetime, rel_depth)
eel <- dplyr::select(eel, date_hour, mean_rel_depth)
eel <- na.omit(eel)
eel$ID <- NULL
eel <- eel[!duplicated(eel$date_hour), ]
class(eel)
tseries <- read.zoo(eel)
class(tseries)
tseries_ts <- as.ts(tseries)
class(tseries_ts)
tseries_ts
plot(tseries_ts,xlab='Day',ylab='Depth')
dataframe_tseries_ts <- as.data.frame(tseries_ts)

# Remove NA from time series object
tseries_ts_no_na <- na_remove(tseries_ts)
sum(is.na(tseries_ts_no_na))

# Create periodogram
# https://online.stat.psu.edu/stat510/lesson/12/12.1#:~:text=The%20raw%20periodogram%20is%20a,over%20a%20continuum%20of%20frequencies.
periodogram(tseries_ts_no_na, log = 'no', ylab='Periodogram', plot = TRUE, lwd = 3)
spec.pgram(tseries_ts_no_na, lwd = 1, xlab = 'Frequency', ylab = 'Spectrum', detrend = FALSE, spans = 6)
spectrum(tseries_ts_no_na)

spec.ar(tseries_ts_no_na, log="no")
spec_values <- spec.ar(tseries_ts_no_na, log="no")

# Create dataframe
freq <- spec_values[[1]]
spec <- spec_values[[2]]

df_periodogram <- as.data.frame(cbind(freq, spec))
df_periodogram <- df_periodogram %>%
  rename(spec = V2)

# Turn frequency into hourly period
df_periodogram$freq_hour <- 1/df_periodogram$freq


# Create plot
ggplot(data=df_periodogram, aes(x=freq_hour, y=spec)) +
  geom_line() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylab("Spectrum") +
  xlab("Hour") +
  xlim(0, 40) +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20)) +
  geom_vline(xintercept = 12, linetype="solid", 
             color = "green", size=1.5) +
  geom_vline(xintercept = 24, linetype="solid", 
             color = "blue", size=1.5)




