# Calculate noon based on DVM behaviour to identify lat and lon with the equation of time (latter performed in excel)
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be


# Create temperature and pressure plot from several days ####
# Create subset with DVM
subset <- filter(aggdata, datetime2 >= "2020-02-13 00:00:00", datetime2 <= "2020-02-14 00:00:00")

# Create line every 24 hours
gnu <-  seq.POSIXt(from = lubridate::floor_date(subset$datetime2[1], "day"), to= subset$datetime2[nrow(subset)], by = 86400)
class(lubridate::floor_date(subset$datetime2[1], "day"))

# Create plot
fig_subset_dvm <- ggplot(subset, aes(x = datetime2,
                                       y = temperature)) +
  geom_point(binaxis='x', dotsize=0.5, binwidth = 1) +
  geom_point(data = subset, aes(x = datetime2, y = pressure/2), dotsize = 0.5, alpha = 0.5, colour = "purple") +
  #scale_y_continuous(breaks = seq(8.000, 12.000, by = 500)) +
  scale_y_continuous(sec.axis = sec_axis(~.*2, name = "Pressure (m)")) +
  theme_minimal() +
  ylab("Temperature (°C)") +
  xlab("Date") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  scale_x_datetime(date_breaks  ="1 hour") +
  geom_vline(xintercept=ymd_hms(release), colour="blue") + # Release date and time
  geom_vline(xintercept=gnu, color = "red", size = 1) 

fig_subset_dvm



# Create file to fill in values manually ####
noon <- data.frame(seq(1:6)) 

noon$depth_range <- NA
noon$dawn <- NA
noon$dusk <- NA


# Filter depth layers
day1 <- filter(aggdata, datetime2 >= "2020-02-13 06:00:00", datetime2 <= "2020-02-13 20:00:00")
day1$pressure_rnd <- round(day1$pressure, digits=1)

depth_day1 <- filter(day1, pressure_rnd >= '-420.0', pressure_rnd <= '-470.0')
depth_day1


# Run in values manually
i = 1

noon$depth_range[i] <- "424.9-428.5"
noon$dawn[i] <- "2020-02-13 07:45:00"
noon$dusk[i] <- "2020-02-13 18:45:00"


# Convert to date-time
noon$dawn_t <- ymd_hms(noon$dawn)
noon$dusk_t <- ymd_hms(noon$dusk)

# Convert to numeric
noon$dawn_num <- as.numeric(noon$dawn_t)
noon$dusk_num <- as.numeric(noon$dusk_t)

# Centralize
noon$noon_num <- (noon$dusk_num+noon$dawn_num)/2

# Convert to POSIXct
noon$noon_t <- as.POSIXct(as.numeric(noon$noon_num), origin='1970-01-01')


# write csv file
write.csv(noon, "./data/interim/dvm_noon/A15789_dvm_noon.csv", row.names = FALSE)
