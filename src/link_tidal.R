# Link tidal data* to the dataset
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be
# *tidal data obtained via John Aldridge (CEFAS, UK. john.aldridge@cefas.co.uk)

# Set time zone
Sys.setenv(TZ='GMT')
Sys.timezone()


# Packages
library(tidyverse)
library(lubridate)




# 1. Load dataset with all eels ####
data <- read_csv("./data/interim/data_circadian.csv")
data$X1 <- NULL
data$X1_1 <- NULL
data$ID <- factor(data$ID)


# 2. Load tidal data ####
tidal_list_names <- list.files(path = "./data/external/tidal_data_john_aldridge/",
                    pattern = "*.dat", 
                    full.names = T)

tidal_list <- lapply(tidal_list_names, function(x) {
  out <- data.table::fread(x, header = FALSE)
  out$source_file <- x
  return(out)
})

tidal <- data.table::rbindlist(tidal_list)

# Format columns
tidal$ID <- str_replace(tidal$source, "./data/external/tidal_data_john_aldridge/eel_", "") 
tidal$ID <- str_replace(tidal$ID, ".dat", "") 
tidal$ID <- factor(tidal$ID)
tidal$source_file <- NULL

tidal <- tidal %>%
  rename(date = V1,
         time = V2,
         lon = V3,
         lat = V4,
         U = V5,
         V = V6,
         speed = V7,
         direction = V8)

tidal$datetime <- paste(tidal$date, tidal$time)
tidal$datetime <- dmy_hms(tidal$datetime)

# set tidal dataset to dataframe
tidal <- as.data.frame(tidal)


# 3. Link tidal data to dataset ####
# Set hourly resolution
data$datehour <- lubridate::floor_date(data$datetime, "hour")
tidal$datehour <- lubridate::floor_date(tidal$datetime, "hour")

# Remove double dates per eel (ID)
tidal <- tidal[!duplicated(tidal[c('ID','datehour')]),] 

# Merge datasets
data_tidal <- left_join(data, tidal, by = c('ID', 'datehour'))

# Process dataset
data_tidal <- rename(data_tidal, datetime = datetime.x)
data_tidal$datetime.y <- NULL
data_tidal$date <- NULL
data_tidal$time <- NULL





# 4. write csv ####
write.csv(data_tidal, "./data/interim/data_circadian_tidal.csv")




# 5. Create plot with tidal data ####
# Create subsets of several days
subset <- filter(data_tidal,
                 ID == "16031",
                 datetime >= "2019-02-08 00:00:00", datetime <= "2019-02-13 00:00:00")

# Create line every 24 hours
gnu <-  seq.POSIXt(from = lubridate::floor_date(subset$datetime[1], "day"), to= subset$datetime[nrow(subset)], by = 86400)
class(lubridate::floor_date(subset$datetime[1], "day"))

# Create plot
fig_tidal <- ggplot(subset, aes(x = datetime,
                                y = corrected_depth), size = 1.0, alpha = 0.5) +
  geom_rect(aes(xmin=sunrise, xmax=sunset, ymin=-Inf, ymax=+Inf), fill='grey', alpha=0.3) +
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
fig_tidal




