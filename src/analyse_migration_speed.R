# Calculate and analyse horizontal migration speeds
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be



# Load file with trajectory distances for all eels
source("./src/read_trajectory_data.R")


# Set columns in according formats
tr_data$ID <- as.factor(tr_data$ID)
tr_data$Date <- as.Date(tr_data$Date)
tr_data$Distance <- as.numeric(tr_data$Distance)


# Calculate number of days and total distance
tr_summary <- tr_data %>%
  group_by(ID) %>%
  summarize(total_dist = sum(Distance),
            days = max(Date)-min(Date))


# Calculate migration speed (km/day)
tr_summary$days <- as.numeric(tr_summary$days)
tr_summary$speed <- tr_summary$total_dist/tr_summary$days


# Upload file with migration direction
direction <- read_csv("./data/external/migration_direction.csv")

# Merge direction to dataset
tr_summary <- merge(tr_summary, direction, by = "ID")


# Data exploration
boxplot(speed ~ Direction, data = tr_summary)
tr_summary %>%
  group_by(Direction) %>%
  summarize(mean = mean(speed))

tr_summary2 <- filter(tr_summary, ID == '9358'|
                      ID == '9374' |
                        ID == '9377' |
                        ID == '9393' |
                        ID == '9423' |
                        ID == '9424' |
                        ID == '15706' |
                        ID == '15714' |
                        ID == '15777' |
                        ID == '15805' |
                        ID == '15981' |
                        ID == '16031' |
                        ID == '112061')

boxplot(speed ~ Direction, data = tr_summary2)
tr_summary2 %>%
  group_by(Direction) %>%
  summarize(mean = mean(speed))




