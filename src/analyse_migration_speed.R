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

# 1. All eels
boxplot(speed ~ Direction, data = tr_summary)
tr_summary %>%
  group_by(Direction) %>%
  summarize(mean = mean(speed))
min(tr_summary$total_dist)
max(tr_summary$total_dist)
min(tr_summary$days)
max(tr_summary$days)

# 2. Eels with nice tracks
tr_summary2 <- filter(tr_summary, ID == '9349' |
                        ID == '9355' |
                        ID == '9358' |
                        ID == '9359' |
                        ID == '9374' |
                        ID == '9377' |
                        ID == '9393' |
                        ID == '9423' |
                        ID == '9424' |
                        ID == '15706' |
                        ID == '15714' |
                        ID == '15730' |
                        ID == '15777' |
                        ID == '15805' |
                        ID == '15981' |
                        ID == '16031' |
                        ID == '112061' |
                        ID == '112064')

boxplot(speed ~ Direction, data = tr_summary2)
tr_summary2 %>%
  group_by(Direction) %>%
  summarize(mean = mean(speed))
min(tr_summary2$total_dist)
max(tr_summary2$total_dist)
min(tr_summary2$days)
max(tr_summary2$days)


# 3. Eels with crappy tracks
tr_summary3 <- filter(tr_summary, ID != '9349' ,
                        ID != '9355' ,
                        ID != '9358' ,
                        ID != '9359' ,
                        ID != '9374' ,
                        ID != '9377' ,
                        ID != '9393' ,
                        ID != '9423' ,
                        ID != '9424' ,
                        ID != '15706' ,
                        ID != '15714' ,
                        ID != '15730' ,
                        ID != '15777' ,
                        ID != '15805' ,
                        ID != '15981' ,
                        ID != '16031' ,
                        ID != '112061' ,
                        ID != '112064')

boxplot(speed ~ Direction, data = tr_summary3)
tr_summary3 %>%
  group_by(Direction) %>%
  summarize(mean = mean(speed))
min(tr_summary3$total_dist)
max(tr_summary3$total_dist)
min(tr_summary3$days)
max(tr_summary3$days)



# Data analysis

# independent 2-group t-test
# t.test(y~x) # where y is numeric and x is a binary factor 
t.test(tr_summary2$speed~tr_summary2$Direction)

# Assumptions:
# 1. 2 independent groups
# N and SW are independent (each direction is attributed to a different animal)

# 2. Normality
qqnorm(tr_summary2$speed, pch = 1, frame = TRUE)
qqline(tr_summary2$speed, col = "steelblue", lwd = 2)

shapiro.test(tr_summary2$speed)
# The p-value > 0.05 implying that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.

# 3. Homogeneity of variances
# F-test 
# The F-test is used to assess whether the variances of two populations (A and B) are equal. The test requires normality (in case no normality, apply Fligner-Killeen’s test)
# https://www.datanovia.com/en/lessons/homogeneity-of-variance-test-in-r/
var.test(speed ~ Direction, data = tr_summary2)
# When p > 0.05, there is no significant difference between the two variances.




