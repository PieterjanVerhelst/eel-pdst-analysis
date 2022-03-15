# Create input file for model: longitude interpolation
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be

# Create dataset with the following columns: Date/Time Stamp,	Long,	Quality
# Date from eel release until tag retrieval
input1 <- data.frame(seq(as.Date("2020/12/24"), as.Date("2021/08/11"), "days")) # take day before retrieval, since exact moment of retrieval is unknown
colnames(input1)[1] <- "Date/Time Stamp"


# Interpolate between longitude of release and retrieval location
input1$Long <- seq(2.757437, -5.02522203, length.out = 231)

# Add quality flag to longitude values
# First and last line have highest certainty and therefore quality; these are flagged with '0'. Other get '5' (= worse quality)
input1$Quality <- 5
input1$Quality[1] <- 0
input1$Quality[231] <- 0


# Write csv file
write.csv(input1, "./data/interim/geolocation_input_files/input_A17538_2/EELA17538LONG.csv", row.names = FALSE)
