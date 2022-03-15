# Create input file for model: longitude interpolation
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be

# Create dataset with the following columns: Date/Time Stamp,	Long,	Quality
# Date from eel release until tag retrieval
input1 <- data.frame(seq(as.Date("2019/11/20"), as.Date("2020/02/09"), "days")) # take day before retrieval, since exact moment of retrieval is unknown
colnames(input1)[1] <- "Date/Time Stamp"


# Interpolate between longitude of release and retrieval location
input1$Long <- seq(12.298746, 12.5063, length.out = 82)

# Add quality flag to longitude values
# First and last line have highest certainty and therefore quality; these are flagged with '0'. Other get '5' (= worse quality)
input1$Quality <- 5
input1$Quality[1] <- 0
input1$Quality[82] <- 0


# Write csv file
write.csv(input1, "./data/interim/geolocation_input_files/input_A17470/EELA17470LONG.csv", row.names = FALSE)
