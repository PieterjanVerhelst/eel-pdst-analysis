# Create input file for model: longitude interpolation
# By Pieterjan Verhelst
# Pieterjan.Verhelst@UGent.be

# Create dataset with the following columns: Date/Time Stamp,	Long,	Quality
# Date from eel release until tag retrieval
input1 <- data.frame(seq(as.Date("2012/11/22"), as.Date("2012/12/14"), "days")) # take day before retrieval, since exact moment of retrieval is unknown
colnames(input1)[1] <- "Date/Time Stamp"


# Interpolate between longitude of release and retrieval location
input1$Long <- seq(9.40194, 6.599874, length.out = 23)

# Add quality flag to longitude values
# First and last line have highest certainty and therefore quality; these are flagged with '0'. Other get '5' (= worse quality)
input1$Quality <- 5
input1$Quality[1] <- 0
input1$Quality[23] <- 0


# Write csv file
write.csv(input1, "./data/interim/input_A09424/EELA09424LONG.csv", row.names = FALSE)
