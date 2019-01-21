# Upload and process files
# Pieterjan Verhelst
# Pieterjan.Verhelst@ugent.be


temp <- list.files(path = "./data/raw/", pattern="*.csv")
myfiles <- lapply(temp, read.delim)
