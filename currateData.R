#Reading the DataFile
dat = read.table("./data/test/SensorTest", header = TRUE, sep="|")
#Getting the Header of the File
head(dat)
#currate Timestamps
#mt= Minimun Timestamp
mt <- min(dat$timestamp)
dat$timestamp <- dat$timestamp - mt
head(dat)
