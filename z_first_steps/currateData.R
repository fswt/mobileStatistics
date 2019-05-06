#Reading the DataFile
dat = read.table("./data/test/test2.txt", header = TRUE, sep="|")
#Getting the Header of the File
head(dat)
#currate Timestamps
#mt= Minimun Timestamp
mt <- min(dat$timestamp)
dat$timestamp <- dat$timestamp - mt
head(dat)
#Plot generates the main 
datAcc <- dat[dat$sensorName=="3-axis Gyroscope",]
plot(datAcc$timestamp,datAcc$x,type="l",col="red")
#Line is adding a new grapg to the Main Plot
lines(datAcc$timestamp,datAcc$y,col="green")
lines(datAcc$timestamp,datAcc$z,col="blue")
dat
