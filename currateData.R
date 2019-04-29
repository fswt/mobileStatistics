#Reading the DataFile
dat = read.table("./data/test/SensorTest", header = TRUE, sep="|")
#Getting the Header of the File
head(dat)
#currate Timestamps
#mt= Minimun Timestamp
mt <- min(dat$timestamp)
dat$timestamp <- dat$timestamp - mt
head(dat)
#Plot generates the main 
plot(dat$timestamp,dat$x,type="l",col="red")
#Line is adding a new grapg to the Main Plot
lines(dat$timestamp,dat$y,col="green")
lines(dat$timestamp,dat$z,col="blue")
