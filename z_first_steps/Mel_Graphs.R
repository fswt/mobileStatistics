source("currateData.R")
#Reading the DataFile
dat = read.table("./data/stairs_prepro", header = TRUE, sep="|")
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
#Create Data Summary 
summary(dat)
#Plot data with Seconds and as lines
plot(dat$timestamp/1000, dat$x,type="l", xlab="Seconds",ylab="Acceleration Magnitude")
#Calculate Mean and variance of the Data
mean(dat[dat$statusId=="6",]$x, )
var(dat[dat$statusId=="6",]$x)
#Compute Magnitude:: 
dat$mag=sqrt(dat$x^2+dat$y^2+dat$z^2)
dat
#Convert Status ID in categorical Variable
dat$statusId <-factor(dat$statusId, labels=c("Walk"))
summary(dat$statusId)
# Mean and Var of Walk
mean(dat[dat$statusId=="Walk",]$x, )
var(dat[dat$statusId=="Walk",]$x)
#attach Data so that we can have access directly on the Variable
attach(dat)
mean(x)
#Create Substets of Data 
walk <- subset(dat, statusId=="Walk")
run <- subset(dat, statusId=="Run")
summary(run)

#Plot both Subsets seperatly
plot(walk$timestamp/1000,walk$mag,type="l",xlab="Seconds", ylab="Magnitude",main="Walk",ylim=c(0,80))
plot(walk$timestamp/1000,walk$mag,type="l",xlab="Seconds", ylab="Magnitude",main="Run")
#Plot Histogram ! argument breaks=c(0.35) is missing
hist(walk$mag, xlab="Acc.Magn.",prob=T)
#drawing curve in Histogram
curve(dnorm(x,mean=mean(walk$mag),sd=sd(walk$mag)),add=TRUE)
#Empirical Cumulative Distribution Function(ECDF)
plot(edfc(mag))

#Q-Q plot 
qqnorm(mag)

#Boxplot

boxplot(mag, ylim=c(0,30))

