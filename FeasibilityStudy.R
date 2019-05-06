#Reading the DataFile
dat = read.table("./data/stairs_prepro", header = TRUE, sep="|")
#Getting the Header of the File
head(dat)
dat
#Rename Labels
dat$statusId <-factor(dat$statusId, labels=c("Stairs Up without weight","Stairs Down without weight","Stairs Up with weight","Stairs Down with weight"))
#Attach Main Data Set, so that the variables could be addressed directly
attach(dat)
summary(statusId)
# calculate the Magnitude 
dat$magnitude=sqrt(x^2+y^2+z^2)
head(dat)
#Creating Subsets after statusID:
sub_up_without <- subset(dat, statusId=="Stairs Up without weight")
sub_down_without <- subset(dat, statusId=="Stairs Down without weight")
sub_up_with <- subset(dat, statusId=="Stairs Up with weight")
sub_down_with <- subset(dat, statusId=="Stairs Down with weight")
#Cleaning Time: 
mt <- min(sub_up_without$timestamp)
sub_up_without$timestamp <- (sub_up_without$timestamp -mt)

mt <- min(sub_down_without$timestamp)
sub_down_without$timestamp <- (sub_down_without$timestamp -mt) 

mt <- min(sub_up_with$timestamp)
sub_up_with$timestamp <- (sub_up_with$timestamp -mt) 

mt <- min(sub_down_with$timestamp)
sub_down_with$timestamp <- (sub_down_with$timestamp -mt) 
#TestPlot
plot(sub_up_without$timestamp,sub_up_without$magnitude,type="l",col="red")
lines(sub_up_with$timestamp,sub_up_with$magnitude,type="l",col="green")


#Test hist
hist(sub_down_without$mag, xlab="Acc.Magn.",prob=T)
hist(sub_down_with$mag, xlab="Acc.Magn.",prob=T)

#Histograms for LGE Linear magnitude Values
par(mfcol=c(2,2))
hist(sub_down_without[sub_down_without$sensorName=="LGE Linear Acceleration Sensor",]$mag, breaks=c(0:50),ylim =c(0,0.500) , xlab="Acc.Magn.",prob=T)
hist(sub_down_with[sub_down_with$sensorName=="LGE Linear Acceleration Sensor",]$mag,breaks=c(0:50),ylim =c(0,0.5), xlab="Acc.Magn",prob=T)
hist(sub_up_without[sub_up_without$sensorName=="LGE Linear Acceleration Sensor",]$mag, breaks=c(0:50),ylim =c(0,0.500) , xlab="Acc.Magn.",prob=T)
hist(sub_up_with[sub_up_with$sensorName=="LGE Linear Acceleration Sensor",]$mag,breaks=c(0:50),ylim =c(0,0.5), xlab="Acc.Magn",prob=T)

#Creating Subsets after Sensor Name:
sub_up_without <- subset(sub_up_without, sub_up_without$sensorName=="LGE Linear Acceleration Sensor")
sub_down_without <- subset(sub_down_without, sub_down_without$sensorName=="LGE Linear Acceleration Sensor")
sub_up_with <- subset(sub_up_with, sub_up_with$sensorName=="LGE Linear Acceleration Sensor")
sub_down_with <- subset(sub_down_with, sub_down_with$sensorName=="LGE Linear Acceleration Sensor")

#Copmare Graph lines
par(mfcol=c(2,1))
plot(sub_down_without$timestamp,sub_down_without$mag, type="l",col="red")
lines(sub_down_with$timestamp,sub_down_with$mag,col="green")
plot(sub_up_without$timestamp,sub_up_without$mag, type="l",col="red")
lines(sub_up_with$timestamp,sub_up_with$mag,col="green")

#boxPlots
par(mfcol=c(2,2))
boxplot(sub_down_without$mag, main="sub_down_without", ylim =c(0,40))
boxplot(sub_down_with$mag, main="sub_down_with", ylim =c(0,40))
boxplot(sub_up_without$mag, main ="sub_up_without", ylim =c(0,40))
boxplot(sub_down_with$mag, main="sub_down_with", ylim =c(0,40))
