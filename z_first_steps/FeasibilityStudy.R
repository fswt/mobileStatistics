#Reading the DataFile
stairsData = read.table("./data/stairs_prepro", header = TRUE, sep="|")
#Getting the Header of the File
head(stairsData)
dat
#Rename Labels
stairsData$statusId <-factor(stairsData$statusId, labels=c("Stairs Up without weight","Stairs Down without weight","Stairs Up with weight","Stairs Down with weight"))
#Attach Main Data Set, so that the variables could be addressed directly
attach(dat)
summary(statusId)
# calculate the Magnitude 
dat$magnitude=sqrt(x^2+y^2+z^2)
head(dat)
#Creating Subsets:
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
