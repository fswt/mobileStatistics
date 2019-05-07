#Rename Labels
stairsData$statusId <-factor(stairsData$statusId, labels=c("Stairs Up without weight","Stairs Down without weight","Stairs Up with weight","Stairs Down with weight"))
#Attach Main Data Set, so that the variables could be addressed directly
attach(stairsData)
# calculate the Magnitude 
stairsData$magnitude=sqrt(x^2+y^2+z^2)
#Creating Subsets:
sub_up_without <- subset(stairsData, statusId=="Stairs Up without weight")
sub_down_without <- subset(stairsData, statusId=="Stairs Down without weight")
sub_up_with <- subset(stairsData, statusId=="Stairs Up with weight")
sub_down_with <- subset(stairsData, statusId=="Stairs Down with weight")
#Cleaning Time: 
mt <- min(sub_up_without$timestamp)
sub_up_without$timestamp <- (sub_up_without$timestamp -mt)

mt <- min(sub_down_without$timestamp)
sub_down_without$timestamp <- (sub_down_without$timestamp -mt) 

mt <- min(sub_up_with$timestamp)
sub_up_with$timestamp <- (sub_up_with$timestamp -mt) 

mt <- min(sub_down_with$timestamp)
sub_down_with$timestamp <- (sub_down_with$timestamp -mt) 