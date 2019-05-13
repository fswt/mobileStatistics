preprocess <- function(){
  stairsData <<- rename_labels() # Why do we have to return, so that a change happens? Variable is global, right? (also: #assign to parent)
  #Attach Main Data Set, so that the variables can be addressed directly
  attach(stairsData)
  stairsData <<- calculate_magnitude() #assign to parent
  subsets <- create_subsets()
  subsets <- normalize_timestamps(subsets$sub_up_without, subsets$sub_down_without, subsets$sub_up_with, subsets$sub_down_with)
  create_sensor_name_subsets(subsets$sub_up_without, subsets$sub_down_without, subsets$sub_up_with, subsets$sub_down_with)
  return(subsets)
}

rename_labels <- function(){
  stairsData$statusId <- factor(stairsData$statusId, labels=c("Stairs Up without weight","Stairs Down without weight","Stairs Up with weight","Stairs Down with weight"))
  return(stairsData)
}

calculate_magnitude <- function(){
  stairsData$magnitude = sqrt(x^2+y^2+z^2)
  return(stairsData)
}

create_subsets <- function(){
  sub_up_without <- subset(stairsData, statusId=="Stairs Up without weight")
  sub_down_without <- subset(stairsData, statusId=="Stairs Down without weight")
  sub_up_with <- subset(stairsData, statusId=="Stairs Up with weight")
  sub_down_with <- subset(stairsData, statusId=="Stairs Down with weight")
  result <- list(sub_up_without=sub_up_without, sub_down_without=sub_down_without, sub_up_with=sub_up_with, sub_down_with=sub_down_with)
  return(result)
}

normalize_timestamps <- function(sub_up_without, sub_down_without, sub_up_with, sub_down_with){
  mt <- min(sub_up_without$timestamp)
  sub_up_without$timestamp <- (sub_up_without$timestamp - mt)
  
  mt <- min(sub_down_without$timestamp)
  sub_down_without$timestamp <- (sub_down_without$timestamp - mt) 
  
  mt <- min(sub_up_with$timestamp)
  sub_up_with$timestamp <- (sub_up_with$timestamp - mt) 
  
  mt <- min(sub_down_with$timestamp)
  sub_down_with$timestamp <- (sub_down_with$timestamp - mt) 
  
  result <- list(sub_up_without=sub_up_without, sub_down_without=sub_down_without, sub_up_with=sub_up_with, sub_down_with=sub_down_with)
  return(result)
}

######### Under construction #################
create_sensor_name_subsets <- function(sub_up_without, sub_down_without, sub_up_with, sub_down_with){
  sub_up_without <- subset(sub_up_without, sub_up_without$sensorName=="LGE Linear Acceleration Sensor")
  sub_down_without <- subset(sub_down_without, sub_down_without$sensorName=="LGE Linear Acceleration Sensor")
  sub_up_with <- subset(sub_up_with, sub_up_with$sensorName=="LGE Linear Acceleration Sensor")
  sub_down_with <- subset(sub_down_with, sub_down_with$sensorName=="LGE Linear Acceleration Sensor")
}