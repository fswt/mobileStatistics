preprocess <- function(data) {
  data <- rename_labels(data)  # Why do we have to return, so that a change happens? Variable is global, right? (also: #assign to parent)
  data <- calculate_magnitude(data)  #assign to parent
  subsets <- create_subsets(data)
  subsets <- normalize_timestamps(subsets$sub_up_without, subsets$sub_down_without, 
    subsets$sub_up_with, subsets$sub_down_with)
  subsets_linAcc <- create_sensor_name_subsets(subsets$sub_up_without, 
    subsets$sub_down_without, subsets$sub_up_with, subsets$sub_down_with)
  return(subsets_linAcc)
}

rename_labels <- function(data) {
  data$statusId <- factor(data$statusId, labels = c("Stairs Up without weight", 
    "Stairs Down without weight", "Stairs Up with weight", 
    "Stairs Down with weight"))
  return(data)
}

calculate_magnitude <- function(data) {
  data$magnitude = sqrt(x^2 + y^2 + z^2)
  return(data)
}

create_subsets <- function(data) {
  sub_up_without <- subset(data, statusId == "Stairs Up without weight")
  sub_down_without <- subset(data, statusId == "Stairs Down without weight")
  sub_up_with <- subset(data, statusId == "Stairs Up with weight")
  sub_down_with <- subset(data, statusId == "Stairs Down with weight")
  result <- list(sub_up_without = sub_up_without, sub_down_without = sub_down_without, 
    sub_up_with = sub_up_with, sub_down_with = sub_down_with)
  return(result)
}

normalize_timestamps <- function(sub_up_without, sub_down_without, 
  sub_up_with, sub_down_with) {
  mt <- min(sub_up_without$timestamp)
  sub_up_without$timestamp <- (sub_up_without$timestamp - mt)
  
  mt <- min(sub_down_without$timestamp)
  sub_down_without$timestamp <- (sub_down_without$timestamp - 
    mt)
  
  mt <- min(sub_up_with$timestamp)
  sub_up_with$timestamp <- (sub_up_with$timestamp - mt)
  
  mt <- min(sub_down_with$timestamp)
  sub_down_with$timestamp <- (sub_down_with$timestamp - mt)
  
  result <- list(sub_up_without = sub_up_without, sub_down_without = sub_down_without, 
    sub_up_with = sub_up_with, sub_down_with = sub_down_with)
  return(result)
}

create_sensor_name_subsets <- function(sub_up_without, sub_down_without, 
  sub_up_with, sub_down_with) {
  sub_up_without <- subset(sub_up_without, sub_up_without$sensorName == 
    "LGE Linear Acceleration Sensor")
  sub_down_without <- subset(sub_down_without, sub_down_without$sensorName == 
    "LGE Linear Acceleration Sensor")
  sub_up_with <- subset(sub_up_with, sub_up_with$sensorName == 
    "LGE Linear Acceleration Sensor")
  sub_down_with <- subset(sub_down_with, sub_down_with$sensorName == 
    "LGE Linear Acceleration Sensor")
  result <- list(sub_up_without_linAcc = sub_up_without, sub_down_without_linAcc = sub_down_without, 
    sub_up_with_linAcc = sub_up_with, sub_down_with_linAcc = sub_down_with)
  return(result)
}