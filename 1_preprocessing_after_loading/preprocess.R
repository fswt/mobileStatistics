preprocess <- function(data, TASK_NAMES) {
  attach(data)
  data <- rename_labels(data, TASK_NAMES)  # Why do we have to return, so that a change happens? Variable is global, right? (also: #assign to parent)
  data <- calculate_magnitude(data)  #assign to parent
  subsets <- create_subsets(data, TASK_NAMES)
  subsets <- normalize_timestamps(subsets)
  subsets_linAcc <- create_sensor_name_subsets(subsets)
  detach(data)
  result = list(data = data, subsets_linAcc = subsets_linAcc)
  return(result)
}

rename_labels <- function(data, TASK_NAMES) {
  data$statusId <- factor(data$statusId, labels = TASK_NAMES)
  return(data)
}

calculate_magnitude <- function(data) {
  data$magnitude = sqrt(x^2 + y^2 + z^2)
  return(data)
}

create_subsets <- function(data, TASK_NAMES) {
  sub_up_without <- subset(data, statusId == TASK_NAMES[[1]])
  sub_down_without <- subset(data, statusId == TASK_NAMES[[2]])
  sub_up_light <- subset(data, statusId == TASK_NAMES[[3]])
  sub_down_light <- subset(data, statusId == TASK_NAMES[[4]])
  sub_up_heavy <- subset(data, statusId == TASK_NAMES[[5]])
  sub_down_heavy <- subset(data, statusId == TASK_NAMES[[6]])
  result <- list(sub_up_without = sub_up_without, sub_down_without = sub_down_without, 
    sub_up_light = sub_up_light, sub_down_light = sub_down_light, sub_up_heavy = sub_up_heavy, sub_down_heavy = sub_down_heavy)
  return(result)
}

normalize_timestamps <- function(subsets) {
  for(i in 1:length(subsets)){
    subset = subsets[[i]]
    mt <- min(subset$timestamp)
    subset$timestamp <- (subset$timestamp - mt)
    # cut the first 2 end the 2 Last Seconds
    subset <- subset(subset, subset$timestamp > 2000)
    max_time <- max(subset$timestamp)
    subset <- subset(subset, subset$timestamp < (max_time - 2000))
    subsets[[i]] = subset
  }
  # cut standing around --> maybe we can it make later better than just always cutting 2 sec
  # CUT_FRONT_SEC = 5
  # CUT_BACK_SEC = 4.5
  # subsets$sub_up_without <- subset(subsets$sub_up_without, subsets$sub_up_without$timestamp > (CUT_FRONT_SEC * 1000))
  # max_time <- max(subsets$sub_up_without$timestamp)
  # subsets$sub_up_without <- subset(subsets$sub_up_without, subsets$sub_up_without$timestamp < (max_time - (CUT_BACK_SEC * 1000)))
  return(subsets)
}

create_sensor_name_subsets <- function(subsets) {
  for(i in 1:length(subsets)){
    subset = subsets[[i]]
    subset <- subset(subset, subset$sensorName == "LGE Linear Acceleration Sensor")
    subsets[[i]] = subset
  }
  return(subsets)
}

calculate_means_all_subjects_subsets <- function(subjects_subsets, 
  NUMBER_OF_DIFFERENT_TASKS, SUBJECTS, TASK_NAMES) {
  subjects_subsets_means <- matrix(nrow = length(subjects_subsets), 
    ncol = NUMBER_OF_DIFFERENT_TASKS)
  rownames(subjects_subsets_means) <- SUBJECTS
  colnames(subjects_subsets_means) <- TASK_NAMES
  
  i <- 1
  for (subject_subsets in subjects_subsets) {
    j <- 1
    subject_subsets
    for (subset in subject_subsets) {
      subjects_subsets_means[i, j] = mean(subset$magnitude)
      j <- j + 1
    }
    i <- i + 1
  }
  return(subjects_subsets_means)
}