test_plot <- function(subset_one, subset_two, pic_name, plot_name, 
  x_lab, y_lab) {
  graphics.off()
  plot(subset_one$timestamp/1000, subset_one$magnitude, type = "l", 
    col = "red", xlab = x_lab, ylab = y_lab)
  #lines(subset_two$timestamp/1000, subset_two$magnitude, type = "l", 
    #col = "green")
  path = paste("./graphs/", pic_name, ".pdf", sep="")
  dev.copy(pdf, path)
  dev.off()
}

test_hist <- function(subset, TASK_NAMES, xlim, ylim) {
  # WHAT DOES BREAKS EXACTLY DOES?
  hist(subset$magnitude, xlab = "Acceleration Magnitude", prob = T, main=TASK_NAMES, xlim=xlim, ylim=ylim, breaks=c(0:75))
  curve(dnorm(x, mean = mean(subset$magnitude), sd = sd(subset$magnitude)), 
    add = TRUE)
  path = paste("./graphs/hist_", TASK_NAMES, ".pdf", sep="")
  dev.copy(pdf, path)
  dev.off()
}

histogram_linear_magnitude <- function(subset_one, subset_two, 
  subset_three, subset_four) {
  par(mfcol = c(2, 3))
  hist(subset_two[subset_two$sensorName == "Linear Acceleration", 
    ]$magnitude, breaks = c(0:50), ylim = c(0, 0.5), xlab = "Acceleration Magnitude", 
    prob = T, main="")
  hist(subset_four[subset_four$sensorName == "Linear Acceleration", 
    ]$magnitude, breaks = c(0:50), ylim = c(0, 0.5), xlab = "Acceleration Magnitude", 
    prob = T, main="")
  hist(subset_one[subset_one$sensorName == "Linear Acceleration", 
    ]$magnitude, breaks = c(0:50), ylim = c(0, 0.5), xlab = "Acceleration Magnitude", 
    prob = T, main="")
  hist(subset_three[subset_three$sensorName == "Linear Acceleration", 
    ]$magnitude, breaks = c(0:50), ylim = c(0, 0.5), xlab = "Acceleration Magnitude", 
    prob = T, main="")
}

compare_graph_lines <- function(subset_one, subset_two, subset_three, 
  subset_four) {
  par(mfcol = c(2, 1))
  plot(subset_two$timestamp, subset_two$magnitude, type = "l", 
    col = "red")
  lines(subset_four$timestamp, subset_four$magnitude, col = "green")
  plot(subset_one$timestamp, subset_one$magnitude, type = "l", 
    col = "red")
  lines(subset_three$timestamp, subset_three$magnitude, col = "green")
}

plot_box <- function(matrix1, matrix2, TASK_NAMES, ylim=c(0,25), col="blue", boxwex=1) {
  plot_name <- TASK_NAMES
  par(mfcol = c(1, 6))
  
  #ATTENTION: BEFORE THE ORDER WAS TWO, FOUR, ONE, THREE <= Did that matter?
  #"sub_up_without" <= What did sub mean?
  for (j in 1:length(TASK_NAMES)){
    d0 <- matrix(data=NA,nrow=15,ncol=2)
    colnames(d0) <- c("female", "male")
    for (i in 1:8){
      d0[i,1] <- matrix1[i,j]
    }
    d0[,2] <- matrix2[,j]
    boxplot(d0, main = plot_name[[j]], ylim = ylim, col = col, boxwex=boxwex) # removed $magnitude to enable in a more general case
  }
  path = paste("./graphs/box_all.pdf", sep="")
  dev.copy(pdf, path)
  dev.off()
}

plot_histograms <- function(subject_subsets, xlim, ylim, TASK_NAMES){
  par(mfrow = c(2, 3))
  
  i <- 1
  for (subset in subject_subsets){
    test_hist(subset, TASK_NAMES[[i]], xlim, ylim)
    i <- i+1
  }
}

plot_ecdf <- function(subject_subsets, TASK_NAMES){
  par(mfrow = c(2, 3))
  plot_name <- TASK_NAMES
  
  i <- 1
  for (subset in subject_subsets){
    plot.ecdf(subset$magnitude, main=plot_name[[i]], xlim=c(0,25), xlab="Acceleration Magnitude", ylab="Fn(Acceleration Magnitude)")
    i <- i+1
  }
  path = paste("./graphs/ecdf_all.pdf", sep="")
  dev.copy(pdf, path)
  dev.off()
}

plot_qq <- function(subject_subsets){
  par(mfrow = c(2, 3))
  plot_name <- TASK_NAMES
  
  i <- 1
  for (subset in subject_subsets){
    qqnorm(subset$magnitude, main=plot_name[i])
    i <- i+1
  }
  path = paste("./graphs/qq_all.pdf", sep="")
  dev.copy(pdf, path)
  dev.off()
}

plot_all_stripcharts <- function(subject_data, method, jitter=0.3){
  path = paste("./graphs/strip_all_", method, ".pdf", sep="")
  pdf(path, height=35, width=20)
  par(cex.lab=3)
  par(cex.axis=3)
  par(mar=c(10,4,10,1)+.1)
  if (method=="stack"){
    stripchart(round(subject_data$magnitude, 
                               2) ~ subject_data$statusId, xlab="Acceleration Magnitude", method = "stack")
  }
  if (method=="jitter"){
    stripchart(subject_data$magnitude ~ subject_data$statusId, xlab="Acceleration Magnitude",
                         method = "jitter", jitter=jitter)
  }
  dev.off()
}

plot_hist_vs_ecdf <- function(subset_one, subset_two, task_name_one="tmp_plot", task_name_two="tmp_plot", ylim, xlim){
  par(mfrow = c(2, 3))
  
  test_hist(subset_one, xlim=xlim, ylim=ylim, task_name_one)
  test_hist(subset_two, xlim=xlim, ylim=ylim, task_name_two)
  plot.ecdf(subset_one$magnitude, xlim=c(0,25), xlab="Acceleration Magnitude", ylab="Fn(Acceleration Magnitude)", main="")
  plot.ecdf(subset_two$magnitude, xlim=c(0,25), xlab="Acceleration Magnitude", ylab="Fn(Acceleration Magnitude)", main="")

  path = paste("./graphs/hist_ecdf_",task_name_one, "_", task_name_two, ".pdf", sep="")
  dev.copy(pdf, path)
  dev.off()
}

plot_hist_vs_qq <- function(subset_one, subset_two, task_name_one, task_name_two, xlim, ylim){
  par(mfrow = c(2, 3))
  
  test_hist(subset_one, xlim=xlim, ylim=ylim, task_name_one)
  test_hist(subset_two, xlim=xlim, ylim=ylim, task_name_two)
  qqnorm(subset_one$magnitude, main="")
  qqnorm(subset_two$magnitude, main="")

  path = paste("./graphs/hist_qq_",task_name_one, "_", task_name_two, ".pdf", sep="")
  dev.copy(pdf, path)
  dev.off()
}

plot_stripchart_vs_hist <- function(subset_one, subset_two, task_name_one, task_name_two, xlim, ylim, method, jitter, TASK_NAMES){
  par(mfrow = c(2, 3))
  par(mar=c(4,4,4,1)+.1)
  subsets = list(subset_one=subset_one, subset_two=subset_two)
  task_names = list(task_name_one=task_name_one, task_name_two=task_name_two)
  i <- 1
  for (subset in subsets){
    test_hist(subset, xlim=xlim, ylim=ylim, TASK_NAMES[[i]])
    i <- i+1
  }
  i <- 1
  for (subset in subsets){
    if (method=="stack"){
      stripchart(round(subset$magnitude, 
                                 2), xlim = xlim, xlab="Acceleration Magnitude", method = "stack")
    }
    if (method=="jitter"){
      stripchart(subset$magnitude, xlim = xlim, xlab="Acceleration Magnitude",
                           method = "jitter", jitter=jitter)
    }
    i <- i+1
  }
  path = paste("./graphs/hist_strip_", method, "_", task_name_one, "_", task_name_two, ".pdf", sep="")
  dev.copy(pdf, path)
  dev.off()
}

