
test_plot <- function(subset_one, subset_two, pic_name, plot_name, 
  x_lab, y_lab) {
  graphics.off()
  plot(subset_one$timestamp/1000, subset_one$magnitude, type = "l", 
    col = "red", xlab = x_lab, ylab = y_lab)
  lines(subset_two$timestamp/1000, subset_two$magnitude, type = "l", 
    col = "green")
  path = paste("./graphs/", pic_name, ".pdf", sep="")
  dev.copy(pdf, path)
  dev.off()
}

test_hist <- function(subset, task_name, xlim, ylim) {
  
  hist(subset$magnitude, xlab = "Acceleration Magnitude", prob = T, main=task_name, xlim=xlim, ylim=ylim, breaks=20)
  curve(dnorm(x, mean = mean(subset$magnitude), sd = sd(subset$magnitude)), 
    add = TRUE)
  path = paste("./graphs/hist_", task_name, ".pdf", sep="")
  dev.copy(pdf, path)
  dev.off()
}

histogram_linear_magnitude <- function(subset_one, subset_two, 
  subset_three, subset_four) {
  par(mfcol = c(2, 2))
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

plot_box <- function(subject_subsets) {
  plot_name <- list("up_without_weight", "up_with_weight", "down_without_weight", "down_with_weight")
  par(mfcol = c(2, 2))
  
  #ATTENTION: BEFORE THE ORDER WAS TWO, FOUR, ONE, THREE <= Did that matter?
  #"sub_up_without" <= What did sub mean?
  i <- 1
  for (subset in subject_subsets){
    boxplot(subset$magnitude, main = plot_name[[i]], ylim = c(0, 
    25))
    i <- i+1
  }
  path = paste("./graphs/box_all.pdf", sep="")
  dev.copy(pdf, path)
  dev.off()
}

plot_histograms <- function(subject_subsets, xlim, ylim){
  task_name <- list("up_without_weight", "up_with_weight", "down_without_weight", "down_with_weight")
  par(mfrow = c(2, 2))
  
  i <- 1
  for (subset in subject_subsets){
    test_hist(subset, task_name[[i]], xlim, ylim)
    i <- i+1
  }
}

plot_ecdf <- function(subject_subsets){
  par(mfrow = c(2, 2))
  plot_name <- list("up_without_weight", "up_with_weight", "down_without_weight", "down_with_weight")
  
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
  par(mfrow = c(2, 2))
  plot_name <- list("up_without_weight", "up_with_weight", "down_without_weight", "down_with_weight")
  
  i <- 1
  for (subset in subject_subsets){
    qqnorm(subset$magnitude, main=plot_name[i])
    i <- i+1
  }
  path = paste("./graphs/qq_all.pdf", sep="")
  dev.copy(pdf, path)
  dev.off()
}

plot_hist_vs_ecdf <- function(subset_one, subset_two, task_name_one="tmp_plot", task_name_two="tmp_plot", ylim, xlim){
  par(mfrow = c(2, 2))
  
  test_hist(subset_one, xlim=xlim, ylim=ylim, task_name_one)
  test_hist(subset_two, xlim=xlim, ylim=ylim, task_name_two)
  plot.ecdf(subset_one$magnitude, xlim=c(0,25), xlab="Acceleration Magnitude", ylab="Fn(Acceleration Magnitude)", main="")
  plot.ecdf(subset_two$magnitude, xlim=c(0,25), xlab="Acceleration Magnitude", ylab="Fn(Acceleration Magnitude)", main="")

  path = paste("./graphs/hist_ecdf_",task_name_one, "_", task_name_two, ".pdf", sep="")
  dev.copy(pdf, path)
  dev.off()
}

plot_hist_vs_qq <- function(subset_one, subset_two, task_name_one, task_name_two, xlim, ylim){
  par(mfrow = c(2, 2))
  
  test_hist(subset_one, xlim=xlim, ylim=ylim, task_name_one)
  test_hist(subset_two, xlim=xlim, ylim=ylim, task_name_two)
  qqnorm(subset_one$magnitude, main="")
  qqnorm(subset_two$magnitude, main="")

  path = paste("./graphs/hist_qq_",task_name_one, "_", task_name_two, ".pdf", sep="")
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
    figure <- stripchart(round(subject_data$magnitude, 
                   2) ~ subject_data$statusId, xlab="Acceleration Magnitude", method = "stack")
  }
  if (method=="jitter"){
    figure <- stripchart(subject_data$magnitude ~ subject_data$statusId, xlab="Acceleration Magnitude",
               method = "jitter", jitter=jitter)
  }
  dev.off()
}