
test_plot <- function(subset_one, subset_two, pic_name, plot_name, 
  x_lab, y_lab) {
  plot(subset_one$timestamp, subset_one$magnitude, type = "l", 
    col = "red", main = plot_name, xlab = x_lab, ylab = y_lab)
  lines(subset_two$timestamp, subset_two$magnitude, type = "l", 
    col = "green")
  path = paste("./graphs/", pic_name, ".png", sep="")
  dev.copy(png, path)
  dev.off()
}

test_hist <- function(subset_one, pic_name) {
  hist(subset_one$magnitude, xlab = "Acc.Magn.", prob = T)
  curve(dnorm(x, mean = mean(subset_one$magnitude), sd = sd(subset_one$magnitude)), 
    add = TRUE)
  path = paste("./graphs/", pic_name, ".png", sep="")
  dev.copy(png, path)
  dev.off()
}

histogram_linear_magnitude <- function(subset_one, subset_two, 
  subset_three, subset_four) {
  par(mfcol = c(2, 2))
  hist(subset_two[subset_two$sensorName == "LGE Linear Acceleration Sensor", 
    ]$magnitude, breaks = c(0:50), ylim = c(0, 0.5), xlab = "Acc.Magn.", 
    prob = T)
  hist(subset_four[subset_four$sensorName == "LGE Linear Acceleration Sensor", 
    ]$magnitude, breaks = c(0:50), ylim = c(0, 0.5), xlab = "Acc.Magn", 
    prob = T)
  hist(subset_one[subset_one$sensorName == "LGE Linear Acceleration Sensor", 
    ]$magnitude, breaks = c(0:50), ylim = c(0, 0.5), xlab = "Acc.Magn.", 
    prob = T)
  hist(subset_three[subset_three$sensorName == "LGE Linear Acceleration Sensor", 
    ]$magnitude, breaks = c(0:50), ylim = c(0, 0.5), xlab = "Acc.Magn", 
    prob = T)
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

box_plots <- function(subset_one, subset_two, subset_three, subset_four) {
  par(mfcol = c(2, 2))
  boxplot(subset_two$magnitude, main = "sub_up_without", ylim = c(0, 
    40))
  boxplot(subset_four$magnitude, main = "sub_up_with", ylim = c(0, 
    40))
  boxplot(subset_one$magnitude, main = "sub_down_without", 
    ylim = c(0, 40))
  boxplot(subset_three$magnitude, main = "sub_down_with", ylim = c(0, 
    40))
}