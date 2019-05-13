test_plot <- function(sub_up_without, sub_up_with, pic_Name){
 
  plot(sub_up_without$timestamp,sub_up_without$magnitude,type="l",col="red")
  lines(sub_up_with$timestamp,sub_up_with$magnitude,type="l",col="green")
  
  }

test_hist <- function(sub_down_without, sub_down_with){
  hist(sub_down_without$mag, xlab="Acc.Magn.",prob=T)
  hist(sub_down_with$mag, xlab="Acc.Magn.",prob=T)
}

histogram_linear_magnitude <- function(sub_up_without, sub_down_without, sub_up_with, sub_down_with){
  par(mfcol=c(2,2))
  hist(sub_down_without[sub_down_without$sensorName=="LGE Linear Acceleration Sensor",]$mag, breaks=c(0:50),ylim =c(0,0.500) , xlab="Acc.Magn.",prob=T)
  hist(sub_down_with[sub_down_with$sensorName=="LGE Linear Acceleration Sensor",]$mag,breaks=c(0:50),ylim =c(0,0.5), xlab="Acc.Magn",prob=T)
  hist(sub_up_without[sub_up_without$sensorName=="LGE Linear Acceleration Sensor",]$mag, breaks=c(0:50),ylim =c(0,0.500) , xlab="Acc.Magn.",prob=T)
  hist(sub_up_with[sub_up_with$sensorName=="LGE Linear Acceleration Sensor",]$mag,breaks=c(0:50),ylim =c(0,0.5), xlab="Acc.Magn",prob=T)
}

compare_graph_lines <- function(sub_up_without, sub_down_without, sub_up_with, sub_down_with){
  par(mfcol=c(2,1))
  plot(sub_down_without$timestamp,sub_down_without$mag, type="l",col="red")
  lines(sub_down_with$timestamp,sub_down_with$mag,col="green")
  plot(sub_up_without$timestamp,sub_up_without$mag, type="l",col="red")
  lines(sub_up_with$timestamp,sub_up_with$mag,col="green")
}

box_plots <- function(sub_up_without, sub_down_without, sub_up_with, sub_down_with){
  par(mfcol=c(2,2))
  boxplot(sub_down_without$mag, main="sub_down_without", ylim =c(0,40))
  boxplot(sub_down_with$mag, main="sub_down_with", ylim =c(0,40))
  boxplot(sub_up_without$mag, main ="sub_up_without", ylim =c(0,40))
  boxplot(sub_up_with$mag, main="sub_down_with", ylim =c(0,40))
}