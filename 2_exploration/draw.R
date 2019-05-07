#TestPlot
plot(sub_up_without$timestamp,sub_up_without$magnitude,type="l",col="red")
lines(sub_up_with$timestamp,sub_up_with$magnitude,type="l",col="green")

#Test hist
hist(sub_down_without$mag, xlab="Acc.Magn.",prob=T)
hist(sub_down_with$mag, xlab="Acc.Magn.",prob=T)