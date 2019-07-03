source("./0_loading/load.R")
source("./1_preprocessing_after_loading/preprocess.R")
source("./2_exploration/draw.R")
source("./3_testing/test.R")

NUMBER_OF_DIFFERENT_TASKS = 6
TASK_NAMES <- list("up_without_weight", "down_without_weight", "up_light_weight", "down_light_weight", "up_heavy_weight", 
  "down_heavy_weight")
par(mar = rep(2, 4))
SUBJECTS_f <- list("subject1", "subject2", 
  "subject3", "subject4", "subject5", "subject6", "subject7", 
  "subject8")
SUBJECTS_m <- list("subject1", "subject2", 
                 "subject3", "subject4", "subject5", "subject6", "subject7", 
                 "subject8", 
                 "subject9", "subject10", "subject11", "subject12", "subject13", 
                 "subject14", "subject15")
subjects_data_f <- rep(list(NULL), length(SUBJECTS_f))
subjects_subsets_f <- rep(list(NULL), length(SUBJECTS_f))
subjects_data_m <- rep(list(NULL), length(SUBJECTS_m))
subjects_subsets_m <- rep(list(NULL), length(SUBJECTS_m))

for (i in 1:length(SUBJECTS_f)) {
  filepath_f <- paste("./z_data/stairs_f_", SUBJECTS_f[[i]], "_prepro", 
    sep = "")
  subjects_data_f[[i]] <- load_data(filepath_f)
  result_preprocessing_f <- preprocess(subjects_data_f[[i]], TASK_NAMES)
  subjects_data_f[[i]] <- result_preprocessing_f$data
  subjects_subsets_f[[i]] <- result_preprocessing_f$subsets_linAcc
}

for (i in 1:length(SUBJECTS_m)) {
  filepath_m <- paste("./z_data/stairs_m_", SUBJECTS_m[[i]], "_prepro", 
                      sep = "")
  subjects_data_m[[i]] <- load_data(filepath_m)
  result_preprocessing_m <- preprocess(subjects_data_m[[i]], TASK_NAMES)
  subjects_data_m[[i]] <- result_preprocessing_m$data
  subjects_subsets_m[[i]] <- result_preprocessing_m$subsets_linAcc
}

means_subjects_subsets_f <- calculate_means_all_subjects_subsets(subjects_subsets_f, 
                                                                 NUMBER_OF_DIFFERENT_TASKS, SUBJECTS_f, TASK_NAMES)
means_subjects_subsets_m <- calculate_means_all_subjects_subsets(subjects_subsets_m, 
  NUMBER_OF_DIFFERENT_TASKS, SUBJECTS_m, TASK_NAMES)

cor(means_subjects_subsets_f)

#creating plots
test_plot(subjects_subsets[[13]]$sub_up_without_linAcc,subjects_subsets[[13]]$sub_up_with_linAcc,"Plot_Sub4","_plot_linAcc","time","mag")
plot_histograms(subjects_subsets[[13]], SUBJECTS[[13]])
plot_ecdf(subjects_subsets[[13]])
plot_qq(subjects_subsets[[13]])
plot_box(subjects_subsets[[13]])
graphics.off()
plot(means_subjects_subsets_f[1,], col = "red", ylim=c(2,9),xaxt = "n")
axis(1, at=1:6, labels=c(TASK_NAMES[1], TASK_NAMES[2],TASK_NAMES[3],TASK_NAMES[4],TASK_NAMES[5],TASK_NAMES[6]))
points(means_subjects_subsets_f[2,], col = "green")
points(means_subjects_subsets_f[3,], col = "orange")
points(means_subjects_subsets_f[4,], col = "blue")
points(means_subjects_subsets_f[5,], col = "brown")
points(means_subjects_subsets_f[6,], col = "black")
points(means_subjects_subsets_f[7,], col = "grey")
points(means_subjects_subsets_f[8,], col = "purple")

graphics.off()



plot_box(means_subjects_subsets_f, means_subjects_subsets_m, TASK_NAMES, ylim=c(2,7))

## ATTENTION: somehow the mean of means diagram is missing -> add again
points(means_subjects_subsets_f[2,], col = "green")

# plots
test_plot(subjects_subsets_f[[1]]$sub_up_without_linAcc, subjects_subsets_f[[16]]$sub_up_with_linAcc, 
  "Plot_R7", "_plot_linAcc", "Seconds", "Acceleration Magnitude")
plot_histograms(subjects_subsets_m[[14]], ylim = c(0, 0.35), xlim = c(0, 
  25), TASK_NAMES)

plot_ecdf(subjects_subsets_m[[14]], TASK_NAMES)
plot_qq(subjects_subsets_m[[14]])
plot_box(subjects_subsets[[16]])
plot_all_stripcharts(subjects_data[[16]], method = "stack")
plot_all_stripcharts(subjects_data[[16]], method = "jitter", 
  jitter = 0.4)

plot_hist_vs_ecdf(subjects_subsets[[16]][[1]], subjects_subsets[[16]][[2]], 
  ylim = c(0, 0.35), xlim = c(0, 25), task_name_one = TASK_NAMES[[1]], 
  task_name_two = TASK_NAMES[[2]])
plot_hist_vs_qq(subjects_subsets[[16]][[1]], subjects_subsets[[16]][[2]], 
  ylim = c(0, 0.35), xlim = c(0, 25), task_name_one = TASK_NAMES[[1]], 
  task_name_two = TASK_NAMES[[2]])
plot_hist_vs_ecdf(subjects_subsets[[16]][[1]], subjects_subsets[[16]][[2]], 
  ylim = c(0, 0.35), xlim = c(0, 25), task_name_one = TASK_NAMES[[1]], 
  task_name_two = TASK_NAMES[[2]])
plot_stripchart_vs_hist(subjects_subsets[[16]][[1]], subjects_subsets[[16]][[2]], 
  ylim = c(0, 0.35), xlim = c(0, 25), task_name_one = TASK_NAMES[[1]], 
  task_name_two = TASK_NAMES[[2]], method = "stack")
plot_stripchart_vs_hist(subjects_subsets[[16]][[1]], subjects_subsets[[16]][[2]], 
  ylim = c(0, 0.35), xlim = c(0, 25), task_name_one = TASK_NAMES[[1]], 
  task_name_two = TASK_NAMES[[2]], method = "jitter", jitter = 0.5)


####################################### tests -> put in test script
t_test_down(means_subjects_subsets)

#power calculation:
a<-rbind(means_subjects_subsets_m,means_subjects_subsets_f)

#going up
#Without - Heavy
print((a[,c("up_without_weight")])-(a[,c("up_heavy_weight")]))
min_up<-min(((a[,c("up_without_weight")])-(a[,c("up_heavy_weight")])))
min_up
mean(abs((a[,c("up_without_weight")])-(a[,c("up_heavy_weight")])))

plot((a[,c("up_without_weight")])-(a[,c("up_heavy_weight")]))

#standard deviation:
sd_up<-sd((a[,c("up_without_weight")])-(a[,c("up_heavy_weight")]))
sd_up
#Two sided:
power.t.test(delta=0.2, sd=0.3, sig.level = 0.05, power=0.95)
#wilcoxon: 
wilcox.test(a[,c("up_without_weight")], a[,c("up_heavy_weight")], paired=TRUE,conf.int = TRUE)
#one sided
power.t.test(delta=0.2, sd=sd_up, sig.level = 0.05, power=0.8, alt="one.sided")
t.test(a[,c("up_without_weight")], a[,c("up_heavy_weight")],alternative="greater", paired=TRUE)

wilcox.test(a[,c("up_without_weight")], a[,c("up_heavy_weight")], paired=TRUE,conf.int = TRUE, alternative = "greater")
b<-a[,c("up_without_weight")]
c<-a[,c("up_heavy_weight")]
hist(b)
hist(c)

#Without Light:
print((a[,c("up_without_weight")])-(a[,c("up_light_weight")]))
min_up<-min(((a[,c("up_without_weight")])-(a[,c("up_light_weight")])))
min_up
mean(abs((a[,c("up_without_weight")])-(a[,c("up_light_weight")])))

plot((a[,c("up_without_weight")])-(a[,c("up_light_weight")]))

#standard deviation:
sd_up<-sd((a[,c("up_without_weight")])-(a[,c("up_light_weight")]))
sd_up
#Two sided:
power.t.test(delta=0.1, sd=0.3, sig.level = 0.05, power=0.95)
#wilcoxon: 
wilcox.test(a[,c("up_without_weight")], a[,c("up_light_weight")], paired=TRUE,conf.int = TRUE)
#one sided
power.t.test(delta=0.2, sd=sd_up, sig.level = 0.05, power=0.8, alt="one.sided")
t.test(a[,c("up_without_weight")], a[,c("up_light_weight")],alternative="greater", paired=TRUE)

wilcox.test(a[,c("up_without_weight")], a[,c("up_light_weight")], paired=TRUE,conf.int = TRUE, alternative = "greater")
b<-a[,c("up_without_weight")]
c<-a[,c("up_light_weight")]
hist(b)
hist(c)

#Light - Heavy
print((a[,c("up_light_weight")])-(a[,c("up_heavy_weight")]))
min_up<-min(((a[,c("up_light_weight")])-(a[,c("up_heavy_weight")])))
min_up
mean(abs((a[,c("up_light_weight")])-(a[,c("up_heavy_weight")])))

plot((a[,c("up_light_weight")])-(a[,c("up_heavy_weight")]))

#standard deviation:
sd_up<-sd((a[,c("up_light_weight")])-(a[,c("up_heavy_weight")]))
sd_up
#Two sided:
power.t.test(delta=0.05, sd=0.24, sig.level = 0.05, power=0.95)
#wilcoxon: 
wilcox.test(a[,c("up_light_weight")], a[,c("up_heavy_weight")], paired=TRUE,conf.int = TRUE)
#one sided
power.t.test(delta=0.2, sd=sd_up, sig.level = 0.05, power=0.8, alt="one.sided")
t.test(a[,c("up_light_weight")], a[,c("up_heavy_weight")],alternative="greater", paired=TRUE)

wilcox.test(a[,c("up_light_weight")], a[,c("up_heavy_weight")], paired=TRUE,conf.int = TRUE, alternative = "greater")
b<-a[,c("up_light_weight")]
c<-a[,c("up_heavy_weight")]
hist(b)
hist(c)



#going down
#Without heavy
print((a[,c("down_without_weight")])-(a[,c("down_heavy_weight")]))
min_down<-min(abs((a[,c("down_without_weight")])-(a[,c("down_heavy_weight")])))
mean(abs((a[,c("down_without_weight")])-(a[,c("down_heavy_weight")])))
plot((a[,c("down_without_weight")])-(a[,c("down_heavy_weight")]))
#0.0196
#standard deviation:
sd_down<-sd((a[,c("down_without_weight")])-(a[,c("down_heavy_weight")]))
#0.601
#Two sided:
power.t.test(delta=1, sd=0.6, sig.level = 0.05, power=0.95)
#wilcoxon: 
wilcox.test(a[,c("down_without_weight")], a[,c("down_heavy_weight")], paired=TRUE,conf.int = TRUE)
#one sided
power.t.test(delta=1, sd=sd_down, sig.level = 0.05, power=0.8, alt="one.sided")
t.test(a[,c("down_without_weight")], a[,c("down_heavy_weight")],alternative="greater", paired=TRUE)

wilcox.test(a[,c("down_without_weight")], a[,c("down_heavy_weight")], paired=TRUE,conf.int = TRUE, alternative = "greater")
b<-a[,c("down_without_weight")]
c<-a[,c("down_heavy_weight")]
hist(b)
hist(c)


#Without light
print((a[,c("down_without_weight")])-(a[,c("down_light_weight")]))
min_down<-min(((a[,c("down_without_weight")])-(a[,c("down_light_weight")])))
min_down
mean(abs((a[,c("down_without_weight")])-(a[,c("down_light_weight")])))
plot((a[,c("down_without_weight")])-(a[,c("down_light_weight")]))
#standard deviation:
sd_down<-sd((a[,c("down_without_weight")])-(a[,c("down_light_weight")]))
sd_down
#Two sided:
power.t.test(delta=0.2, sd=0.3, sig.level = 0.05, power=0.95)
#wilcoxon: 
wilcox.test(a[,c("down_without_weight")], a[,c("down_light_weight")], paired=TRUE,conf.int = TRUE)
#one sided
power.t.test(delta=1, sd=sd_down, sig.level = 0.05, power=0.8, alt="one.sided")
t.test(a[,c("down_without_weight")], a[,c("down_light_weight")],alternative="greater", paired=TRUE)

wilcox.test(a[,c("down_without_weight")], a[,c("down_light_weight")], paired=TRUE,conf.int = TRUE, alternative = "greater")
b<-a[,c("down_without_weight")]
c<-a[,c("down_light_weight")]
hist(b)
hist(c)



#light heavy
print((a[,c("down_light_weight")])-(a[,c("down_heavy_weight")]))
min_down<-min(((a[,c("down_light_weight")])-(a[,c("down_heavy_weight")])))
min_down
mean(abs((a[,c("down_light_weight")])-(a[,c("down_heavy_weight")])))
plot((a[,c("down_light_weight")])-(a[,c("down_heavy_weight")]))
#standard deviation:
sd_down<-sd((a[,c("down_light_weight")])-(a[,c("down_heavy_weight")]))
sd_down
#Two sided:
power.t.test(delta=0.8, sd=0.5, sig.level = 0.05, power=0.8)
#wilcoxon: 
wilcox.test(a[,c("down_light_weight")], a[,c("down_heavy_weight")], paired=TRUE,conf.int = TRUE)
#one sided
power.t.test(delta=1, sd=sd_down, sig.level = 0.05, power=0.8, alt="one.sided")
t.test(a[,c("down_light_weight")], a[,c("down_heavy_weight")],alternative="greater", paired=TRUE)

wilcox.test(a[,c("down_light_weight")], a[,c("down_heavy_weight")], paired=TRUE,conf.int = TRUE, alternative = "greater")
b<-a[,c("down_light_weight")]
c<-a[,c("down_heavy_weight")]
hist(b)
hist(c)

#test for Trend 
#Going Down: 
plot(a[,c("down_without_weight")])
plot(a[,c("down_light_weight")])
plot(a[,c("down_heavy_weight")])
positive_down_without_weight=0
positive_down_light_weight=0
positive_down_heavy_weight=0
#Threshold for  going down 5.5
for(i in 1:nrow(a)){ 
  if(a[i,c("down_without_weight")]>5.5){
  positive_down_without_weight= positive_down_without_weight+1
  }
  if(a[i,c("down_light_weight")]>5.5){
    positive_down_light_weight= positive_down_light_weight+1
  }
  if(a[i,c("down_heavy_weight")]>5.5){
    positive_down_heavy_weight= positive_down_heavy_weight+1
  }
} 
print(positive_down_light_weight)
x <- matrix( nrow=2, ncol=3)
x
x[1,1]=positive_down_without_weight
x[2,1]=22
x[1,2]=positive_down_light_weight
x[2,2]=22
x[1,3]=positive_down_heavy_weight
x[2,3]=22
positive<-(x[1,])
negative<-(x[2,])
prop.test(positive,negative)

#Going Up: 
plot(a[,c("up_without_weight")])
plot(a[,c("up_light_weight")])
plot(a[,c("up_heavy_weight")])
positive_up_without_weight=0
positive_up_light_weight=0
positive_up_heavy_weight=0
#Threshold for  going up 5.5
for(i in 1:nrow(a)){ 
  if(a[i,c("up_without_weight")]>5.5){
    positive_up_without_weight= positive_up_without_weight+1
  }
  if(a[i,c("up_light_weight")]>5.5){
    positive_up_light_weight= positive_up_light_weight+1
  }
  if(a[i,c("up_heavy_weight")]>5.5){
    positive_up_heavy_weight= positive_up_heavy_weight+1
  }
} 
print(positive_up_light_weight)
x <- matrix( nrow=2, ncol=3)
x
x[1,1]=positive_up_without_weight
x[2,1]=22
x[1,2]=positive_up_light_weight
x[2,2]=22
x[1,3]=positive_up_heavy_weight
x[2,3]=22
positive<-(x[1,])
negative<-(x[2,])
prop.test(positive,negative)

#F-test: 
#going down without and heavy weight
means_down_without<-(a[1:10,c("down_without_weight")])
means_down_heavy<-(a[11:20,c("down_heavy_weight")])
var.test(means_down_without, means_down_heavy)

#going up without and heavy weight
means_up_without<-(a[1:10,c("up_without_weight")])
means_up_heavy<-(a[11:20,c("up_heavy_weight")])
var.test(means_up_without, means_up_heavy)
