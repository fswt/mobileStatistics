source("./0_loading/load.R")
source("./1_preprocessing_after_loading/preprocess.R")
source("./2_exploration/draw.R")
source("./3_testing/test.R")

NUMBER_OF_DIFFERENT_TASKS = 4
subjects <- list("melanie", "fabian", "juan","subject1","subject2","subject3","subject4","subject5","subject6")
subjects_data <- rep(list(NULL), length(subjects))
subjects_subsets <- rep(list(NULL), length(subjects))

for (i in 1:length(subjects)) {
  filepath <- paste("./z_data/stairs_", subjects[[i]], "_prepro", sep = "")
  subjects_data[[i]] <- load_data(filepath)
  subjects_subsets[[i]] <- preprocess(subjects_data[[i]])
}

###EXAMPLE
addition = "_plot_linAcc"
png_name = paste(subjects[[i]], addition, sep="")
###

#creating plots
test_plot(subjects_subsets[[5]]$sub_up_without_linAcc,subjects_subsets[[5]]$sub_up_with_linAcc,"Plot_Sub4","_plot_linAcc","time","mag")
head(subjects_subsets[[7]])

subjects_subsets_means <- matrix(nrow=length(subjects_subsets),ncol=NUMBER_OF_DIFFERENT_TASKS)

i <- 1
for (subject_subsets in subjects_subsets) {
  j <- 1
  subject_subsets
  for (subset in subject_subsets) {
    subjects_subsets_means[j,i] = mean(subset$magnitude)
    j <- j+1
  }
  i <- i+1
}

# plot histograms
par(mfrow = c(2, 2))
test_hist(fab_subsets$sub_up_without_linAcc, "fab_hist_up_without_mag")
test_hist(fab_subsets$sub_up_with_linAcc, "fab_hist_up_with_mag")
test_hist(fab_subsets$sub_down_without_linAcc, "fab_hist_down_without_mag")
test_hist(fab_subsets$sub_down_with_linAcc, "fab_hist_down_with_mag")

# plot ECDF
plot.ecdf(fab_subsets$sub_up_without_linAcc$magnitude)
plot.ecdf(fab_subsets$sub_up_with_linAcc$magnitude)
plot.ecdf(fab_subsets$sub_down_without_linAcc$magnitude)
plot.ecdf(fab_subsets$sub_down_with_linAcc$magnitude)

# Q-QPlot:
qqnorm(fab_subsets$sub_up_without_linAcc$magnitude)
qqnorm(fab_subsets$sub_up_with_linAcc$magnitude)
qqnorm(fab_subsets$sub_down_without_linAcc$magnitude)
qqnorm(fab_subsets$sub_down_with_linAcc$magnitude)

# boxplot
box_plots(fab_subsets$sub_up_without_linAcc, fab_subsets$sub_up_with_linAcc, 
  fab_subsets$sub_down_without_linAcc, fab_subsets$sub_down_with_linAcc)

# hist vs. ECDF
par(mfrow = c(2, 2))
test_hist(fab_subsets$sub_up_without_linAcc, "fab_hist_up_without_mag")
plot.ecdf(fab_subsets$sub_up_without_linAcc$magnitude)
test_hist(fab_subsets$sub_up_with_linAcc, "fab_hist_up_with_mag")
plot.ecdf(fab_subsets$sub_up_with_linAcc$magnitude)

# stripcharts under development
stripchart(round(fab_subsets$sub_up_without_linAcc$magnitude, 
  2) ~ fab_subsets$sub_up_without_linAcc$statusId, method = "stack")

stripchart(fab_subsets$sub_up_without_linAcc$magnitude ~ fab_subsets$sub_up_without_linAcc$statusId, 
  method = "jitter", jitter = 0.3)


# T test down
mean_down_with <- c(mean(fab_subsets$sub_down_with_linAcc$magnitude), 
  mean(mel_subsets$sub_down_with_linAcc$magnitude), mean(jua_subsets$sub_down_with_linAcc$magnitude))
mean_down_without <- c(mean(fab_subsets$sub_down_without_linAcc$magnitude), 
  mean(mel_subsets$sub_down_without_linAcc$magnitude), mean(jua_subsets$sub_down_without_linAcc$magnitude))
# t test:
t.test(mean_down_with, mean_down_without, alternative = "less", 
  paired = TRUE)

# T test up
mean_up_with <- c(mean(fab_subsets$sub_up_with_linAcc$magnitude), 
  mean(mel_subsets$sub_up_with_linAcc$magnitude), mean(jua_subsets$sub_up_with_linAcc$magnitude))
mean_down_without <- c(mean(fab_subsets$sub_up_without_linAcc$magnitude), 
  mean(mel_subsets$sub_up_without_linAcc$magnitude), mean(jua_subsets$sub_up_without_linAcc$magnitude))
# t test:
t.test(mean_down_without, mean_up_with, alternative = "less", 
  paired = TRUE)
