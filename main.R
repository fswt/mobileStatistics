source("./0_loading/load.R")
source("./1_preprocessing_after_loading/preprocess.R")
source("./2_exploration/draw.R")
source("./3_testing/test.R")

NUMBER_OF_DIFFERENT_TASKS = 4
par(mar = rep(2, 4))
subjects <- list("melanie", "fabian", "juan","subject1","subject2","subject3","subject4","subject5","subject6")
subjects_data <- rep(list(NULL), length(subjects))
subjects_subsets <- rep(list(NULL), length(subjects))

for (i in 1:length(subjects)) {
  filepath <- paste("./z_data/stairs_", subjects[[i]], "_prepro", sep = "")
  subjects_data[[i]] <- load_data(filepath)
  subjects_subsets[[i]] <- preprocess(subjects_data[[i]])
}

calculate_means_all_subjects_subsets <- calculate_means_all_subjects_subsets(subjects_subsets, NUMBER_OF_DIFFERENT_TASKS)

#creating plots
test_plot(subjects_subsets[[5]]$sub_up_without_linAcc,subjects_subsets[[5]]$sub_up_with_linAcc,"Plot_Sub4","_plot_linAcc","time","mag")
plot_histograms(subjects_subsets[[1]], subjects[[1]])
plot_ecdf(subjects_subsets[[1]])
plot_qq(subjects_subsets[[1]])
plot_box(subjects_subsets[[1]])

plot_hist_vs_ecdf(subjects_subsets[[1]][[1]], subjects_subsets[[1]][[2]])

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
