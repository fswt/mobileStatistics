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

points(means_subjects_subsets_f[2,], col = "green")

# plots
test_plot(subjects_subsets[[16]]$sub_up_without_linAcc, subjects_subsets[[16]]$sub_up_with_linAcc, 
  "Plot_R7", "_plot_linAcc", "Seconds", "Acceleration Magnitude")
plot_histograms(subjects_subsets[[16]], ylim = c(0, 0.35), xlim = c(0, 
  25))
plot_ecdf(subjects_subsets[[16]])
plot_qq(subjects_subsets[[16]])
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


# tests
t_test_down(means_subjects_subsets)

