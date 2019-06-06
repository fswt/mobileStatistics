source("./0_loading/load.R")
source("./1_preprocessing_after_loading/preprocess.R")
source("./2_exploration/draw.R")
source("./3_testing/test.R")

NUMBER_OF_DIFFERENT_TASKS = 4
TASK_NAMES <- list("up_without_weight", "up_with_weight", "down_without_weight", 
  "down_with_weight")
par(mar = rep(2, 4))
SUBJECTS <- list("melanie", "fabian", "juan", "subject1", "subject2", 
  "subject3", "subject4", "subject5", "subject6", "subjectR1", 
  "subjectR2", "subjectR3", "subjectR4", "subjectR5", "subjectR6", 
  "subjectR7")
subjects_data <- rep(list(NULL), length(SUBJECTS))
subjects_subsets <- rep(list(NULL), length(SUBJECTS))

for (i in 1:length(SUBJECTS)) {
  filepath <- paste("./z_data/stairs_", SUBJECTS[[i]], "_prepro", 
    sep = "")
  subjects_data[[i]] <- load_data(filepath)
  result_preprocessing <- preprocess(subjects_data[[i]])
  subjects_data[[i]] <- result_preprocessing$data
  subjects_subsets[[i]] <- result_preprocessing$subsets_linAcc
}
means_subjects_subsets <- calculate_means_all_subjects_subsets(subjects_subsets, 
  NUMBER_OF_DIFFERENT_TASKS, SUBJECTS, TASK_NAMES)


means_subjects_subsets <- calculate_means_all_subjects_subsets(subjects_subsets, NUMBER_OF_DIFFERENT_TASKS, SUBJECTS, TASK_NAMES)
print(means_subjects_subsets)
#creating plots
test_plot(subjects_subsets[[13]]$sub_up_without_linAcc,subjects_subsets[[13]]$sub_up_with_linAcc,"Plot_Sub4","_plot_linAcc","time","mag")
plot_histograms(subjects_subsets[[13]], SUBJECTS[[13]])
plot_ecdf(subjects_subsets[[13]])
plot_qq(subjects_subsets[[13]])
plot_box(subjects_subsets[[13]])


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

