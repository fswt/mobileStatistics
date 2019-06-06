source("./0_loading/load.R")
source("./1_preprocessing_after_loading/preprocess.R")
source("./2_exploration/draw.R")
source("./3_testing/test.R")

NUMBER_OF_DIFFERENT_TASKS = 4
TASK_NAMES <- list("up_without", "up_with", "down_without", "down_with")
par(mar = rep(2, 4))
SUBJECTS <- list("melanie", "fabian", "juan","subject1","subject2","subject3","subject4","subject5","subject6","subjectR1","subjectR2","subjectR3","subjectR4","subjectR5","subjectR6","subjectR7")
subjects_data <- rep(list(NULL), length(SUBJECTS))
subjects_subsets <- rep(list(NULL), length(SUBJECTS))

for (i in 1:length(SUBJECTS)) {
  filepath <- paste("./z_data/stairs_", SUBJECTS[[i]], "_prepro", sep = "")
  subjects_data[[i]] <- load_data(filepath)
  subjects_subsets[[i]] <- preprocess(subjects_data[[i]])
}
head(subjects_subsets[[10]]$sub_down_without_linAcc)

means_subjects_subsets <- calculate_means_all_subjects_subsets(subjects_subsets, NUMBER_OF_DIFFERENT_TASKS, SUBJECTS, TASK_NAMES)
print(means_subjects_subsets)
#creating plots
test_plot(subjects_subsets[[13]]$sub_up_without_linAcc,subjects_subsets[[13]]$sub_up_with_linAcc,"Plot_Sub4","_plot_linAcc","time","mag")
plot_histograms(subjects_subsets[[13]], SUBJECTS[[13]])
plot_ecdf(subjects_subsets[[13]])
plot_qq(subjects_subsets[[13]])
plot_box(subjects_subsets[[13]])

plot_hist_vs_ecdf(subjects_subsets[[12]][[1]], subjects_subsets[[12]][[1]])

t_test_down(means_subjects_subsets)

# stripcharts UNDER DEVELOPMENT
stripchart(round(fab_subsets$sub_up_without_linAcc$magnitude, 
  2) ~ fab_subsets$sub_up_without_linAcc$statusId, method = "stack")

stripchart(fab_subsets$sub_up_without_linAcc$magnitude ~ fab_subsets$sub_up_without_linAcc$statusId, 
  method = "jitter", jitter = 0.3)