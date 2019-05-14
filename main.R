source("./0_loading/load.R")
source("./1_preprocessing_after_loading/preprocess.R")
source("./2_exploration/draw.R")
source("./3_testing/test.R")

subjects <- list("melanie", "fabian", "juan")
subject_data <- list(length=length(subjects))
subject_subsets <- list(length=length(subjects))

for (i in 1:length(subjects)) {
  filepath <- paste("./z_data/stairs_", subjects[i], "_prepro", sep="")
  subject_data[i] <- load_data(filepath)
  subject_subsets[i] <- preprocess(subject_data[i])
  test_plot(subject_subsets[i]$sub_up_without_linAcc, subject_subsets[i]$sub_up_with_linAcc, 
    "mel_plot_linAcc", "Magnitude of the Acceleration Sensor", 
    "Time", "Acc")
}

mel_data <- load_data("./z_data/stairsMelanie_prepro")
mel_subsets <- preprocess(mel_data)
test_plot(mel_subsets$sub_up_without_linAcc, mel_subsets$sub_up_with_linAcc, 
  "mel_plot_linAcc", "Magnitude of the Acceleration Sensor", 
  "Time", "Acc")

fab_data <- load_data("./z_data/stairsFabian_prepro")
fab_subsets <- preprocess(fab_data)
test_plot(fab_subsets$sub_up_without_linAcc, fab_subsets$sub_up_with_linAcc, 
  "fab_plot_linAcc", "Magnitude of the Acceleration Sensor", 
  "Time", "Acc")

jua_data <- load_data("./z_data/stairsJuan_prepro")
jua_subsets <- preprocess(jua_data)
test_plot(jua_subsets$sub_up_without_linAcc, jua_subsets$sub_up_with_linAcc, 
  "jua_plot_linAcc", "Magnitude of the Acceleration Sensor", 
  "Time", "Acc")


# calculating the mean of the different subsets:
mean(mel_subsets$sub_up_without_linAcc$magnitude)
mean(mel_subsets$sub_up_with_linAcc$magnitude)
mean(mel_subsets$sub_down_without_linAcc$magnitude)
mean(mel_subsets$sub_down_with_linAcc$magnitude)

mean(fab_subsets$sub_up_without_linAcc$magnitude)
mean(fab_subsets$sub_up_with_linAcc$magnitude)
mean(fab_subsets$sub_down_without_linAcc$magnitude)
mean(fab_subsets$sub_down_with_linAcc$magnitude)

mean(jua_subsets$sub_up_without_linAcc$magnitude)
mean(jua_subsets$sub_up_with_linAcc$magnitude)
mean(jua_subsets$sub_down_without_linAcc$magnitude)
mean(jua_subsets$sub_down_with_linAcc$magnitude)

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
