t_test_down <- function(means_subjects_subsets) {
  
}

t_test_up <- function(means_subjects_subsets) {
  mean_up_with <- c(means_subjects_subsets[1, 2], means_subjects_subsets[2, 
    2], means_subjects_subsets[3, 2])
  mean_up_without <- c(means_subjects_subsets[1, 1], means_subjects_subsets[2, 
    1], means_subjects_subsets[3, 1])
  
  t.test(mean_up_with, mean_up_without, alternative = "less", 
    paired = TRUE)
}












mean_up_with <- c(means_subjects_subsets[1, 2], means_subjects_subsets[2, 
                                                                       2], means_subjects_subsets[3, 2])
mean_up_without <- c(means_subjects_subsets[1, 1], means_subjects_subsets[2, 
                                                                          1], means_subjects_subsets[3, 1])





mean_down_with <- c(means_subjects_subsets[10, 4], means_subjects_subsets[11, 
  4], means_subjects_subsets[12, 4], means_subjects_subsets[13, 
  4], means_subjects_subsets[14, 4], means_subjects_subsets[15, 
  4], means_subjects_subsets[16, 4])
mean_down_without <- c(means_subjects_subsets[10, 3], means_subjects_subsets[11, 
  3], means_subjects_subsets[12, 3], means_subjects_subsets[13, 
  3], means_subjects_subsets[14, 3], means_subjects_subsets[15, 
  3], means_subjects_subsets[16, 3])
mean_down_without
wilcox.test(mean_down_without, mean_down_with, alt = "greater", 
  paired = TRUE)

mean_down_without
mean_down_with

hist(mean_down_without, xlab = "Mean Acceleration Magnitude", prob = T, breaks=5)
curve(dnorm(x, mean = mean(mean_down_without), sd = sd(mean_down_without)), 
      add = TRUE)
qqnorm(mean_down_without, main="Q-Q Plot for mean_down_without")
path = paste("./graphs/qq_mean_down_without.png", sep="")
dev.copy(png, path)
dev.off()

hist(mean_down_with, xlab = "Mean Acceleration Magnitude", prob = T, breaks=5)
curve(dnorm(x, mean = mean(mean_down_with), sd = sd(mean_down_with)), 
      add = TRUE)
qqnorm(mean_down_with, main="Q-Q Plot for mean_down_with")
path = paste("./graphs/qq_mean_down_with.png", sep="")
dev.copy(png, path)
dev.off()

sd(means_subjects_subsets[10:16, 4])
max(means_subjects_subsets[10:16, 1]-means_subjects_subsets[10:16, 2])

power.t.test(delta=0.5, sd=0.7*sqrt(2), power=0.85, type="paired", alt="one.sided")





