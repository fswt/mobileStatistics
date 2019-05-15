t_test_down <- function(means_subjects_subsets){
  mean_down_with <- c(means_subjects_subsets[1,4], 
                      means_subjects_subsets[2,4], means_subjects_subsets[3,4])
  mean_down_without <- c(means_subjects_subsets[1,3], 
                         means_subjects_subsets[2,3], means_subjects_subsets[3,3])
  
  t.test(mean_down_with, mean_down_without, alternative = "less", 
         paired = TRUE)
}

t_test_up <- function(means_subjects_subsets){
  mean_up_with <- c(means_subjects_subsets[1,2], 
                    means_subjects_subsets[2,2], means_subjects_subsets[3,2])
  mean_up_without <- c(means_subjects_subsets[1,1], 
                         means_subjects_subsets[2,1], means_subjects_subsets[3,1])
  
  t.test(mean_up_with, mean_up_without, alternative = "less", 
         paired = TRUE)
}