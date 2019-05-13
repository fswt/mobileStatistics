source("./1_preprocessing_after_loading/preprocess.R")
source("./2_exploration/draw.R")
source("./3_testing/test.R")

stairsData <- read.table("./z_data/stairs_prepro", header = TRUE, sep="|")
subsets <- preprocess()
show_overview()
test_plot(subsets$sub_up_without, subsets$sub_up_with)
