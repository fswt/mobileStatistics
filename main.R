source("./1_preprocessing_after_loading/preprocess.R")
source("./2_exploration/draw.R")
source("./3_testing/test.R")

stairsData <- read.table("./z_data/stairs_prepro", header = TRUE, sep="|")
subsets <- preprocess()
test_plot(subsets$sub_up_without, subsets$sub_up_with)

#Reading the different datasets:
mel_data <- read.table("./z_data/stairsMelanie_prepro", header = TRUE, sep="|")
fab_data <- read.table("./z_data/stairsFabian_prepro", header = TRUE, sep="|")
jua_data <- read.table("./z_data/stairsJuan_prepro", header = TRUE, sep="|")

#preprocess the data 
mel_subsets <- preprocess()