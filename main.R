source("./1_preprocessing_after_loading/preprocess.R")
source("./2_exploration/draw.R")
source("./3_testing/test.R")

stairsData <- read.table("./z_data/stairs_prepro", header = TRUE, sep="|")

attach(stairsData) #for adressing variables from dataset directly
subsets <- preprocess(stairsData)

test_plot(subsets$sub_up_without, subsets$sub_up_with)

#Reading the different datasets and preprocess them:
mel_data <- read.table("./z_data/stairsMelanie_prepro", header = TRUE, sep="|")
detach(stairsData)
attach(mel_data)
mel_subsets <- preprocess(mel_data)
test_plot(mel_subsets$sub_up_without_linAcc, mel_subsets$sub_up_with_linAcc)
fab_data <- read.table("./z_data/stairsFabian_prepro", header = TRUE, sep="|")
detach(mel_data)
attach(fab_data)
fab_subsets <- preprocess(fab_data)
test_plot(fab_subsets$sub_up_without_linAcc, fab_subsets$sub_up_with_linAcc)
jua_data <- read.table("./z_data/stairsJuan_prepro", header = TRUE, sep="|")
detach(fab_data)
attach(jua_data)
jua_subsets <- preprocess(jua_data)
test_plot(jua_subsets$sub_up_without_linAcc, jua_subsets$sub_up_with_linAcc)
test_hist(jua_subsets$sub_up_without_linAcc, jua_subsets$sub_up_with_linAcc)


