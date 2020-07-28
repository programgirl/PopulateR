library("dplyr")
library("rlang")
#library("sn")
library("ggplot2")
library("truncdist")
library("tidyr")
library("tibble")



readRDS(File8ChildrenForCouples, "File8ChildrenForCouples.rds")
readRDS(File8FemaleInCouple, "File8FemaleInCouple.rds")

File8FemaleInCouple <- File8FemaleInCouple %>%
  mutate(HouseholdID = row_number() + 1000)


set.seed(106)
File8ReducedParents <- File8FemaleInCouple %>%
  slice_sample(n = 400)


# test dataframe restriction
TwinsMatchedMore3kids <- AddChildrenLnLoop(File8ChildrenForCouples, 6, 7, 5, TwinRate = .1, File8ReducedParents, 6, 7, meanlogUsed = 3.365, sdlogUsed = 0.13,
                                    MinParentAge = 18, MaxParentAge = 54, HouseholdNumVariable= "HouseholdID", UserSeed=106)


