
library("dplyr")
library("rlang")
#library("sn")
library("ggplot2")
library("truncdist")
library("tidyr")
library("tibble")

# #####################################################################
# #####################################################################
# # Construct file with three children
# #####################################################################
# #####################################################################
#
# # readRDS(File8ChildrenForCouples, "File8ChildrenForCouples.rds")
# # readRDS(File8FemaleInCouple, "File8FemaleInCouple.rds")
# #
# # File8FemaleInCouple <- File8FemaleInCouple %>%
# #   mutate(HouseholdID = row_number() + 1000)
# #
# #
# set.seed(106)
# File8ReducedParents <- File8FemaleInCouple %>%
#   slice_sample(n = 400) # for 4 children
# # slice_sample(n = 360) # for 3 children
#
#
#test dataframe restriction
# TwinsMatchedAdjustedParentSample <- AddChildrenLnLoop(File8ChildrenForCouples, 6, 7, 4, TwinRate = .1, File8ReducedParents, 6, 7, meanlogUsed = 3.365, sdlogUsed = 0.13,
#                                     MinParentAge = 18, MaxParentAge = 54, HouseholdIDVariable= 8, UserSeed=106)

#
# Kids3 <- AddChildren(File8ChildrenForCouples, 6, 7, 3, TwinRate = .1, File8FemaleInCouple, 6, 7, MinParentAge = 18,
#                      MaxParentAge = 54, HouseholdIDVariable= 8, UserSeed=106)
#
# # get kids data frame
# # as parents are the first block of people, just remove the first Combined3PHH/3
# KidsOnlyFor3 <- Combined3PHH %>%
#   slice_tail(n = nrow(Combined3PHH)*3/4)
#
#
# # check for duplicates of person IDs
# DuplicateCheck <- Combined3PHH %>%
#   group_by(PersonID) %>%
#   mutate(dupe = n()>1) %>%
#   filter(dupe == TRUE)
#
#
# # check counts of each age used
# # compare to count vector after each step
# # note that the child age vector count is constructed AFTER the twin ages are removed
#
# # initial child count by age
# ChildrenRenamed4 %>%
#   group_by(ChildAge) %>%
#   summarise(n=n())
#
# # child counts by age after twins assigned
# TwinsMatched4 %>%
#   group_by(ChildAge) %>%
#   summarise(n=2*n())
#
# # Diff between ChildrenRenamed and TwinsMatched should be the counts in ChildrenCounts
# # it worked
#
# # get remaining child counts after adding in the non-twins
#
#
# TwinsMatched4Deductions %>%
#   gather("key", "value", ChildAge3, ChildAge4) %>%
#   group_by(value) %>%
#   summarise(n=n())
#
# TwinsMatched4ChangedIndexRefsNoDeductions %>%
#   gather("key", "value", ChildAge3, ChildAge4) %>%
#   group_by(value) %>%
#   summarise(n=n())
#
# TwinsMatched4TestingIndexDeductions %>%
#   gather("key", "value", ChildAge3, ChildAge4) %>%
#   group_by(value) %>%
#   summarise(n=n())
#
# TwinsMatched4WithPreviousAgeTesting %>%
#   gather("key", "value", ChildAge3, ChildAge4) %>%
#   group_by(value) %>%
#   summarise(n=n())
#
# NotTwins4 %>%
#   group_by(AssignedAge) %>%
#   summarise(n=2*n())
#
# # need parent difference check for appropriate age
# TwinsMatched4WithPreviousAgeTesting <- TwinsMatched4WithPreviousAgeTesting %>%
#   mutate(ParentAgeDiff3 = ParentAge - ChildAge3,
#          ParentAgeDiff4 = ParentAge -ChildAge4)

# #####################################################################
# #####################################################################
# # Further adding children testing
## see what happens when child is too old
# #####################################################################
# #####################################################################

# File8FemaleInCoupleNoOld <- File8FemaleInCouple %>%
#   filter(AssignedAge < 46) %>%
#   rename(MotherAge = AssignedAge, TheirHousehold = HouseholdID)

OutputDataframe <- AddChildren(File8ChildrenForCouples, 6, 7, 3, TwinRate = .1, File8FemaleInCoupleNoOld, 6, 7, MinParentAge = 18,
                     MaxParentAge = 54, HouseholdIDVariable= 8, UserSeed=106)

# test with households with double twins and triplets

ComplicatedTestingHouseholds <- ComplicatedTesting %>%
  select(-HouseholdID)

MakeDivisibleByFour <- data.frame("Age" = 5, "Sex" = "M", "AgeGroup" = "5-9 years", "PartnershipStatus" = "No", "UsualResidents" = "Three",
                                  "HoursWorked" = "Not Working", "PersonID" = 999999)

ComplicatedTestingHouseholds <- bind_rows(ComplicatedTestingHouseholds, MakeDivisibleByFour)

ComplicatedMothers <- data.frame("Age" = seq(30, 56, by = 2),
                                 "Sex" = c("F","F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F", "F"),
                                 "AgeGroup" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes","Yes", "Yes"),
                                 "PartnershipStatus" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes","Yes", "Yes"),
                                 "UsualResidents" = c("Six", "Six", "Six", "Six", "Six", "Six", "Six", "Six", "Six", "Six","Six", "Six", "Six", "Six"),
                                 "HoursWorked" = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes","Yes","Yes", "Yes"),
                                 "PersonID" = seq(150001, 150014, by = 1),
                                 "HouseholdID" = seq(20001, 20014, by=1))

ComplicatedOutputDataframe <- AddChildren(ComplicatedTestingHouseholds, 7, 1, 5, TwinRate = .1, ComplicatedMothers, 7, 1, MinParentAge = 18,
                               MaxParentAge = 54, HouseholdIDVariable= 8, UserSeed=106)


# #####################################################################
# #####################################################################
# # Construct file with 1 child
# #####################################################################
# #####################################################################
#
# Kids1 <- AddChild(File7DpndntCldrn, 6, 7, File7FRmnppl, 6, 7, meanlogUsed = 3.365, sdlogUsed = 0.13, MinParentAge = 18,
#                      MaxParentAge = 54, MinPropRemain = .5, HouseholdIDVariable= 8, UserSeed=106)
#
# KidsOnlyFor1 <- Kids1 %>%
#   slice_tail(n = nrow(Kids1)*1/2)
#
# #####################################################################
# #####################################################################
# # Merge the two child files into one for school assignment function development
# #####################################################################
# #####################################################################
#
# KidsForSchools <- rbind(KidsOnlyFor1, KidsOnlyFor3) %>%
#   mutate(Sex = ifelse(Sex=="Male", "M", "F"))
#
# #####################################################################
# #####################################################################
# # Testing Schools program
# #####################################################################
# #####################################################################
#
# OutputKids <- AddSchools(KidsForSchools, 7, 1, 2, HouseholdIDVariable = 8, File4TimaruSchoolLong, 2, 4, 5, 6)
#
# # look at what on earth is going on within households
# HouseholdCountsOutput <- OutputKids %>%
#   group_by(HouseholdID) %>%
#   summarise(Count = n())
#
# # check schools counts
# # this is number of child in assignment ages given parameters input above
# InputKids5To17 <- KidsForSchools %>%
#   filter(Age >= 5 & Age <=17)
#
# # that is 971 five-to-17-year-olds
# StartSchoolCount <- File4TimaruSchoolLong %>%
#   filter(Age %in% (5:17))
#
# sum(StartSchoolCount$Counts)
#
# sum(SchoolsRenamed2$ChildCounts)
#
# # 5 three-kid households turned into one-kid households
# # 150 three-kid households turned into one-kid households
#
# # grab missing kids
#
# MissingKidsFromOutput <- anti_join(KidsForSchools, OutputKids, by = c("PersonID" = "ChildID"))
#

# TESTING WITH 1-KID ONLY
# OneKidTesting <- KidsForSchools %>%
#     filter(HouseholdID %in% c(233, 246, 332, 338, 396))

# FinalMatchedChildrenNoSampling <- ThirdTimesACharm(OneKidTesting, 7, 1, 2, HouseholdIDVariable = 8, File4TimaruSchoolLong, 2, 4, 5, 6, ChildProb = .5, UserSeed = 106)

# subset twins households for testing
# TeenageTwinHouseholds <- KidsForSchools %>%
#   group_by(HouseholdID, Age) %>%
#   summarise(Count = n()) %>%
#   filter(Count >1)


# test twins only

# TwinsHouseholdsOnly <- KidsForSchools %>%
#   filter(HouseholdID %in% c(1886, 1114, 1998, 1358, 1108, 1489))

# TESTING WITH 1-KID AND 3-KIDS COMBINED, PLUS DEFINED TYPES OF TWINS, INCLUDING B/B, B/G, G/G
# SameSexAgeTesting <- KidsForSchools %>%
#   filter(HouseholdID %in% c(233, 246, 332, 338, 396, 1892, 1188, 1473, 1336, 1914, 1886, 1114, 1998, 1358, 1108))
#
#
#
#
# CTSchoolsRenamed <- SameSexTwins(ComplicatedTesting, 7, 1, 2, HouseholdIDVariable = 8, File4TimaruSchoolLong, 2, 4, 5, 6, ChildProb = 1, UserSeed = 106)
#
# CTSchoolsRenamedCounts <- CTFinalMatchedChildren %>%
#   group_by(SchoolID, ChildAge) %>%
#   summarise(Count = n(), .groups = "keep")
#
#
# table(CTSchoolsRenamed$SchoolID, CTSchoolsRenamed$ChildCounts)
#
# # file of children who are in twin households
#
# BigFileOnlyTwins <- BigFileAllHouseholds %>%
#   filter(TwinMarker == "Y") %>%
#   left_join(KidsForSchools, by = "HouseholdID")
#
#
# WorkingChildren1969 <- KidsForSchools %>%
#   filter(HouseholdID == 1969)
#
# WorkingChildren1969 <- WorkingChildren1969 %>%
#   arrange(Age)
#
# TwinsAges1969 <- WorkingChildren1969 %>%
#   group_by(Age) %>%
#   summarise(Twins = n()) %>%
#   filter(Twins > 1)
#
# cat("The number of twins ages are", nrow(TwinsAges), "\n")
# #
# #
# # extract the existing households I'm using
# # add in triplet households: two males, one female; two females, one male; three males; have two extra non-triplet child in each. Two extra tests the loop
# # add in twins families: one same sex male, one same sex female, one male one female, and have two extra non-twin child in each. Two extra tests the loop
# #
# # ComplicatedTesting <- KidsForSchools %>%
# #   filter(HouseholdID %in% c(1108, 1969, 1497, 1156, 1810, 1302, 1482))
# #
# # ExtraKids <- data.frame("Age" = c(15, 15, 15, 14, 8,
# #                                           16, 16, 16, 17, 15,
# #                                           8, 8, 8, 16, 5,
# #                                           15, 15, 9, 9, 16,
# #                                           16, 16, 13, 13, 8,
# #                                           5, 5, 8, 8, 15),
# #                            "Sex" = c("M", "M", "F", "M", "M",
# #                                      "F", "F", "M", "M", "F",
# #                                      "M", "M", "M", "F", "M",
# #                                      "M", "M", "F", "F", "M",
# #                                      "M", "M", "M", "M", "M",
# #                                      "F", "F", "F", "F", "M"
# #                                      ),
# #                            "AgeGroup" = c("10 - 14 years", "10 - 14 years", "10 - 14 years", "10 - 14 years", "10 - 14 years",
# #                                           "10 - 14 years", "10 - 14 years", "10 - 14 years", "10 - 14 years", "10 - 14 years",
# #                                           "10 - 14 years", "10 - 14 years", "10 - 14 years", "10 - 14 years", "10 - 14 years",
# #                                           "10 - 14 years", "10 - 14 years", "10 - 14 years", "10 - 14 years", "10 - 14 years",
# #                                           "10 - 14 years", "10 - 14 years", "10 - 14 years", "10 - 14 years", "10 - 14 years",
# #                                           "10 - 14 years", "10 - 14 years", "10 - 14 years", "10 - 14 years", "10 - 14 years"),
# #                            "PartnershipStatus" = c("Non-Partnered", "Non-Partnered", "Non-Partnered", "Non-Partnered", "Non-Partnered",
# #                                                    "Non-Partnered", "Non-Partnered", "Non-Partnered", "Non-Partnered", "Non-Partnered",
# #                                                    "Non-Partnered", "Non-Partnered", "Non-Partnered", "Non-Partnered", "Non-Partnered",
# #                                                    "Non-Partnered", "Non-Partnered", "Non-Partnered", "Non-Partnered", "Non-Partnered",
# #                                                    "Non-Partnered", "Non-Partnered", "Non-Partnered", "Non-Partnered", "Non-Partnered",
# #                                                    "Non-Partnered", "Non-Partnered", "Non-Partnered", "Non-Partnered", "Non-Partnered"),
# #                            "UsualResidents" = c("Six", "Six", "Six", "Six", "Six",
# #                                                 "Six", "Six", "Six", "Six", "Six",
# #                                                 "Six", "Six", "Six", "Six", "Six",
# #                                                 "Six", "Six", "Six", "Six", "Six",
# #                                                 "Six", "Six", "Six", "Six", "Six",
# #                                                 "Six", "Six", "Six", "Six", "Six"),
# #                            "HoursWorked" = c("Not Working", "Not Working", "Not Working", "Not Working", "Not Working",
# #                                              "Not Working", "Not Working", "Not Working", "Not Working", "Not Working",
# #                                              "Not Working", "Not Working", "Not Working", "Not Working", "Not Working",
# #                                              "Not Working", "Not Working", "Not Working", "Not Working", "Not Working",
# #                                              "Not Working", "Not Working", "Not Working", "Not Working", "Not Working",
# #                                              "Not Working", "Not Working", "Not Working", "Not Working", "Not Working"),
# #                            "PersonID" = c(50001, 50002, 50003, 50004, 50005,
# #                                          51001, 51002, 51003, 51004, 51005,
# #                                          52001, 52002, 52003, 52004, 52005,
# #                                          53001, 53002, 53003, 53004, 53005,
# #                                          54001, 54002, 54003, 54004, 54005,
# #                                          55001, 55002, 55003, 55004, 55005),
# #                            "HouseholdID" = c(20001, 20001, 20001, 20001, 20001,
# #                                              20002, 20002, 20002, 20002, 20002,
# #                                              20003, 20003, 20003, 20003, 20003,
# #                                              30001, 30001, 30001, 30001, 30001,
# #                                              30002, 30002, 30002, 30002, 30002,
# #                                              30003, 30003, 30003, 30003, 30003))
# #
# # ComplicatedTesting <- bind_rows(ComplicatedTesting, ExtraKids)
#
# CTFinalMatchedChildrenRandom <- ThirdTimesACharm(ComplicatedTesting, 7, 1, 2, HouseholdIDVariable = 8, File4TimaruSchoolLong, 2, 4, 5, 6, ChildProb = 1, UserSeed = 106)
#
# ProblemHousehold30001 <- ComplicatedTesting %>%
#   filter(HouseholdID == 30001)
#
# CTFinalMatchedChildrenRandomWC <- ThirdTimesACharm(ProblemHousehold30001, 7, 1, 2, HouseholdIDVariable = 8, File4TimaruSchoolLong, 2, 4, 5, 6, ChildProb = 1, UserSeed = 106)
#
# # testing on full dataframe below
# # HouseholdIDList <- as.data.frame(KidsForSchools %>%
# #                                    distinct(HouseholdID))
# #
# # LimitedToTwinsHouseholds <- KidsForSchools %>%
# #   group_by(HouseholdID, Age) %>%
# #   summarise(CountsByAge = n(), .groups = "keep") %>%
# #   filter(CountsByAge >1)
# #
# # HouseholdIDList <-HouseholdIDList %>%
# #   mutate(TwinMarker = ifelse(HouseholdID %in% LimitedToTwinsHouseholds$HouseholdID, "Y", "N"))
# #
# # TwinHouseholdSubset <- HouseholdIDList %>%
# #   filter(TwinMarker == "Y")
# #
# # NoTwinsHouseholdSubset <- HouseholdIDList %>%
# #   filter(TwinMarker == "N")
# #
# # BottomNoTwins <- NoTwinsHouseholdSubset %>%
# #   slice_sample(n=(nrow(HouseholdIDList)*.1))
# #
# # TopNoTwins <- NoTwinsHouseholdSubset %>%
# #   filter(!(HouseholdID %in% BottomNoTwins$HouseholdID))
# #
# # InitialHouseholdRebind <- bind_rows(TwinHouseholdSubset, TopNoTwins)
# #
# # HouseholdIDList <- bind_rows(InitialHouseholdRebind, BottomNoTwins)
#
# # how many single-child households in the first 148 used for testing?
# RestrictedHouseholds <- HouseholdIDList148 %>%
#   slice_head(n = 148)
#
# OneChildTestCases <- inner_join(KidsForSchools, RestrictedHouseholds, by = "HouseholdID") %>%
#   filter(between(Age, 5, 17)==TRUE) %>%
#   group_by(HouseholdID) %>%
#   summarise(ChildCount = n()) %>%
#   filter(ChildCount == 1)
#
# MultipleChildNoTwins <- RestrictedHouseholds %>%
#   filter(!(HouseholdID %in% c(OneChildTestCases$HouseholdID)),
#          TwinMarker == "N")
#
# MultipleChildTwins <- RestrictedHouseholds %>%
#   filter(!(HouseholdID %in% c(OneChildTestCases$HouseholdID)),
#          TwinMarker == "Y")
#
# SchoolsInAgeRange <- File4TimaruSchoolLong %>%
#   filter(Age <18)

ChildrenFinal <- ChildrenToSchools(KidsForSchools, 7, 1, 2, HouseholdIDVariable = 8, File4TimaruSchoolLong, 2, 4, 5, 6, ChildProb = .2, UserSeed = 106)

MissingKids <- anti_join(KidsForSchools, ChildrenFinal, by = c("PersonID"= "ChildID"))

ShouldBeAssigned <- MissingKids %>%
  filter(Age > 4)
