#construct data files for the PopulateR package

library(dplyr)
library(readr)
library(ggplot2)
library(stringr)



# # flowchart construction
# library(DiagrammeR)
# grViz("
# digraph flowchart {
# node [overlap = true, fontsize = 20, shape = box, fontname = Helvetica]
#
# tab1 [label = 'Create age structure']
# tab2 [label = 'Identify school leavers']
# tab3 [label = 'Restrict working hours of children in school']
# tab4 [label = 'Construct couples living in the same household']
# tab5 [label = 'Identify children living in family homes']
# tab6 [label = 'Add children to family homes']
# tab7 [label = 'Add other people to households']
# tab8 [label = 'Assign schools to children']
# tab9 [label = 'Assign employers to working people']
# tab10 [label = 'Create non-school and non-employer social networks']
# tab11 [label = 'Completed synthetic population']
#
# tab1 -> tab2 -> tab3 -> tab4 -> tab5 -> tab6 -> tab7 -> tab8 -> tab9 -> tab10 -> tab11
#
# }
#  ")
#
# detach("package:DiagrammeR", unload = TRUE)

# can't save the chart to an external file as constructs an html object
# manually export from viewer
# save as png as Latex won't load tif files and jpgs arent't lossless


# import base data frame
# this will be the one with the relationships
# Legally registered relationship status and partnership status in current relationship by age and sex,
# for the census usually resident population count aged 15 years and over, 2006, 2013, and 2018 censuses
# (RC, TA, SA2, DHB) TABLECODE 8395. Using the 2018 data
OriginalRelationships <- read.csv("DataForPopsim/TABLECODE8395_Data_ba118e69-f7d7-41f8-83ac-3e296de588cd.csv")

############################################
# merge the non-partnered categories
NotPartnered <- OriginalRelationships %>%
  filter(grepl("^Non-partnered", Partnership.status.in.current.relationship)) %>%
  group_by(Sex, Age.group) %>%
  summarise(Value = sum(Value)) %>%
  ungroup() %>%
  mutate(Relationship = "NonPartnered")

Partnered <- OriginalRelationships %>%
  filter(Partnership.status.in.current.relationship %in% c("De facto partner", "Partnered, nfd", "Spouse")) %>%
  group_by(Sex, Age.group) %>%
  summarise(Value = sum(Value)) %>%
  ungroup() %>%
  mutate(Relationship = "Partnered")

Others <- OriginalRelationships %>%
  filter(Partnership.status.in.current.relationship == "Not stated") %>%
  group_by(Sex, Age.group) %>%
  summarise(Value = sum(Value)) %>%
  ungroup() %>%
  mutate(Relationship = "Unknown")

CleanedRels <- bind_rows(NotPartnered, Partnered, Others)
#############################################3

# expand relationship data frame so that it contains one row per person
library(stringi)

Relationships <- CleanedRels %>%
  slice(rep(row_number(), Value)) %>%
  select(-Value) %>%
  mutate(LowerAge = as.numeric(sub("([0-9]+).*$", "\\1", Age.group)),
         UpperAge =  as.numeric(stri_extract_last_regex(Age.group, "([0-9]+)")),
         UpperAge = ifelse(UpperAge == LowerAge, 91, UpperAge)
  )

# append the missing age-groups for those aged under 15 years
# Age and sex by ethnic group (grouped total responses), for census usually resident population counts
# 2006, 2013, and 2018 Censuses (RC, TA, SA2, DHB) TABLECODE 8395 (variant) using the 2018 data
ChildAgeGroups <- read_csv("DataForPopsim/TABLECODE8277_Data_901c1c13-bbdd-4e37-bb88-fb03624eced0.csv",
                           col_types = cols(Area = col_skip(), Flags = col_skip(), Year = col_skip(),
                                            `Ethnic group` = col_skip()))

ChildAgeGroupsExpanded <- ChildAgeGroups %>%
  rename(Age.group = `Age group`) %>%
  mutate(Relationship = "NonPartnered",
         LowerAge = as.numeric(sub("([0-9]+).*$", "\\1", Age.group)),
         UpperAge =  as.numeric(stri_extract_last_regex(Age.group, "([0-9]+)"))) %>%
  slice(rep(row_number(), Value)) %>%
  select(-Value)

Relationships <- bind_rows(Relationships, ChildAgeGroupsExpanded)

# add unique ID
InitialDataframe <- as.data.frame(Relationships %>%
  arrange(LowerAge, Sex))

InitialDataframe$ID <- row.names(InitialDataframe)

rm(OriginalRelationships, NotPartnered, Partnered, Others, CleanedRels, ChildAgeGroups, ChildAgeGroupsExpanded, Relationships)




#####################################
# AGE DATA FRAME
####################################
# Age and sex by ethnic group (grouped total responses), for census usually resident population counts
# 2006, 2013, 2018 Censuses (RC, TA, SA2, DHB) TABLECODE8277
Ages <- read_csv("DataForPopsim/TABLECODE8277_Data_42129480-9594-4698-9413-074dcdcb3d63.csv",
                 col_types = cols(Area = col_skip(), Flags = col_skip(), Year = col_skip(),
                                  `Ethnic group` = col_skip()))

SingleAges <- data.frame(Ages %>%
  filter(!(`Age group` == "Median age")) %>%
  rename(Age.group = `Age group`) %>%
 # mutate(Age = as.numeric(sub("([0-9]+).*$", "\\1", `Age group`)))
  mutate(Age = ifelse(Age.group=="Less than one year", 0,
                      ifelse(Age.group=="One year", 1,
                             ifelse(Age.group=="Two years", 2,
                                    ifelse(Age.group=="Three years", 3,
                                           ifelse(Age.group=="Four years", 4,
                                                  ifelse(Age.group=="Five years", 5,
                                                         ifelse(Age.group=="Six years", 6,
                                                                ifelse(Age.group=="Seven years", 7,
                                                                       ifelse(Age.group=="Eight years", 8,
                                                                              ifelse(Age.group=="Nine years", 9,
                                                                                     as.numeric(sub("([0-9]+).*$", "\\1", Age.group))))))))))))))

set.seed(2)
DisaggregateAge <- agedis(InitialDataframe, "Sex", "LowerAge", "UpperAge", SingleAges, "Sex", "Age", "Value",
                          agevarname = "TheAge", userseed = 4)


rm(Ages)

#####################################
# add the two datasets to the package
####################################
# NOTE THAT RELATIONSHIPS OUTPUT FILE NAME IS CHANGED TO SOMETHING MORE SPECIFIC
# AS THIS IS NO LONGER THE BASE FILE FOR EXAMPLES
####################################

save(InitialDataframe, file = "data/InitialDataframe.rda")
save(SingleAges, file = "data/SingleAges.rda")

# usethis::use_data(InitialDataframe)
# usethis::use_data(SingleAges)


#####################################
# Add in working hours
# will be a subset
####################################
# subset Relationships
# set seed so replicable
set.seed(2)
WorkDF <- DisaggregateAge %>%
  slice_sample(n=10000)

# bring in the working hours data
WorkingHoursSummary <- read_csv("DataForPopsim/TABLECODE8460_Data_35d9d921-3a94-41eb-b97d-bd70af4e7ab0.csv",
                                col_types = cols(`Status in employment` = col_skip(), Area = col_skip(),
                                                 Flags = col_skip(), Year = col_skip()))

# remove the totals
WorkingHoursSummary2 <- data.frame(WorkingHoursSummary %>%
  rename(HoursWorked = `Hours worked in employment per week`) %>%
  filter(!(`Age group` == "Total people - age group")) %>%
  mutate(LowerAge = as.numeric(sub("([0-9]+).*$", "\\1", `Age group`)),
         UpperAge =  as.numeric(stri_extract_last_regex(`Age group`, "([0-9]+)"))) %>%
  mutate(NewAgeGroups = ifelse(between(LowerAge, 15, 24), "15-24 years",
                                       ifelse(between(LowerAge, 25, 34), "25-34 years",
                                                      ifelse(between(LowerAge, 35, 44), "35-44 years",
                                                             ifelse(between(LowerAge, 45, 54), "45-54 years",
                                                                            ifelse(between(LowerAge, 55, 64),
                                                                            "55-64 years", "65 years and over"
                                                                            )))))) %>%
  group_by(Sex, NewAgeGroups, HoursWorked) %>%
  summarise(UpdatedCount = sum(Value)))

# add in the ones who aren't working, not included in the working hours data
# how many are we missing
UnderCounts <- WorkingHoursSummary2 %>%
  group_by(Sex, NewAgeGroups) %>%
  summarise(Total = sum(UpdatedCount))

TotalsFromPopulation <- DisaggregateAge %>%
  filter(!(Age.group %in% c("0-4 years", "5-9 years", "10-14 years"))) %>%
  group_by(Sex, Age.group) %>%
  summarise(NumberOfPpl = n()) %>%
  left_join(UnderCounts, by = c(Age.group = "NewAgeGroups", "Sex")) %>%
  mutate(MissingFromEmp = NumberOfPpl - Total)

# turn these undercounts into rows to bind to the working hours dataframe
# we are sampling from the Timaru District count totals, so no problem with row binding
# at this point, apples with apples
# create the look of the data frame we want, then expand

MissingHoursToRowBind <- TotalsFromPopulation %>%
  select(-c(NumberOfPpl, Total)) %>%
  rename(NewAgeGroups = Age.group,
         UpdatedCount = MissingFromEmp) %>%
  mutate(HoursWorked = "Not working") %>%
  slice(rep(row_number(), UpdatedCount))


# now expand to one row per count as dplyr slice_sample won't resample from the summarised rows
WorkingHoursExpanded <- WorkingHoursSummary2 %>%
  slice(rep(row_number(), UpdatedCount))

# add the two expanded data frames
WorkingHoursExpanded <- bind_rows(WorkingHoursExpanded, MissingHoursToRowBind)


# get the counts for each age group
HoursNeeded <- WorkDF %>%
  filter(!(Age.group %in% c("0-4 years", "5-9 years", "10-14 years"))) %>%
  group_by(Sex, Age.group) %>%
  summarise(CountsNeeded = n())


# get the sample
set.seed(2)
for(i in 1:nrow(HoursNeeded)) {
  AgeNeeded <- HoursNeeded$Age.group[i]
  SexNeeded <- HoursNeeded$Sex[i]
  CountNeeded <- HoursNeeded$CountsNeeded[i]

  cat("AgeNeeded is", AgeNeeded, "SexNeeded is", SexNeeded, "CountNeeded is", CountNeeded, "\n")

  # subset to match the AgeNeeded
  CurrentWorkSubset <- WorkingHoursExpanded %>%
    dplyr::filter(Sex == SexNeeded, NewAgeGroups == AgeNeeded)

  cat("The subset size is ", nrow(CurrentWorkSubset), "\n")

  if(nrow(CurrentWorkSubset) > 0) {

   # print(i)

    # draw with replacement

    SampleFromHours <- CurrentWorkSubset %>%
      slice_sample(n=CountNeeded)

    cat("Sampled size was", nrow(SampleFromHours), "\n")

    if(exists("OutputHours")) {
      OutputHours <- bind_rows(OutputHours, SampleFromHours)
    } else {
      OutputHours <- SampleFromHours
    }

  }

}

OutputHours <- OutputHours %>%
  rename(OHSex = Sex)

# pull out the older-than-15 year olds from the subset data
HoursOfWorkToMatch <- WorkDF %>%
  filter(TheAge > 14) %>%
  arrange(Sex, Age.group) %>%
  bind_cols(OutputHours) %>%
  select(-c(MinAge, MaxAge, OHSex, NewAgeGroups, UpdatedCount))

# add in the non-working children
NonWorkingKids <- WorkDF %>%
  filter(TheAge < 15) %>%
  select(-c(MinAge, MaxAge)) %>%
  mutate(HoursWorked = "Not working")

# construct the subset data frame for use with fixing hours, relationships, adding children, etc
Township <- bind_rows(NonWorkingKids, HoursOfWorkToMatch) %>%
  rename(Age = TheAge)

# make hours worked an ordered factor
Township$OrderedHours <- factor(Township$HoursWorked,
                                levels=c("Not working", "1-9 hours worked", "10-19 hours worked",
                                         "20-29 hours worked", "30-39 hours worked", "40-49 hours worked",
                                         "50-59 hours worked", "60 hours or more worked"), ordered=TRUE)

Township <- Township %>%
  select(-c(Age.group, HoursWorked)) %>%
  rename(HoursWorked = OrderedHours)


save(Township, file = "data/Township.rda")
rm(WorkDF, WorkingHoursSummary, WorkingHoursSummary2, UnderCounts, TotalsFromPopulation, MissingHoursToRowBind,
   WorkingHoursExpanded, HoursNeeded, SampleFromHours, OutputHours, HoursOfWorkToMatch, NonWorkingKids, CurrentWorkSubset)







####################################
# Fixing relationship status
#####################################

BadRels <- Township %>%
  filter(Age > 19 & Age < 91) %>%
  mutate(AgeBand = case_when(between(Age, 20, 29) ~ "20-29 Years",
                             between(Age, 30, 39) ~ "30-39 Years",
                             between(Age, 40, 49) ~ "40-49 Years",
                             between(Age, 50, 59) ~ "50-59 Years",
                             between(Age, 60, 69) ~ "60-69 Years",
                             between(Age, 70, 79) ~ "70-79 Years",
                             Age > 79 ~ "80-90 Years"),
         MinAge = case_when(between(Age, 20, 29) ~ 20,
                            between(Age, 30, 39) ~ 30,
                            between(Age, 40, 49) ~ 40,
                            between(Age, 50, 59) ~ 50,
                            between(Age, 60, 69) ~ 60,
                            between(Age, 70, 79) ~ 70,
                            Age > 79 ~ 80),
         MaxAge = case_when(between(Age, 20, 29) ~ 29,
                            between(Age, 30, 39) ~ 39,
                            between(Age, 40, 49) ~ 49,
                            between(Age, 50, 59) ~ 59,
                            between(Age, 60, 69) ~ 69,
                            between(Age, 70, 79) ~ 79,
                            Age > 79 ~ 90))

save(BadRels, file = "data/BadRels.rda")


table(BadRels$AgeBand, BadRels$Relationship)

# create data frame for interdiff example
GroupInfo <- as.data.frame(PropPart)
GroupInfo$MidPoints <- c(rep(c(25.5, 35.5, 45.5, 55.5, 65.5, 75.5, 86),2))

save(GroupInfo, file = "data/GroupInfo.rda")



# show the problem re the counts in the ages
FemaleProbCounts <- FinalRels %>%
  filter(Sex == "Female", between(Age, 35, 46)) %>%
  group_by(Sex, Age) %>%
  summarise(AgeCount = n())

# remove the objects constructed

rm(BadRels, PropPart, PropRelAgeR2, OrigRels, GroupInfo, FinalRelAge, FinalRelPlot, FemaleProbCounts)

####################################
# School leavers
#####################################
library("stringi")

AllSchoolLeavers <- MinEdSchool_Leavers <- read_csv("~/Sync/PhD/Ministry of Education files/Machine-Readable-School-Leavers.csv")

#school leavers
CRSchoolLeavers <- AllSchoolLeavers %>%
  filter(`Region: Regional Council`=="Canterbury Region",
         `Student: Ethnic Group` == "Total",
         `School: School Sector` != "Not Applicable") %>%
  mutate(Age = stri_extract_last_regex(`Student: Student Age`, "([0-9]+)"),
         YearLeft = as.numeric(sub("([0-9]+).*$", "\\1", `Year: Left School`)),
         Sex = as.character(`Student: Student Gender`),
         Count = `Students (∑ Values)`) %>%
  select(YearLeft, Sex, Age, Count)

LeftSchool <- as.data.frame(CRSchoolLeavers %>%
  group_by(YearLeft, Sex, Age) %>%
  summarise(Total = sum(Count)) %>%
  ungroup() %>%
  mutate(Age = as.numeric(Age)))

rm(CRSchoolLeavers)
save(LeftSchool, file = "data/LeftSchool.rda")

# Age and sex by ethnic group (grouped total responses), for census usually resident population counts
# 2006, 2013, 2018 Censuses (RC, TA, SA2, DHB) TABLECODE8277 with age range 13-19 years
CRAgePyramid <- read_csv("DataForPopsim/TABLECODE8277_Data_c73b5677-7dbe-4119-a26c-8a6a60a3b1d7.csv",
                         col_types = cols(Area = col_skip(), Flags = col_skip(), Year = col_skip(),
                                          `Ethnic group` = col_skip()))

RegionalStructure <- data.frame(CRAgePyramid %>%
  mutate(Age = as.numeric(sub("([0-9]+).*$", "\\1", `Age group`))))

save(RegionalStructure, file = "data/RegionalStructure.rda")


# run function
WithSchoolInd <- addind(Township, pplid = "ID", pplsx = "Sex", pplage = "Age", pplyear = 2018, minedage = 5,
                        maxedage = 18, LeftSchool, lvrsx = "Sex", lvrage = "Age", lvryear = "YearLeft",
                        lvrcount = "Total", RegionalStructure, pyrsx = "Sex", pyrage = "Age",
                        pyrcount = "Value", stvarname = "SchoolStatus", userseed = 4)

save(WithSchoolInd, file = "data/WithSchoolInd.rda")

rm(CRAgePyramid, AllSchoolLeavers, MinEdSchool_Leavers)

####################################
# Fix school leaver hours
#####################################

# construct school age data frame to fix the hours
# 4-year age band with the school leavers in it
WorkingAdolescents <- WithSchoolInd %>%
  filter(between(Age, 15, 18))

save(WorkingAdolescents, file = "data/WorkingAdolescents.rda")


# remove all files at this point via global environment window






####################################
# Add schools
#####################################
# get a subsample of schools for adding schools for the kids

Schools2018MOEData <- read.csv("~/Sync/PhD/PopulateR/DataForPopsim/Student-rolls-by-School2018.csv", na.strings=".")

# delete irrelevant columns for function use
InterimSchools <- Schools2018MOEData %>%
  filter(`Regional.Council` == "Canterbury Region") %>%
  select(`School.ID`, `School.Name`, Gender, `Age.5`, `Age.6`, `Age.7`, `Age.8`, `Age.9`, `Age.10`,
         `Age.11`, `Age.12`, `Age.13`, `Age.14`, `Age.15`, `Age.16`, `Age.17`, `Age.18`) %>%
  mutate(across(.cols= 4:17, ~replace(., is.na(.), 0)),
         Gender = ifelse(Gender == "Co-Ed", "C", ifelse(Gender == "Single Sex-Boys", "M", "F")))

CRSchools <- InterimSchools %>%
  tidyr::pivot_longer(`Age.5`:`Age.18`,
                      names_to = "AgeInRoll",
                      values_to = "RollCount") %>%
  mutate(AgeInRoll = as.numeric(stringr::str_sub(AgeInRoll, 5, -1)))

#####################################
# extract kids for schools


KidsYoung <- Township %>%
  filter(between(Age, 0, 14)) %>%
  mutate(SchoolStatus = ifelse(Age < 5, "N", "Y"))

KidsOlderTemp <- Township %>%
  filter(between(Age, 15, 18))

KidsCounts <- KidsOlderTemp %>%
  group_by(Age) %>%
  summarise(AgeCounts = n()) %>%
  mutate(Num = ifelse(Age == 15, round(AgeCounts*.98),
                        ifelse(Age == 16, round(AgeCounts*.85),
                              ifelse(Age == 17, round(AgeCounts*.6), round(AgeCounts*.2)))))
Nums15to18 <- KidsCounts %>%
  select(Age, Num)


# now do sample for each age for school status

SampledKids <- diffsample(people = KidsOlderTemp, pplage = "Age", Nums15to18, smplage = "Age",
                          smplcounts = "Num", userseed = 4)

# add school indicator for these kids
SampledKids <- SampledKids %>%
  mutate(SchoolStatus = "Y")

NotSampledKids <- KidsOlderTemp %>%
  filter(!(ID %in% c(SampledKids$ID))) %>%
  mutate(SchoolStatus = "N")

# add in the other older kids, with a school status of N

KidsForSchools <- bind_rows(KidsYoung, SampledKids, NotSampledKids) %>%
  mutate(SchoolStatus = forcats::fct_relevel(SchoolStatus, c("N", "Y")))

rm(KidsYoung, KidsOlderTemp, KidsCounts, SampledKids, NotSampledKids, Nums15to18, Schools2018MOEData)

# construct parents
set.seed(2)
Parents <- Township %>%
  filter(Relationship == "Partnered", Age > 19) %>%
  slice_sample(n = 300) %>% # down from 2000
  mutate(HouseholdID = row_number()+500,
         SchoolStatus = factor("N"))

ParentsAndKids <- pairmultNum(KidsForSchools, chlid = "ID", chlage = "Age", numchild = 3, twinprob = .03,
                              Parents, parid = "ID", parage = "Age", minparage = 18, maxparage = 54,
                              HHNumVar = "HouseholdID", userseed=4, maxdiff=4)


# create the factor for SchoolStatus into an ordered factor
IntoSchools <- ParentsAndKids$Matched %>%
  mutate(SexCode = ifelse(Sex == "Female", "F", "M"),
         SchoolStatus = factor(SchoolStatus, c("N", "Y"), ordered = TRUE))

rm(ParentsAndKids, Parents)

# check counts of kids of school age
# SchoolCountCheck <- IntoSchools %>%
#   filter(between(Age, 5, 18) & SchoolStatus == "Y") %>%
#   group_by(Age) %>%
#   summarise(NumKidsThatAge = n())
#
# FiveYearOldCounts <- CRSchools %>%
#   filter(AgeInRoll == 5 & RollCount > 0)
#
# CheckPrimaryCounts <- CRSchools %>%
#   filter(School.ID %in% c(3335, 355, 335, 3488)) %>%
#   group_by(AgeInRoll) %>%
#   summarise(NumKidsThatAge = sum(RollCount))
#
# # add in the secondary schools, only add in the single-sex ones
# CheckAllCounts <- CRSchools %>%
#   filter(School.ID %in% c(3335, 355, 335, 3488, 357, 360)) %>%
#   group_by(AgeInRoll) %>%
#   summarise(NumKidsThatAge = sum(RollCount))
#
# rm(SchoolCountCheck, FiveYearOldCounts, CheckPrimaryCounts, CheckAllCounts)


# get subset of schools as the kids data frame is smaller relative to the kids count in the Township file
SchoolsToUse <- as.data.frame(CRSchools %>%
  filter(School.ID %in% c(352, 357, 360, 2110, 3348, 3354, 3368, 3432, 3441, 3519, 3529, 3535, 3539, 3540,
                          3565, 3566, 3567, 3574, 3597)))

# check short by
OriginalSchoolsToUse <- SchoolsToUse %>%
  filter(AgeInRoll == 5 & RollCount > 0)


save(SchoolsToUse, file = "data/SchoolsToUse.rda")
save(IntoSchools, file = "data/IntoSchools.rda")

# # get the sums of each age
# AgeSums <- colSums(InterimSchools[4:17], na.rm = T)
#
# # have to delete a lot of schools, e.g. we have 138 five-year-olds in the sample data and
# # 6985 spaces on the rolls for five-year-olds
#
# # extract schools with rolls containing five-year-olds
# # remove the larger schools
# PrimarySchools <- InterimSchools %>%
#   filter(between(Age.5, 8, 12),
#          Age.14 == 0,
#          Age.12 < 15)
#
# SelectedPrimarySchoolAgeSums <- colSums(PrimarySchools[4:17], na.rm = T)
#
# Colleges <- InterimSchools %>%
#   filter(Age.15 > 0)
#
#
# Colleges <- InterimSchools %>%
#   filter(School.Name %in% c("Geraldine High School", "Craighead Diocesan School", "Timaru Boys' High School"))
#   # filter(SchoolName %in% c("Craighead Diocesan School", "St Thomas of Canterbury College", "Kingslea School",
#   #                          "Unlimited Paenga Tawhiti"))
#
# SelectedCollegeSchoolAgeSums <- colSums(Colleges[4:17], na.rm = T)
#
# BothCounts <- SelectedPrimarySchoolAgeSums + SelectedCollegeSchoolAgeSums
#
# rm(Schools2018MOEData, InterimSchools, AgeSums, SelectedPrimarySchoolAgeSums, Colleges,
#    SelectedCollegeSchoolAgeSums, BothCounts, OriginalSchoolsToUse, CRSchools)
#
# # add in some extra mock children as singletons to test the amended code
# set.seed(2)
# ExtraKids <- KidsIntoSchools %>%
#   filter(Age < 16) %>%
#   slice_sample(n = 50) %>%
#   mutate(HouseholdID = row_number(),
#          ID = as.character(row_number()+100))
#
# ExtraTesting <- bind_rows(KidsIntoSchools, ExtraKids)
#
# SchoolsAdded <- ChildrenToSchools(ExtraTesting, ChildIDCol = 3, ChildAgeCol = 4, ChildSxCol = 7,
#                                   HouseholdIDCol = 6, SchoolsToUse, SchoolIDCol = 2, SchoolAgeCol = 4,
#                                   SchoolRollCol = 5, SchoolTypeCol = 3, ChildProb = .8, UserSeed = 4)


####################################
# Matching into households
#####################################
# Dataframe for matching into households, no pre-existing household id

AdultsNoID <- Township %>%
  filter(Age > 19, Relationship == "NonPartnered")

save(AdultsNoID, file = "data/AdultsNoID.rda")

####################################
# Businesses
#####################################

#  use Timaru
Tablecode7602 <- read_csv("DataForPopsim/TABLECODE7602_Data_d5d2c02c-3326-4be7-bdcc-194bb676d5c1.csv",
                          col_types = cols(Flags = col_skip()))

# remove rows that are totals, this will be painful
Tablecode7602fixed <- Tablecode7602 %>%
  select(-c(Year, Area)) %>%
  filter(str_detect(ANZSIC06, "^[a-zA-Z][0-9]{3}"))

Tablecode7602probs <- Tablecode7602 %>%
  select(-c(Year, Area)) %>%
  filter(!(str_detect(ANZSIC06, "^[a-zA-Z][0-9]{3}")))


Tablecode7602fixed <- Tablecode7602fixed %>%
  mutate(Measure = ifelse(Measure == "Geographic Units", "BusinessCount", "EmployeeCount"))

AllEmployers <-tidyr::spread(Tablecode7602fixed, Measure, Value)

# add in min and max values
AllEmployers <- as.data.frame(AllEmployers %>%
  mutate(minCo = BusinessCount - 3,
         maxCo = BusinessCount + 3,
         minStaff = EmployeeCount - 3,
         maxStaff = EmployeeCount + 3,
         minCo = ifelse(minCo < 1, 1, minCo),
         minStaff = ifelse(minStaff < 1, 1, minStaff)))

save(AllEmployers, file = "data/AllEmployers.rda")




####################################
# Add employers
#####################################

# extract a subset of employers

# how many employed in Township?
table(Township$HoursWorked)
#10000-4795 = 5205
# therefore need employers that can employ at least 5205 employees
# USES THE DATA OUTPUT FROM THE TOWNSHIPEMPLOYMENT EXAMPLE
# TheCompanies <- TownshipEmployment$Companies
#
# A014 <- TheCompanies %>%
#   filter(ANZSIC06 == "A014 Sheep, Beef Cattle and Grain Farming")

#### use the output from the createemp example

set.seed(2)
EmployerSet <- TownshipEmployment %>%
  slice_sample(weight_by = NumEmployees, n = 225) %>%
  select(ANZSIC06, NumEmployees, Company)

sum(EmployerSet$NumEmployees)

save(EmployerSet, file = "data/EmployerSet.rda")





####################################
# Social networks
#####################################

set.seed(2)
Ppl4networks <- Township  %>%
  slice_sample(n = 1000)

save(Ppl4networks, file="data/Ppl4networks.rda")

set.seed(2)
NetworkMatrix <- as.vector(rpois(n = nrow(Ppl4networks), lambda = 4))
save(NetworkMatrix, file="data/NetworkMatrix.rda")

# plot the igraph data
# NEED THE SMALL EXAMPLE FROM THE PACKAGE TO DO THIS
plot(NetworkSmallN)














































devtools::run_examples()

# keep running this until no more warnings or errors
devtools::check()

devtools::build_readme()
# readme file constructed

usethis::use_cran_comments()

usethis::use_release_issue("1.0")

urlchecker::url_check()

devtools::check(remote = TRUE, manual = TRUE)

devtools::check_win_devel()

devtools::submit_cran()



