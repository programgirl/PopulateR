#construct data files for the PopSim package

library(dplyr)
library(readr)


library(DiagrammeR)

# flowchart construction
grViz("
digraph flowchart {
node [overlap = true, fontsize = 20, shape = box, fontname = Helvetica]

tab1 [label = 'Create age structure']
tab2 [label = 'Identify school leavers']
tab3 [label = 'Restrict working hours of children in school']
tab4 [label = 'Construct couples living in the same household']
tab5 [label = 'Identify children living in family homes']
tab6 [label = 'Add children to family homes']
tab7 [label = 'Add other people to households']
tab8 [label = 'Assign schools to children']
tab9 [label = 'Assign employers to working people']
tab10 [label = 'Create non-school and non-employer social networks']
tab11 [label = 'Completed synthetic population']

tab1 -> tab2 -> tab3 -> tab4 -> tab5 -> tab6 -> tab7 -> tab8 -> tab9 -> tab10 -> tab11

}
 ")

detach("package:DiagrammeR", unload = TRUE)

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
Relationships <- Relationships %>%
  arrange(LowerAge, Sex)

Relationships$ID <- row.names(Relationships)

rm(OriginalRelationships, NotPartnered, Partnered, Others, CleanedRels, ChildAgeGroups, ChildAgeGroupsExpanded)




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


DisaggregateAge <- AgeStructure(Relationships, IndividualSxCol = 1, MinimumAgeCol = 4,
                                MaximumAgeCol = 5, SingleAges, PyramidSxCol = 2,
                                PyramidAgeCol = 4, PyramidCountCol = 3, NewAgeVariable = "TheAge",
                                UserSeed = 4)

rm(Ages)

#####################################
# add the two datasets to the package
####################################
# NOTE THAT RELATIONSHIPS OUTPUT FILE NAME IS CHANGED TO SOMETHING MORE SPECIFIC
# AS THIS IS NO LONGER THE BASE FILE FOR EXAMPLES
####################################

save(Relationships, file = "data/Relationships.RData")
save(SingleAges, file = "data/SingleAges.RData")




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
set.seed(4)
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


save(Township, file = "data/Township.RData")
rm(WorkDF, WorkingHoursSummary, WorkingHoursSummary2, UnderCounts, TotalsFromPopulation, MissingHoursToRowBind,
   WorkingHoursExpanded, HoursNeeded, SampleFromHours, OutputHours, HoursOfWorkToMatch, NonWorkingKids)





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

LeftSchool <- CRSchoolLeavers %>%
  group_by(YearLeft, Sex, Age) %>%
  summarise(Total = sum(Count)) %>%
  ungroup() %>%
  mutate(Age = as.numeric(Age))

rm(CRSchoolLeavers)
save(LeftSchool, file = "data/LeftSchool.RData")

# Age and sex by ethnic group (grouped total responses), for census usually resident population counts
# 2006, 2013, 2018 Censuses (RC, TA, SA2, DHB) TABLECODE8277 with age range 13-19 years
CRAgePyramid <- read_csv("DataForPopsim/TABLECODE8277_Data_c73b5677-7dbe-4119-a26c-8a6a60a3b1d7.csv",
                         col_types = cols(Area = col_skip(), Flags = col_skip(), Year = col_skip(),
                                          `Ethnic group` = col_skip()))

RegionalStructure <- data.frame(CRAgePyramid %>%
  mutate(Age = as.numeric(sub("([0-9]+).*$", "\\1", `Age group`))))

save(RegionalStructure, file = "data/RegionalStructure.RData")

# need to save Regional Structure
save(RegionalStructure, file = "data/RegionalStructure.RData")

# run function
WithSchoolInd <- SchoolLeavers(Township, AdolescentSxCol = 1, AdolescentAgeCol = 4,
                             AdolescentsYear = 2018, MinSchoolAge = 5, MaxSchoolAge = 18,
                             LeftSchool, LeaversSxCol = 2, LeaversAgeCol = 3, LeaversCountCol = 4,
                             LeaversYearCol = 1,
                             RegionalStructure, PyramidSxCol = 1, PyramidAgeCol = 4, PyramidCountCol = 3,
                             SchoolStatus = "SchoolStatus", UserSeed = 4)

save(WithSchoolInd, file = "data/WithSchoolInd.RData")



####################################
# Fix school leaver hours
#####################################

# construct school age data frame to fix the hours
WorkingAdolescents <- WithSchoolInd %>%
  filter(between(Age, 15, 24))

save(WorkingAdolescents, file = "data/WorkingAdolescents.RData")


# comparisons of before and after fix
# graph
library(cowplot)
library(ggplot2)

Original <- WorkingAdolescents %>%
  group_by(SchoolStatus, HoursWorked) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

Fixed <- AdolescentWork %>%
  group_by(SchoolStatus, HoursWorked) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

AllHoursValues <- Fixed %>%
  tidyr::expand(SchoolStatus, HoursWorked)

Fixed <- Fixed %>%
  right_join(AllHoursValues) %>%
  mutate(across(where(is.numeric), ~tidyr::replace_na(., 0)))

OriginalGraph <- ggplot(Original, aes(x=HoursWorked, y = freq,
                                      fill = SchoolStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values=c("#5e3c99", "#fdb863")) +
  coord_cartesian(ylim = c(0, .8)) +
  labs(x="Hours worked per week", y = "Proportion of 15-24 year olds",
       fill = "Person in school?") +
  scale_x_discrete(labels= c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+"),
                   guide = guide_axis(angle = 90)) +
  theme(text = element_text(size = 18),
        plot.margin = unit(c(1,0,0,0), "cm"))

FixedGraph <- ggplot(Fixed, aes(x=HoursWorked, y = freq,
                                fill = SchoolStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values=c("#5e3c99", "#fdb863")) +
  coord_cartesian(ylim = c(0, .8)) +
  labs(x="Hours worked per week", y = "Proportion of 15-24 year olds",
       fill = "Person in school?") +
  scale_x_discrete(labels= c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+"),
                   guide = guide_axis(angle = 90)) +
  theme(text = element_text(size = 18),
        plot.margin = unit(c(1,0,0,0), "cm"))

BothPlots <- cowplot::plot_grid(OriginalGraph + theme(legend.position = "none"),
                                FixedGraph + theme(legend.position = "none"),
                                labels = c("Original", "Adjusted"),
                                label_size = 16,
                                align = "h",
                                hjust = -2)

BothPlotsLegend <- cowplot::get_legend(OriginalGraph +
                                         guides(color = guide_legend(nrow = 1)) +
                                         theme(legend.position = "bottom"))

BothPlotsFinal <- cowplot::plot_grid(BothPlots, BothPlotsLegend,
                                     ncol = 1, rel_heights = c(1, .1))

# ggsave(BothPlotsFinal, file="~/Sync/PhD/Thesis2020/PopSimArticle/SchoolWorkHours.pdf")

# compare the non-parametric ordinal variation assocations.
# demonstrate lack of link between school, work, age
# Ordered hours is being correctly assessed as an ordinal variable
# original data
cor.test(as.numeric(WorkingAdolescents$HoursWorked), as.numeric(WorkingAdolescents$SchoolStatus),
         method = "kendall")
# fixed data
cor.test(as.numeric(AdolescentWork$HoursWorked), as.numeric(AdolescentWork$SchoolStatus),
         method = "kendall",
         exact=FALSE)

# remove all extra files
rm(Original, Fixed, AllHoursValues, OriginalGraph, FixedGraph, BothPlots, BothPlotsLegend,
   BothPlotsFinal)

# detach packages
detach("package:cowplot", unload = TRUE)
detach("package:ggplot2", unload = TRUE)

####################################
# Graph and differences between the weighted and unweighted same-sex couples
# Cumulative sum by age
# complete on the one plot
# NEEDS THE TWO DATA FRAMES FROM THE CODE EXAMPLES FILE
#####################################
# library("ggplot2")
#
# ggplot(NoUpweightGiven, aes(x = Age)) +
#   stat_ecdf(col = "#5e3c99") +
#   stat_ecdf(data = Upweighted, col = "#fdb863")

# is ugly
# not enough points to get a good cumulative density
# get proportion of points lying between the upweights

# not upweighted:
# sum(NoUpweightGiven$Age >= 25 & NoUpweightGiven$Age <= 40)
# sum(Upweighted$Age >= 25 & Upweighted$Age <= 40)


####################################
# Graph and differences for the opposite sex
# NEEDS THE OUTPUT FROM THE CODE EXAMPLES FILE
#####################################
# get graph of age differences
OppSexAgeDiffPlotValues <- OppSexCouples$Matched %>%
  group_by(HouseholdID) %>%
  arrange(desc(Sex), .by_group = TRUE) %>%
  mutate(AgeDiff = -(Age - lag(Age, default = first(Age))),
         Source = "Normal") %>%
  filter(Sex == "Female")


library(ggplot2)
AgeDiffs <- ggplot(OppSexAgeDiffPlotValues, aes (x = AgeDiff)) +
  geom_bar(fill = "#5e3c99") +
  labs(x="Age difference, years, male age - female age", y = "Number of couples") +
  theme(text = element_text(size = 12))
# ggsave(AgeDiffs, file="~/Sync/PhD/Thesis2020/PopSimArticle/OppSexAgeDiffs.pdf", width = 7, height = 4)

rm(OppSexAgeDiffPlotValues, AgeDiffs)

detach("package:ggplot2", unload = TRUE)



####################################
# Graph and differences parents and kids
# NEEDS THE OUTPUT FROM THE CODE EXAMPLES FILE
#####################################
ParentKidDiffs <- ChildrenMatchedID$Matched %>%
  group_by(HouseholdID) %>%
  arrange(desc(Age), .by_group = TRUE) %>%
  mutate(AgeDiff = -(Age - lag(Age, default = first(Age)))) %>%
  filter(AgeDiff > 0)

library(ggplot2)
AgeDiffs <- ggplot(ParentKidDiffs, aes (x = AgeDiff)) +
  geom_bar(fill = "#5e3c99") +
  labs(x="Age difference, parent age - child age", y = "Number of parent-child pairs") +
  theme(text = element_text(size = 18))
# ggsave(AgeDiffs, file="~/Sync/PhD/Thesis2020/PopSimArticle/ParentAgeDiffs.pdf")

rm(ParentKidDiffs, AgeDiffs)

detach("package:ggplot2", unload = TRUE)





####################################
# Graph and differences parents and kids
# multiple children households
# NEEDS THE OUTPUT FROM THE CODE EXAMPLES FILE
#####################################
# generic testing

Adultsnotmatched <- KidsMatched$Adults
Kidsnotmatched <- KidsMatched$Children
TheMatched <- KidsMatched$Matched

Inboth <- inner_join(Adultsnotmatched, KidsMatched$Matched)

# doubled up parents?
TestIDSize <- KidsMatched$Matched %>%
  group_by(ID) %>%
  summarise(numinid = n())

TestHHSize <- KidsMatched$Matched %>%
  group_by(HouseholdID) %>%
  summarise(numinhouse = n())

rm(Adultsnotmatched, Kidsnotmatched, TheMatched, Inboth, TestIDSize, TestHHSize)
####################################
# Add schools
#####################################
# get a subsample of schools for adding schools for the kids

Schools2018MOEData <- read.csv("~/Sync/PhD/PopSim/DataForPopsim/Student-rolls-by-School2018.csv", na.strings=".")

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
  mutate(Probs = ifelse(Age == 15, round(AgeCounts*.98),
                        ifelse(Age == 16, round(AgeCounts*.85),
                              ifelse(Age == 17, round(AgeCounts*.6), round(AgeCounts*.2)))))
Probs15to18 <- KidsCounts %>%
  pull(Probs)


# now do sample for each age for school status
# use the stratify package

library(splitstackshape)
# https://stackoverflow.com/questions/62603942/how-do-i-sample-specific-sizes-within-groups
set.seed(4)
SampledKids <- stratified(KidsOlderTemp, "Age", c("15" = Probs15to18[1], "16" = Probs15to18[2],
                                               "17" = Probs15to18[3], "18" = Probs15to18[4]))

# add school indicator for these kids
SampledKids <- SampledKids %>%
  mutate(SchoolStatus = "Y")

NotSampledKids <- KidsOlderTemp %>%
  filter(!(ID %in% c(SampledKids$ID))) %>%
  mutate(SchoolStatus = "N")

# add in the other older kids, with a school status of N

KidsForSchools <- bind_rows(KidsYoung, SampledKids, NotSampledKids) %>%
  mutate(SchoolStatus = forcats::fct_relevel(SchoolStatus, c("N", "Y")))

rm(KidsYoung, KidsOlderTemp, KidsCounts, SampledKids, NotSampledKids, Probs15to18, Schools2018MOEData)

# construct parents
set.seed(4)
Parents <- Township %>%
  filter(Relationship == "Partnered", Age > 19) %>%
  slice_sample(n = 2000) %>%
  mutate(HouseholdID = row_number()+500,
         SchoolStatus = factor("N"))

ParentsAndKids <- childrenyes(KidsForSchools, 3, 4, 3, .2, Parents, 3, 4, 18, 54, 6, UserSeed = 4)

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
SchoolsToUse <- CRSchools %>%
  filter(School.ID %in% c(352, 357, 360, 2110, 3348, 3354, 3368, 3432, 3441, 3519, 3529, 3535, 3539, 3540,
                          3565, 3566, 3567, 3574, 3597))

# check short by
OriginalSchoolsToUse <- SchoolsToUse %>%
  filter(AgeInRoll == 5 & RollCount > 0)


save(SchoolsToUse, file = "data/SchoolsToUse.RData")
save(IntoSchools, file = "data/IntoSchools.RData")

# get the sums of each age
AgeSums <- colSums(SchoolsInterim[6:19], na.rm = T)

# have to delete a lot of schools, e.g. we have 139 five-year-olds in the sample data and
# 7024 spaces on the rolls for five-year-olds

# extract schools with rolls containing five-year-olds
# remove the larger schools
PrimarySchools <- InterimSchools %>%
  filter(between(Age.5, 8, 12),
         Age.14 == 0,
         Age.12 < 15)

SelectedPrimarySchoolAgeSums <- colSums(PrimarySchools[6:19], na.rm = T)

Colleges <- InterimSchools %>%
  filter(Age.15 > 0)


Colleges <- InterimSchools %>%
  filter(School.Name %in% c("Geraldine High School", "Craighead Diocesan School", "Timaru Boys' High School"))
  # filter(SchoolName %in% c("Craighead Diocesan School", "St Thomas of Canterbury College", "Kingslea School",
  #                          "Unlimited Paenga Tawhiti"))

SelectedCollegeSchoolAgeSums <- colSums(Colleges[6:19], na.rm = T)

BothCounts <- SelectedPrimarySchoolAgeSums + SelectedCollegeSchoolAgeSums

rm(Schools2018MOEData, InterimSchools, AgeSums, SelectedPrimarySchoolAgeSums, Colleges,
   SelectedCollegeSchoolAgeSums, BothCounts, OriginalSchoolsToUse, CRSchools)

# add in some extra mock children as singletons to test the amended code
ExtraKids <- KidsIntoSchools %>%
  filter(Age < 16) %>%
  slice_sample(n = 50) %>%
  mutate(HouseholdID = row_number(),
         ID = as.character(row_number()+100))

ExtraTesting <- bind_rows(KidsIntoSchools, ExtraKids)

SchoolsAdded <- ChildrenToSchools(ExtraTesting, ChildIDCol = 3, ChildAgeCol = 4, ChildSxCol = 7,
                                  HouseholdIDCol = 6, SchoolsToUse, SchoolIDCol = 2, SchoolAgeCol = 4,
                                  SchoolRollCol = 5, SchoolTypeCol = 3, ChildProb = .8, UserSeed = 4)


####################################
# Matching into households
#####################################
# Dataframe for matching into households, no pre-existing household id

AdultsNoID <- Township %>%
  filter(Age > 19, Relationship == "NonPartnered")

save(AdultsNoID, file = "data/AdultsNoID.RData")

####################################
# Businesses
#####################################
# this data is somehow contaminated
# TABLECODE7602 <- read_csv("DataForPopsim/TABLECODE7602_Data_a4aa8e6e-60d1-45ae-a5f0-310ec3fb5001.csv",
#                           col_types = cols(Area = col_skip(), Flags = col_skip(), Year = col_skip()))

# replacement data is excel format, and directly downloaded
# csv data was via a link in email

library("stringr")

Tablecode7602 <- read_csv("DataForPopsim/newdownload/TABLECODE7602_Data_c80a8f15-9e6b-4dba-8f22-94d3409e4751.csv", col_types = cols(Flags = col_skip()))

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

# remove all double-up counts

save(AllEmployers, file = "data/AllEmployers.RData")

# remember, uses the code from the CodeExamples to construct the data frame

# do some sort of comparison of cumulative counts
library("ggplot2")
#
#################
# was by industry, do by employer

# OriginalAveEmpl <- AllEmployers %>%
#   mutate(AveEmp = round(EmployeeCount/BusinessCount, 2),
#          Size = ifelse(AveEmp == 0, "None",
#                        ifelse(AveEmp > 0 & AveEmp < 1, "< 1",
#                        ifelse(between(AveEmp, 1, 5), "1 - 5",
#                        ifelse(AveEmp > 5 & AveEmp <= 10, "6 - 10",
#                        ifelse(AveEmp > 10 & AveEmp <= 20, "11 - 20",
#                               ifelse(AveEmp > 20 & AveEmp <= 50, "21 - 50",
#                                      ifelse(AveEmp > 50 & AveEmp <= 100, "51 - 100",
#                                             ifelse(AveEmp > 100 & AveEmp <= 150, "101 - 150",
#                                                    ifelse(is.infinite(AveEmp), "Other",
#                                                    ifelse(AveEmp > 150, "> 150",
#                                                                  "Other")))))))))),
#          Size = ifelse(is.na(Size), "Other", Size),
#          Source = "Input") %>%
#   select(-c(BusinessCount, EmployeeCount)) %>%
#   group_by(Source, Size) %>%
#   summarise(n = n()) %>%
#   mutate(freq = n/sum(n))
#
# ######################################
# # NEED THE OUTPUT FROM THE FUNCTION
#
# NewAveEmpl <- TownshipEmployment$Companies %>%
#   group_by(ANZSIC06) %>%
#   summarise(AveEmp = mean(EmployeeCount)) %>%
#   mutate(Size = ifelse(between(AveEmp, 1, 5), "1 - 5",
#          ifelse(AveEmp > 5 & AveEmp <= 10, "6 - 10",
#                 ifelse(AveEmp > 10 & AveEmp <= 20, "11 - 20",
#                        ifelse(AveEmp > 20 & AveEmp <= 50, "21 - 50",
#                               ifelse(AveEmp > 50 & AveEmp <= 100, "51 - 100",
#                                      ifelse(AveEmp > 100 & AveEmp <= 150, "101 - 150",
#                                             ifelse(AveEmp %in% c(NaN, Inf),"None",
#                                                    ifelse(AveEmp > 150, "> 150",
#                                                           "< 1")))))))),
#          Source = "Output") %>%
#   group_by(Source, Size) %>%
#   summarise(n = n()) %>%
#   mutate(freq = n/sum(n)) %>%
#   ungroup() %>%
#   add_row(Source = "Output", Size = "None", n = 0, freq = 0) %>%
#   add_row(Source = "Output", Size = "< 1", n = 0, freq = 0) %>%
#   add_row(Source = "Output", Size = "Other", n = 0, freq = 0)
#
# FullCompData <- bind_rows(OriginalAveEmpl, NewAveEmpl) %>%
#   mutate(Size = factor(Size, levels=c("None", "< 1", "1 - 5", "6 - 10", "11 - 20", "21 - 50",
#                                       "51 - 100", "101 - 150", "> 150", "Other")))
#
# CompanyPropComps <- ggplot(FullCompData, aes(x=Size, y = freq, fill = Source)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_manual(values=c("#5e3c99", "#fdb863")) +
#   labs(x="Number of employers per company", y = "Proportion of companies",
#        fill = "Source")
# # ggsave(CompanyPropComps, file="~/Sync/PhD/Thesis2020/PopSimArticle/CompanySizes.pdf")

#######
# by employer
library("ggplot2")

AveEmpCount <- TownshipEmployment$Companies %>%
  mutate(Size = ifelse(between(EmployeeCount, 1, 5), "1 - 5",
                ifelse(EmployeeCount > 5 & EmployeeCount <= 10, "6 - 10",
                ifelse(EmployeeCount > 10 & EmployeeCount <= 20, "11 - 20",
                ifelse(EmployeeCount > 20 & EmployeeCount <= 50, "21 - 50",
                ifelse(EmployeeCount > 50 & EmployeeCount <= 100, "51 - 100",
                ifelse(EmployeeCount > 100 & EmployeeCount <= 150, "101 - 150","> 150"))))))) %>%
  group_by(Size) %>%
  summarise(n = n()) %>%
    mutate(freq = n/sum(n),
           Size = factor(Size, levels=c("None", "< 1", "1 - 5", "6 - 10", "11 - 20", "21 - 50",
                                                 "51 - 100", "101 - 150", "> 150", "Other"))) %>%
    ungroup()

CompanyPropComps <- ggplot(AveEmpCount, aes(x = Size, y = freq)) +
  geom_bar(stat = "identity", fill = "#5e3c99") +
  coord_cartesian(ylim = c(0, .8)) +
  labs(x="Number of employees per company", y = "Proportion of companies") +
  theme(text = element_text(size = 18))

# ggsave(CompanyPropComps, file="~/Sync/PhD/Thesis2020/PopSimArticle/CompanySizes.pdf")

rm(Tablecode7602, Tablecode7602fixed, Tablecode7602probs, AllEmployers, OriginalAveEmpl, NewAveEmpl, FullCompData, CompanyPropComps, AveEmpCount)

# write out data frame examples below
# saveRDS(Parents, "Parents.rds")
# saveRDS(KidsForSchools, "KidsForSchools.rds")
#
# Parents <- readRDS("Parents.rds")
# KidsForSchools <- readRDS("KidsForSchools.rds")



####################################
# Add employers
#####################################

library("dplyr")

# extract a subset of employers

# how many employed in Township?
table(Township$HoursWorked)
#10000-4770 = 5230
# therefore need employers that can employ at least 5230 employees
# USES THE DATA OUTPUT FROM THE TOWNSHIPEMPLOYMENT EXAMPLE
# TheCompanies <- TownshipEmployment$Companies
#
# A014 <- TheCompanies %>%
#   filter(ANZSIC06 == "A014 Sheep, Beef Cattle and Grain Farming")

set.seed(4)
EmployerSet <- TownshipEmployment$Companies %>%
  slice_sample(weight_by = EmployeeCount, n = 225)

sum(EmployerSet$EmployeeCount)

save(EmployerSet, file = "data/EmployerSet.RData")

####################################
# Social networks
# #####################################
# set.seed(4)
# NotChildren <- Township %>%
#   filter(Relationship == "Partnered", Age > 18) %>%
#   group_by(Sex) %>%
#   slice_sample(n = 1000) %>%
#   ungroup() %>%
#   mutate(HouseholdID = row_number()+500)
#
# AllChildren <- Township %>%
#   filter(Relationship == "NonPartnered", Age < 20)
#
#
#
#
# ParentHouseholds <- childrenyes(AllChildren, chlidcol = 3, chlagecol = 4, numchild = 2, twinrate = .2,
#                               NotChildren, paridcol = 3, paragecol = 4, minparage = 18,
#                               maxparage = 54, hhidcol = 6, UserSeed = 4)
#
# ParentsMatched <- ParentHouseholds$Matched
#
# LeftOvers <- Township %>%
#   filter(!(ID %in% c(ParentsMatched$ID)))
#
# LeftOverPartneredF <- LeftOvers %>%
#   filter(Relationship == "Partnered" & Sex=="Female")
# LeftOverPartneredM <- LeftOvers %>%
#   filter(Relationship == "Partnered" & Sex=="Male")
#
# ParentFWithChildren <- ParentsMatched %>%
#   filter(Sex== "Female" & Relationship == "Partnered" & Age > 18)
# ParentMWithChildren <- ParentsMatched %>%
#   filter(Sex== "Male" & Relationship == "Partnered" & Age > 18)
#
# CouplesFbase <- otheryes(ParentFWithChildren, exsidcol = 3, exsagecol = 4, hhidcol = 6,
#                          LeftOverPartneredM, addidcol = 3, addagecol = 4, numppl = 1, sdused = 3,
#                          userseed=4, ptostop = .01, numiters = 5000)
#
# CouplesMbase <- otheryes(ParentMWithChildren, exsidcol = 3, exsagecol = 4, hhidcol = 6,
#                          LeftOverPartneredF, addidcol = 3, addagecol = 4, numppl = 1, sdused = 3,
#                          userseed=4, ptostop = .01, numiters = 5000)
#
# Families <- bind_rows(CouplesFbase$Matched, CouplesMbase$Matched)
#
# ChildrenMatched <- ParentsMatched %>%
#   filter(!(ID %in% c(Families$ID)))
#
# Families <- bind_rows(Families, ChildrenMatched)
#
# TownshipNetworks <- Township %>%
#   mutate(NetworkSize = rpois(nrow(.), lambda = 4))

MaleToCouple <- Township %>%
  filter(Relationship == "Partnered", Age > 19, Sex == "Male")

FemaleToCouple <- Township %>%
  filter(Relationship == "Partnered", Age > 19, Sex == "Female")

TCouples <-couples(FemaleToCouple, smlidcol=3, smlagecol=4, MaleToCouple, lrgidcol=3,
            lrgagecol=4, directxi=-2, directomega=3, alphaused=0, hhidstart = 1,
            hhidvar="Household", UserSeed=4, ptostop=.01, numiters=1000000)

Parents <- TCouples$Matched

Children <- Township %>%
    filter(Relationship == "NonPartnered", Age < 20)

FemaleForChild <- Parents %>%
  filter(Sex=="Female") %>%
  slice_sample(n = 1000)

MaleForChild <- Parents %>%
  filter(Sex=="Male", !(Household %in% c(FemaleForChild$Household))) %>%
  slice_sample(n = 110)

PossibleParents <- bind_rows(FemaleForChild, MaleForChild)

ParentsAndKids <- childrenyes(Children, chlidcol = 3, chlagecol = 4, numchild = 2, twinrate = .1,
                              PossibleParents, paridcol = 3, paragecol = 4, minparage = 22, maxparage = 56,
                              hhidcol= 6, UserSeed=4)

CompletedHH <- ParentsAndKids$Matched

OtherPartToCouple <- Parents %>%
  filter(Household %in% CompletedHH$Household,
         !(ID %in% CompletedHH$ID))

CompletedHH <- bind_rows(CompletedHH, OtherPartToCouple)

RemainingAdultsInHH <- Parents %>%
  filter(!(ID %in% CompletedHH$ID))

# random extract half of them
RemainingAinHHSample <- RemainingAdultsInHH %>%
  group_by(Household) %>%
  slice_sample(n = 1)

RemainingKids <-  Township %>%
  filter(ID %in% ParentsAndKids$Children$ID)

ParentAndKids2 <- childyes(RemainingKids, chlidcol = 3, chlagecol = 4, RemainingAinHHSample, paridcol = 3,
                           paragecol = 4, directxi = 30, directomega = 2, minparage = 22, maxparage = 56,
                           hhidcol = 6, UserSeed=4)

CompletedHH2 <- ParentAndKids2$Matched

OtherPartToCouple2 <- Parents %>%
  filter(Household %in% CompletedHH2$Household,
         !(ID %in% CompletedHH2$ID))

CompletedHH2 <- bind_rows(CompletedHH2, OtherPartToCouple2)

AllCompletedHH <- bind_rows(CompletedHH, CompletedHH2)

HHMissingPPl <- Parents %>%
  filter(!(ID %in% AllCompletedHH$ID))

AllCompletedHH <- bind_rows(AllCompletedHH, HHMissingPPl)

RemainingPPl <- Township %>%
  filter(!(ID %in% AllCompletedHH$ID))

ThreePPl <- RemainingPPl %>%
  slice_sample(n = 900)

ThreepplHH <- otherno(ThreePPl, pplidcol = 3, pplagecol = 4, numppl = 3, sdused = 3, hhidstart = 2320,
                      hhidvar= "Household", userseed=4, ptostop = .01, numiters = 5000)

RemainingPeople <- RemainingPPl %>%
  filter(!(ID %in% ThreepplHH$Matched$ID))

AllCompletedHH <- bind_rows(AllCompletedHH, ThreepplHH$Matched)

TwoPPl <- RemainingPeople %>%
  slice_sample(n = 700)

TwopplHH <- otherno(TwoPPl, pplidcol = 3, pplagecol = 4, numppl = 2, sdused = 3, hhidstart = 2620,
                      hhidvar= "Household", userseed=4, ptostop = .01, numiters = 5000)

AllCompletedHH <- bind_rows(AllCompletedHH, TwopplHH$Matched)

Networks <- AllCompletedHH %>%
    mutate(NetworkSize = rpois(nrow(.), lambda = 4))

save(Networks, file="data/Networks.RData")

# so I could push the small subset to GitHub for Jonathan to see.
save(subset, file = "data/subset.RData")
