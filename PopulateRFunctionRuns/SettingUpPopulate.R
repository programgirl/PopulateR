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
InitialDataframe <- Relationships %>%
  arrange(LowerAge, Sex)

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
DisaggregateAge <- agedis(InitialDataframe, indsxcol = 1, minagecol = 4,
                          maxagecol = 5, SingleAges, pyrsxcol = 2, pyragecol = 4,
                          pyrcountcol = 3, agevarname = "TheAge", userseed = 4)

rm(Ages)

#####################################
# add the two datasets to the package
####################################
# NOTE THAT RELATIONSHIPS OUTPUT FILE NAME IS CHANGED TO SOMETHING MORE SPECIFIC
# AS THIS IS NO LONGER THE BASE FILE FOR EXAMPLES
####################################

save(InitialDataframe, file = "data/InitialDataframe.RData")
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


save(Township, file = "data/Township.RData")
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

save(BadRels, file = "data/BadRels.RData")


table(BadRels$AgeBand, BadRels$Relationship)

PropPart <- BadRels %>%
  group_by(Sex, AgeBand, MinAge, MaxAge, Relationship) %>%
  summarise(NuminRel = n()) %>%
  mutate(RelProps = NuminRel/sum(NuminRel)) %>%
  filter(Relationship == "Partnered") %>%
  select(-NuminRel) %>%
  ungroup()

PropRelAgeR2 <- BadRels %>%
  group_by(Sex, Age, Relationship) %>%
  summarise(NuminRel = n()) %>%
  mutate(RelProps = round(NuminRel/sum(NuminRel), 2)) %>%
  filter(Relationship == "Partnered") %>%
  select(-NuminRel) %>%
  ungroup()

OrigRels <- ggplot() +
  geom_segment(data = PropPart, aes(x = MinAge, y = RelProps, xend = MaxAge, yend = RelProps, colour = Sex,
                                    ), linewidth = 1) +
  geom_point(data = PropRelAgeR2, aes(x = Age, y = RelProps, colour = Sex)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0,100)) +
  scale_y_continuous(breaks = c(0, .2, .4, .6, .8, 1), limits = c(0,1)) +
  labs(x = "Age (years)", y = "Proportion partnered") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#    ggsave(OrigRels, file="~/Sync/PhD/Thesis2020/PopSimArticle/OrigRels.pdf", width = 12.25, height = 7.15, units = "in")


# create data frame for interdiff example
GroupInfo <- PropPart
GroupInfo$MidPoints <- c(rep(c(25.5, 35.5, 45.5, 55.5, 65.5, 75.5, 86),2))

save(GroupInfo, file = "data/GroupInfo.RData")






# plot the data for the fixed output for relationships

FinalRelAge <- FinalRels %>%
  group_by(Sex, Age, Relationship) %>%
  summarise(NuminRel = n()) %>%
  mutate(RelProps = round(NuminRel/sum(NuminRel), 2)) %>%
  filter(Relationship == "Partnered") %>%
  select(-NuminRel) %>%
  ungroup()

FinalRelPlot <- ggplot() +
  geom_segment(data = PropPart, aes(x = MinAge, y = RelProps, xend = MaxAge, yend = RelProps, colour = Sex,
  ), size = 1) +
  geom_point(data = FinalRelAge, aes(x = Age, y = RelProps, colour = Sex)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0,100)) +
  scale_y_continuous(breaks = c(0, .2, .4, .6, .8, 1), limits = c(0,1)) +
  labs(x = "Age (years)", y = "Proportion partnered") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#    ggsave(FinalRelPlot, file="~/Sync/PhD/Thesis2020/PopSimArticle/FinalRelPlot.pdf", width = 12.25, height = 7.15, units = "in")

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
set.seed(2)
WithSchoolInd <- schoolind(Township, adlsxcol = 1, adlagecol = 4, adlyear = 2018, minschage = 5, maxschage = 18,
                           LeftSchool, lvrsxcol = 2, lvragecol = 3, lvrctcol = 4, lvryearcol = 1,
                           RegionalStructure, strusxcol = 1, struagecol = 4, structcol = 3, stvarname = "SchoolStatus",
                           userseed = 4)

save(WithSchoolInd, file = "data/WithSchoolInd.RData")

rm(CRAgePyramid, AllSchoolLeavers, MinEdSchool_Leavers)

####################################
# Fix school leaver hours
#####################################

# construct school age data frame to fix the hours
# pretend there was a 5-year age band with the school leavers in it
WorkingAdolescents <- WithSchoolInd %>%
  filter(between(Age, 16, 20))

save(WorkingAdolescents, file = "data/WorkingAdolescents.RData")

# comparisons of before and after fix
# graph

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

FixedGroup <- AdolescentWork2 %>%
  group_by(SchoolStatus, HoursWorked) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

FixedGroup <- FixedGroup %>%
  right_join(AllHoursValues) %>%
  mutate(across(where(is.numeric), ~tidyr::replace_na(., 0)))


##########################
# graphs now placed inside the article
# this is the previous version that did the placement inside the R code
# also now have 3 graphs
##########################
# library(cowplot)
# library(ggplot2)
#
# OriginalGraph <- ggplot(Original, aes(x=HoursWorked, y = freq,
#                                       fill = SchoolStatus)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_manual(values=c("#5e3c99", "#fdb863")) +
#   coord_cartesian(ylim = c(0, .8)) +
#   labs(x="Hours worked per week", y = "Proportion of 15-19 year olds",
#        fill = "Person in school?") +
#   scale_x_discrete(labels= c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+"),
#                    guide = guide_axis(angle = 90)) +
#   theme(text = element_text(size = 18),
#         plot.margin = unit(c(1,0,0,0), "cm"))
#
# FixedGraph <- ggplot(Fixed, aes(x=HoursWorked, y = freq,
#                                 fill = SchoolStatus)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_fill_manual(values=c("#5e3c99", "#fdb863")) +
#   coord_cartesian(ylim = c(0, .8)) +
#   labs(x="Hours worked per week", y = "Proportion of 15-19 year olds",
#        fill = "Person in school?") +
#   scale_x_discrete(labels= c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+"),
#                    guide = guide_axis(angle = 90)) +
#   theme(text = element_text(size = 18),
#         plot.margin = unit(c(1,0,0,0), "cm"))
#
# BothPlots <- cowplot::plot_grid(OriginalGraph + theme(legend.position = "none"),
#                                 FixedGraph + theme(legend.position = "none"),
#                                 labels = c("Original", "Adjusted"),
#                                 label_size = 16,
#                                 align = "h",
#                                 hjust = -2)
#
# BothPlotsLegend <- cowplot::get_legend(OriginalGraph +
#                                          guides(color = guide_legend(nrow = 1)) +
#                                          theme(legend.position = "bottom"))
#
# BothPlotsFinal <- cowplot::plot_grid(BothPlots, BothPlotsLegend,
#                                      ncol = 1, rel_heights = c(1, .1))

# ggsave(BothPlotsFinal, file="~/Sync/PhD/Thesis2020/PopSimArticle/SchoolWorkHours.pdf")
# detach packages
# detach("package:cowplot", unload = TRUE)
# detach("package:ggplot2", unload = TRUE)
######################

# new graph approach

OriginalHours <- ggplot(Original, aes(x=HoursWorked, y = freq,
                                      fill = SchoolStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values=c("#5e3c99", "#fdb863")) +
  coord_cartesian(ylim = c(0, .8)) +
  labs(x="Hours worked per week", y = "Proportion of 15-19 year olds",
       fill = "Person in school?") +
  scale_x_discrete(labels= c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+"),
                   guide = guide_axis(angle = 90)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

FixedHours1 <- ggplot(Fixed, aes(x=HoursWorked, y = freq,
                                      fill = SchoolStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values=c("#5e3c99", "#fdb863")) +
  coord_cartesian(ylim = c(0, .8)) +
  labs(x="Hours worked per week", y = "Proportion of 15-19 year olds",
       fill = "Person in school?") +
  scale_x_discrete(labels= c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+"),
                   guide = guide_axis(angle = 90)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

FixedHours2 <- ggplot(FixedGroup, aes(x=HoursWorked, y = freq,
                            fill = SchoolStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values=c("#5e3c99", "#fdb863")) +
  coord_cartesian(ylim = c(0, .8)) +
  labs(x="Hours worked per week", y = "Proportion of 15-19 year olds",
       fill = "Person in school?") +
  scale_x_discrete(labels= c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+"),
                   guide = guide_axis(angle = 90)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

# save the graphs
#    ggsave(OriginalHours, file="~/Sync/PhD/Thesis2020/PopSimArticle/OriginalHours.pdf", width = 12.25, height = 7.15, units = "in")
#    ggsave(FixedHours1, file="~/Sync/PhD/Thesis2020/PopSimArticle/FixedHours1.pdf", width = 12.25, height = 7.15, units = "in")
#    ggsave(FixedHours2, file="~/Sync/PhD/Thesis2020/PopSimArticle/FixedHours2.pdf", width = 12.25, height = 7.15, units = "in")

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

cor.test(as.numeric(AdolescentWork2$HoursWorked), as.numeric(AdolescentWork2$SchoolStatus),
         method = "kendall",
         exact=FALSE)

# remove all extra files
rm(Original, Fixed, FixedGroup, AllHoursValues, OriginalGraph, OriginalHours, FixedHours1,
   FixedHours2)






####################################
# Graph and differences for the opposite sex
# NEEDS THE OUTPUT FROM THE CODE EXAMPLES FILE
#####################################

# now doing a combined graph for both examples
OppSexAgeDiffPlotValues1 <- Couples1 %>%
  group_by(HouseholdID) %>%
  arrange(desc(Sex), .by_group = TRUE) %>%
  mutate(AgeDiff = -(Age - lag(Age, default = first(Age))),
         Source = "Normal") %>%
  filter(Sex == "Female")

OppSexAgeDiffPlotValues2 <- Couples2 %>%
  group_by(HouseholdID) %>%
  arrange(desc(Sex), .by_group = TRUE) %>%
  mutate(AgeDiff = -(Age - lag(Age, default = first(Age))),
         Source = "Normal") %>%
  filter(Sex == "Female")

AgeDiffs1 <- ggplot(OppSexAgeDiffPlotValues1, aes (x = AgeDiff)) +
  geom_bar(fill = "#5e3c99") +
  xlim(-15, 15) +
  labs(x="Age difference, years, male age - female age", y = "Number of couples") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")
# ggsave(AgeDiffs1, file="~/Sync/PhD/Thesis2020/PopSimArticle/OppSexAgeDiffs1.pdf", width = 12.25, height = 7.15, units = "in")

AgeDiffs2 <- ggplot(OppSexAgeDiffPlotValues2, aes (x = AgeDiff)) +
  geom_bar(fill = "#fdb863") +
  xlim(-15,15) +
  labs(x="Age difference, years, male age - female age", y = "Number of couples") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

# ggsave(AgeDiffs2, file="~/Sync/PhD/Thesis2020/PopSimArticle/OppSexAgeDiffs2.pdf", width = 12.25, height = 7.15, units = "in")

# get quantile values
quantile(OppSexAgeDiffPlotValues1$AgeDiff)
quantile(OppSexAgeDiffPlotValues2$AgeDiff)



####################################
# Graph and differences parents and kids
# NEEDS THE OUTPUT FROM THE CODE EXAMPLES FILE
# from pairbeta4
#####################################
# graph the single child, 4 parameter beta age difference, children smaller

ParentKidDiffsCh1 <- ChildAllMatched$Matched %>%
  group_by(Household) %>%
  arrange(desc(Age), .by_group = TRUE) %>%
  mutate(AgeDiff = -(Age - lag(Age, default = first(Age)))) %>%
  filter(AgeDiff > 0)


PKAgeDiffs1 <- ggplot(ParentKidDiffsCh1, aes (x = AgeDiff)) +
  geom_bar(fill = "#5e3c99") +
  labs(x="Age difference, parent age - child age", y = "Number of parent-child pairs") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(PKAgeDiffs1, file="~/Sync/PhD/Thesis2020/PopSimArticle/PKAgeDiffs1.pdf", width = 12.25, height = 7.15, units = "in")


# plot the parent ages
ParentAges1 <- ChildAllMatched$Matched %>%
  group_by(Household) %>%
  filter(Age == max(Age))

PKAgesG1 <- ggplot(ParentAges1, aes (x = Age)) +
  geom_bar(fill = "#fdb863") +
  xlim(10, 70) +
  labs(x="Current age of parent", y = "Number of parents") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(PKAgesG1, file="~/Sync/PhD/Thesis2020/PopSimArticle/PKAgesG1.pdf", width = 12.25, height = 7.15, units = "in")

# get numbers for the article
# age at childbirth
min(ParentKidDiffsCh1$AgeDiff)
max(ParentKidDiffsCh1$AgeDiff)
median(ParentKidDiffsCh1$AgeDiff)

#current age of parent
min(ParentAges1$Age)
max(ParentAges1$Age)
median(ParentAges1$Age)

# table of parent ages
table(ParentAges1$Age)


# graph the single child, 4 parameter beta age difference, children larger

ParentKidDiffsCh2 <- ChildAllMatched2$Matched %>%
  group_by(Household) %>%
  arrange(desc(Age), .by_group = TRUE) %>%
  mutate(AgeDiff = -(Age - lag(Age, default = first(Age)))) %>%
  filter(AgeDiff > 0)


PKAgeDiffs2 <- ggplot(ParentKidDiffsCh2, aes (x = AgeDiff)) +
  geom_bar(fill = "#5e3c99") +
  labs(x="Age difference, parent age - child age", y = "Number of parent-child pairs") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(PKAgeDiffs2, file="~/Sync/PhD/Thesis2020/PopSimArticle/PKAgeDiffs2.pdf", width = 12.25, height = 7.15, units = "in")


# plot the parent ages
ParentAges2 <- ChildAllMatched2$Matched %>%
  group_by(Household) %>%
  filter(Age == max(Age))

PKAgesG2 <- ggplot(ParentAges2, aes (x = Age)) +
  geom_bar(fill = "#fdb863") +
  xlim(10, 80) +
  labs(x="Current age of parent", y = "Number of parents") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(PKAgesG2, file="~/Sync/PhD/Thesis2020/PopSimArticle/PKAgesG2.pdf", width = 12.25, height = 7.15, units = "in")

# get numbers for the article
# age at childbirth
min(ParentKidDiffsCh2$AgeDiff)
max(ParentKidDiffsCh2$AgeDiff)
median(ParentKidDiffsCh2$AgeDiff)

#current age of parent
min(ParentAges2$Age)
max(ParentAges2$Age)
median(ParentAges2$Age)

# table of parent ages
table(ParentAges2$Age)













# second, from child yes
#####################################
# graph the single child, skew normal age difference

ParentKidDiffsCh2 <- ChildMatchedID$Matched %>%
  group_by(HouseholdID) %>%
  arrange(desc(Age), .by_group = TRUE) %>%
  mutate(AgeDiff = -(Age - lag(Age, default = first(Age)))) %>%
  filter(AgeDiff > 0)


PKAgeDiffs2 <- ggplot(ParentKidDiffsCh2, aes (x = AgeDiff)) +
  geom_bar(fill = "#fdb863") +
  xlim(10, 70) +
  labs(x="Current age of parent", y = "Number of parents") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(PKAgeDiffs2, file="~/Sync/PhD/Thesis2020/PopSimArticle/PKAgeDiffs2.pdf", width = 12.25, height = 7.15, units = "in")

# get age of parents now
ParentsID2 <- ChildMatchedID$Matched %>%
  filter(Relationship == "Partnered")

PKAgesG2 <-  ggplot(ParentsID2, aes (x = Age)) +
  geom_bar(fill = "#fdb863") +
  xlim(10,70) +
  labs(x="Current age of parent", y = "Number of parents") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(PKAgesG2, file="~/Sync/PhD/Thesis2020/PopSimArticle/PKAgesG2.pdf", width = 12.25, height = 7.15, units = "in")

rm(ParentKidDiffs, AgeDiffs, ParentsID, ParentAges)






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
set.seed(2)
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
set.seed(2)
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
set.seed(2)
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

save(AllEmployers, file = "data/AllEmployers.RData")

# remember, uses the code from the CodeExamples to construct the data frame

# by employer
CompaniesCreated <- TownshipEmployment$Companies

library("ggplot2")
AveEmpCount <- CompaniesCreated %>%
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

# rm(Tablecode7602, Tablecode7602fixed, Tablecode7602probs, AllEmployers, OriginalAveEmpl, NewAveEmpl, FullCompData, CompanyPropComps, AveEmpCount)

# Overcount contents
EmpOvercount <- TownshipEmployment$Overcount

# With no employees or employers
Nope <- TownshipEmployment$NoEmps


####################################
# Add employers
#####################################

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

set.seed(2)
EmployerSet <- TownshipEmployment$Companies %>%
  slice_sample(weight_by = EmployeeCount, n = 225)

sum(EmployerSet$EmployeeCount)

save(EmployerSet, file = "data/EmployerSet.RData")





####################################
# Social networks
# #####################################

set.seed(2)
Ppl4networks <- Township  %>%
  slice_sample(n = 5000)

save(Ppl4networks, file="data/Ppl4networks.RData")

set.seed(2)
NetworkMatrix <- as.vector(rpois(n = nrow(Ppl4networks), lambda = 4))
save(NetworkMatrix, file="data/NetworkMatrix.RData")

# plot the igraph data
# NEED THE SMALL EXAMPLE FROM THE PACKAGE TO DO THIS
plot(NetworkSmallN)

# keep running this until no more warnings or errors
devtools::check()

# after all checks passed
# for compiling the package
devtools::document()

