#############################################################
# only use this once to demonstrate how to disaggregate age
library(dplyr)

DisaggregateAge <- AgeStructure(Relationships, IndividualSxCol = 1, MinimumAgeCol = 4,
                                MaximumAgeCol = 5, SingleAges, PyramidSxCol = 2,
                                PyramidAgeCol = 4, PyramidCountCol = 3, NewAgeVariable = "TheAge",
                                UserSeed = 4)

# Township is the file to use for the other functions
###########################################################

########################################################### #
# School leavers function
library(dplyr)

WithSchoolInd <- SchoolLeavers(Township, AdolescentSxCol = 1, AdolescentAgeCol = 4,
                               AdolescentsYear = 2018, MinSchoolAge = 5, MaxSchoolAge = 18,
                               LeftSchool, LeaversSxCol = 2, LeaversAgeCol = 3, LeaversCountCol = 4,
                               LeaversYearCol = 1,
                               RegionalStructure, PyramidSxCol = 1, PyramidAgeCol = 4, PyramidCountCol = 3,
                               SchoolStatus = "SchoolStatus", UserSeed = 4)

rm(WithSchoolInd)

########################################################### ##

########################################################### #
# Fix hours function
library(dplyr)

AdolescentWork <- FixHours(WorkingAdolescents, AdolescentIDCol=3, AdolescentAgeCol=4, InSchoolCol=6,
                           HoursWorkedCol=5, HoursCutOff=3, UserSeed=4)



########################################################### ##
# same-sex couples
# females shown as example

library(dplyr)

# same-sex couples
PartneredFemales <- Township %>%
  filter(Sex == "Female", Relationship == "Partnered")

# assume 1% of partnered female couples are same-sex
# assume that there is an equal probability of any age of partnered women being in a same-sex couple
# household number provided
NoUpweightGiven <- SameSex(PartneredFemales, AgeCol = 4, ProbSameSex = 0.01,
                           IDStartValue = 10, HouseholdNumVariable = "HouseholdID", UserSeed = 4)

# partnered females with upweighted ages, no starting household ID value provided
Upweighted <- SameSex(PartneredFemales, AgeCol = 4, ProbSameSex = 0.01, UpWeightProp = 0.5,
                      UpWeightLowerAge = 25, UpWeightUpperAge = 40,HouseholdNumVariable = "HouseholdID",
                      UserSeed = 4)

rm(PartneredFemales, NoUpweightGiven, Upweighted)

########################################################### ##

########################################################### #
# opposite sex couples

library(dplyr)

set.seed(1)
PartneredFemales <- Township %>%
  filter(Sex == "Female", Relationship == "Partnered")

PartneredMales <- Township %>%
  filter(Sex == "Male", Relationship == "Partnered")

# there are more partnered males than partnered females
# so all partnered males will have a matched female partner
# but not all females will be matched
# being the smallest data frame, the female one must be the first

# normal distribution
OppSexCouples <- OppSexN(PartneredFemales, RecipientIDCol=3, RecipientAgeCol=4,
                         PartneredMales, DonorIDCol=3, DonorAgeCol=4, meanUsed= -2,
                         sdUsed = 3, IDStartValue = 100, HouseholdNumVariable="HouseholdID",
                         UserSeed = 4, pValueToStop=.01,  NumIterations=1000000)

# repeat with matched counts
set.seed(1)
PartneredMalesSmall <- Township %>%
  filter(Sex == "Male", Relationship == "Partnered") %>%
  slice_sample(n = nrow(PartneredFemales))

OppSexCouples2 <- OppSexN(PartneredFemales, RecipientIDCol=3, RecipientAgeCol=4,
                          PartneredMalesSmall, DonorIDCol=3, DonorAgeCol=4, meanUsed= -2,
                         sdUsed = 3, IDStartValue = 100, HouseholdNumVariable="HouseholdID",
                         UserSeed = 4, pValueToStop=.01,  NumIterations=1000000)


TheMatched <- OppSexCouples$Matched

# skew normal distribution (not run)
# OppSexCouplesSN <-OppSexSN(PartneredFemales, RecipientIDCol=4, RecipientAgeCol=5,
#                            PartneredMales, DonorIDCol=4, DonorAgeCol=5,
#                            DirectXi = -2.5, DirectOmega = 12, AlphaUsed = -2,
#                            IDStartValue = 100, HouseholdNumVariable="HouseholdID",
#                            UserSeed=4,pValueToStop=.01, NumIterations=1000000)


rm(PartneredFemales, PartneredMales, OppSexCouples, PartneredMalesSmall, OppSexCouples2, TheMatched)
########################################################### ##

########################################################### #
# assign child, no parental household

library("dplyr")
# sample a combination of females and males to be parents
Parents <- Township %>%
  filter(Relationship == "Partnered", Age > 18) %>%
  slice_sample(n = 500)

Children <- Township %>%
  filter(Relationship == "NonPartnered", Age < 20) %>%
  slice_sample(n = 200)

# match the children to the parents
# no ID on the parents
ParentsAndKids <- AddChildSN(Children, ChildIDCol = 3, ChildAgeCol = 4,
                             Parents, ParentIDCol = 3, ParentAgeCol = 4,
                             DirectXi = 30, DirectOmega = 3, AlphaUsed = 1.2,
                             MinParentAge = 18, MaxParentAge = 54, MinPropRemain = 0,
                             IDStartValue = 100, HouseholdNumVariable = "HouseholdID", UserSeed=4)

rm(Parents, Children, ParentsAndKids, ParentKidDiffs, AgeDiffs)

########################################################### ##

########################################################### #
# assign child, parent has household ID

library("dplyr")

Parents <- Township %>%
  filter(Relationship == "Partnered", Age > 18) %>%
  slice_sample(n = 500) %>%
  mutate(HouseholdID = row_number()+500)

Children <- Township %>%
  filter(Relationship == "NonPartnered", Age < 20) %>%
  slice_sample(n = 200)

OutputWithID <- AddChildSNID(Children, ChildIDCol = 4, ChildAgeCol = 5,
                             Parents, ParentIDCol = 4, ParentAgeCol = 5,
                             DirectXi = 30, DirectOmega = 3, AlphaUsed = 1.2,
                             MinParentAge = 18, MaxParentAge = 54, MinPropRemain = 0,
                             HouseholdIDCol = 8, UserSeed = 4)

ParentKidDiffs <- OutputWithID %>%
  group_by(HouseholdID) %>%
  arrange(desc(Age), .by_group = TRUE) %>%
  mutate(AgeDiff = -(Age - lag(Age, default = first(Age)))) %>%
  filter(AgeDiff > 0)

library(ggplot2)
AgeDiffs <- ggplot(ParentKidDiffs, aes (x = AgeDiff)) +
  geom_bar(fill = "#fdb863") +
  labs(x="Age difference, parent age - child age", y = "Number of parent-child pairs")
# ggsave(AgeDiffs, file="~/Sync/PhD/Thesis2020/PopSimArticle/ParentAgeDiffsWithID.pdf")

detach("package:ggplot2", unload = TRUE)

rm(Parents, Children, OutputWithID, ParentKidDiffs, AgeDiffs)

########################################################### ##

########################################################### #
# assign multiple children, no parental household

library("dplyr")

Parents <- Township %>%
  filter(Relationship == "Partnered", Age > 18) %>%
  slice_sample(n = 500) %>%
  mutate(HouseholdID = row_number()+500)

Children <- Township %>%
  filter(Relationship == "NonPartnered", Age < 20) %>%
  slice_sample(n = 400)

# example with assigning two children to a parent
# the same number of children is assigned to all parents
# adding two children to each parent

ParentsAndKids <- AddChildren(Children, ChildIDCol = 3, ChildAgeCol = 4, NumChildren = 2,
                              TwinRate = .2, Parents, ParentIDCol = 3, ParentAgeCol = 4,
                              MinParentAge = 18, MaxParentAge = 54, HouseholdIDCol = 6,
                              UserSeed = 4)


# trying to add three results in one child unmatched, 400 is not divisible by 3
ParentsAndKids <- AddChildren(Children, ChildIDCol = 4, ChildAgeCol = 5, NumChildren = 3,
                              TwinRate = .2, Parents, ParentIDCol = 4, ParentAgeCol = 5,
                              MinParentAge = 18, MaxParentAge = 54, HouseholdIDCol = 8,
                              UserSeed = 4)

rm(Parents, Children, ParentsAndKids)

########################################################### ##
# add children to schools
########################################################### #
library("dplyr")

SchoolsAdded <- ChildrenToSchools(KidsIntoSchools, ChildIDCol = 3, ChildAgeCol = 4, ChildSxCol = 7,
                                           HouseholdIDCol = 6, SchoolsToUse, SchoolIDCol = 2, SchoolAgeCol = 4,
                                           SchoolRollCol = 5, SchoolTypeCol = 3, ChildProb = .8, UserSeed = 4)

table(SchoolsAdded$SchoolID, SchoolsAdded$Sex)


########################################################### ##
# expand employers
########################################################### #
library("dplyr")

TownshipEmployment <- CreateEmployers(AllEmployers, EmployerTypeCol = 1, EmployerCountCol = 2, EmployeeCountCol = 3, UserSeed = 4)



########################################################### ##
# social networks
########################################################### #

TownshipNetworks <- SocialNetworks(NetworkSizes, IDCol = 3, AgeCol = 4, HouseholdCol = 6, NetworkCol = 6, SDUsed = 2, UserSeed = 4)
