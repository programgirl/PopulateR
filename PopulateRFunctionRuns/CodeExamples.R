########################################################### ##
# addnetwork examples
########################################################### #
library("dplyr")

NetworksMadeN <- addnetwork(Ppl4networks, "ID", "Age", NetworkMatrix, sdused=2,
                            probsame = .5, userseed=4, numiters = 10)

NetworksMadeY <- addnetwork(Ppl4networks, "ID", "Age", NetworkMatrix, sdused=2,
                            probsame = .5, userseed=4, numiters = 10, usematrix = "Y")

# smaller examples for the article

set.seed(4)
SmallDemo <- Township %>%
  slice_sample(n = 20)

Smallnetwork <- rpois(n = nrow(SmallDemo), lambda = 1.5)

NetworkSmallN <- addnetwork(SmallDemo, "ID", "Age", Smallnetwork, sdused=2,
                            probsame = .5, userseed=4, numiters = 10, usematrix = "N")

plot(NetworkSmallN)




#############################################################
# agedis example
#############################################################

library(dplyr)

DisaggregateAge <- agedis(InitialDataframe, indsx = "Sex", minage = "LowerAge", maxage = "UpperAge",
                          pyramid = SingleAges, pyrsx = "Sex", pyrage = "Age", pyrcount = "Value",
                          agevarname = "TheAge", userseed = 4)


# Township is the file to use for the other functions

rm(SingleAges)


########################################################### ##
# createemp example
########################################################### #
library("dplyr")

TownshipEmployment <- createemp(AllEmployers, industry = "ANZSIC06", indsmin = "minCo", indsmax = "maxCo",
                                pplmin = "minStaff", pplmax = "maxStaff", stffname="Employees",
                                cpyname="Employer", userseed = 4)


###########################################################
# 2 functions to fix the proportion of partnered people
###########################################################

# 1st function:

thegroups <- as.vector("Sex")
GroupAges <- data.frame(Sex = c("Female", "Male"), GrpMinAge = c(20,20), GrpMaxAge = c(90,90))
RelProps <- interdiff(GroupInfo, 5, 4, GroupAges, 2, 3, thegroups)

# 2nd function

joinwith <- c("Age", "Sex")
thegroups <- c("Sex", "AgeBand")

FinalRels <- relstatfix(BadRels, grpdef = thegroups, pplidcol = 3, pplagecol = 4, pplstatcol = 2,
                        stfixval = "Partnered", RelProps, matchdef = joinwith, fitscol = 2, userseed = 4)


# go to SettingUpPopulateR at this point to draw the graph for the fixed variables

rm(thegroups, GroupAges, RelProps, joinwith, thegroups, FinalRels)



###########################################################
# School leavers function
###########################################################

WithSchoolInd <- schoolind(Township, adlsxcol = 1, adlagecol = 4, adlyear = 2018, minschage = 5, maxschage = 18,
                           LeftSchool, lvrsxcol = 2, lvragecol = 3, lvrctcol = 4, lvryearcol = 1,
                           RegionalStructure, strusxcol = 1, struagecol = 4, structcol = 3,
                           stvarname = "SchoolStatus", userseed = 4)

rm(WithSchoolInd, LeftSchool, RegionalStructure)

########################################################### ##
# Fix hours function
########################################################### #

AdolescentWork <- hoursfix(WorkingAdolescents, adlidcol = 3, statuscol = 6, hourscol = 5, hoursmax = 3, userseed = 4)

# when a group is used
AdolescentWork2 <- hoursfix(WorkingAdolescents, adlidcol = 3, statuscol = 6, hourscol = 5, hoursmax = 3, grpcol = 1,
                            userseed = 4)

# go to SettingUpPopulateR to do the graphs

rm(AdolescentWork, AdolescentWork2, WorkingAdolescents)

########################################################### ##

########################################################### #
# opposite sex couples

library(dplyr)

# demonstrate matched dataframe sizes first
set.seed(1)
PartneredFemales <- Township %>%
  filter(Sex == "Female", Relationship == "Partnered")
PartneredMalesSmall <- Township %>%
  filter(Sex == "Male", Relationship == "Partnered") %>%
  slice_sample(n = nrow(PartneredFemales))

OppSexCouples1 <- couples(PartneredFemales, smlidcol=3, smlagecol=4,
                          PartneredMalesSmall, lrgidcol=3, lrgagecol=4, directxi = -2,
                          directomega = 3, hhidstart = 100, hhidvar="HouseholdID",
                          userseed = 4, ptostop=.01, numiters=1000000)

# there are more partnered males than partnered females
# so all partnered males will have a matched female partner
# but not all females will be matched
# being the smallest data frame, the female one must be the first

# different size dataframes
set.seed(1)
PartneredFemales <- Township %>%
  filter(Sex == "Female", Relationship == "Partnered")

PartneredMales <- Township %>%
  filter(Sex == "Male", Relationship == "Partnered")

OppSexCouples2 <- couples(PartneredFemales, smlidcol=3, smlagecol=4,
                          PartneredMales, lrgidcol=3, lrgagecol=4, directxi = -2,
                          directomega = 3, hhidstart = 100, hhidvar="HouseholdID",
                          userseed = 4, ptostop=.01, numiters=1000000)

# go to the graphs in SettingUpPopulate.R

rm(PartneredFemales, PartneredMales, OppSexCouples, PartneredMalesSmall, OppSexCouples2, TheMatched)




########################################################### ##
# Assign children
########################################################### #
# assign child, no parental household
set.seed(1)
# sample a combination of females and males to be parents
Parents <- Township %>%
  filter(Relationship == "Partnered", Age > 18) %>%
  slice_sample(n = 500)

Children <- Township %>%
  filter(Relationship == "NonPartnered", Age < 20) %>%
  slice_sample(n = 200)

# match the children to the parents
# no ID on the parents

ChildAllMatched <- childno(Children, chlidcol = 3, chlagecol = 4, Parents, paridcol = 3, paragecol = 4,
                                   directxi = 30, directomega = 3, alphaused = 1.2, minparage = 18,
                                   maxparage = 54, hhidstart = 100, hhidvar = "HouseholdID", userseed=4)

# go to SettingUpPopulateR for graphs

# NOT USED
# set.seed(1)
# ShorterParents <- Township %>%
#   filter(Relationship == "Partnered", Age > 18) %>%
#   slice_sample(n = 200)
#
# ChildWithNonMatches<- childno(Children, chlidcol = 3, chlagecol = 4, ShorterParents, paridcol = 3, paragecol = 4,
#                    directxi = 30, directomega = 3, alphaused = 1.2, minparage = 18,
#                    maxparage = 54, hhidstart = 100, hhidvar = "HouseholdID", userseed = 4)
#
# LookAtLast <- ChildWithNonMatches$Matched
# LookAtFirst <- ChildAllMatched$Matched
#
# # get larger samples
# set.seed(1)
# ChildrenBigger <- Township %>%
#   filter(Relationship == "NonPartnered", Age < 20) %>%
#   slice_sample(n = 1000)
#
# LargerParents <-  Township %>%
#   filter(Relationship == "Partnered", Age > 18) %>%
#   slice_sample(n = 1000)
#
# MoreMatches <- childno(ChildrenBigger, chlidcol = 3, chlagecol = 4, LargerParents, paridcol = 3, paragecol = 4,
#                        directxi = 30, directomega = 3, alphaused = 1.2, minparage = 18,
#                        maxparage = 54, hhidstart = 100, hhidvar = "HouseholdID", userseed = 4)
#
# # get everyone
# ChildrenAll <- Township %>%
#   filter(Relationship == "NonPartnered", Age < 20)
#
# ParentsAll <-  Township %>%
#   filter(Relationship == "Partnered", Age > 18)
#
# MatchesAll <- childno(ChildrenAll, chlidcol = 3, chlagecol = 4, ParentsAll, paridcol = 3, paragecol = 4,
#                        directxi = 30, directomega = 3, alphaused = 1.2, minparage = 18,
#                        maxparage = 54, hhidstart = 100, hhidvar = "HouseholdID", userseed = 4)
# rm(ShorterParents, ChildWithNonMatches, LookAtLast, LookAtFirst, ChildrenBigger, LargerParents,
# MoreMatches, ChildrenAll, ParentsAll, MatchesAll)

# go to graphs in SettingUpPopulate.R


rm(Parents, Children, ChildAllMatched, ShorterParents, ChildWithNonMatches)

########################################################### ##

########################################################### #
# assign child, parent has household ID
set.seed(1)
Parents <- Township %>%
  filter(Relationship == "Partnered", Age > 18) %>%
  slice_sample(n = 500) %>%
  mutate(HouseholdID = row_number()+500)

Children <- Township %>%
  filter(Relationship == "NonPartnered", Age < 20) %>%
  slice_sample(n = 200)


ChildMatchedID <- childyes(Children, chlidcol = 3, chlagecol = 4, Parents, paridcol = 3, paragecol = 4,
                         directxi = 30, directomega = 3, alphaused = 1.2, minparage = 18,
                         maxparage = 54, hhidcol = 6, userseed = 4)

# go to SettingUpPopulateR for graphs


rm(Parents, Children, OutputWithID, ParentKidDiffs, AgeDiffs)

########################################################### ##

########################################################### #
# assign multiple children, no parental household

library("dplyr")
set.seed(1)
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

KidsMatched <- childrenyes(Children, chlidcol = 3, chlagecol = 4, numchild = 2, twinrate = .2,
                           Parents, paridcol = 3, paragecol = 4, minparage = 18, maxparage = 54,
                           hhidcol = 6, UserSeed = 4)



rm(Parents, Children, ParentsAndKids)

# with no household ID
library("dplyr")
set.seed(1)
Parents <- Township %>%
  filter(Relationship == "Partnered", Age > 18) %>%
  slice_sample(n = 200)

Children <- Township %>%
  filter(Relationship == "NonPartnered", Age < 20) %>%
  slice_sample(n = 400)

# example with assigning two children to a parent
# the same number of children is assigned to all parents
# adding two children to each parent

KidsMatched <- childrenno(Children, chlidcol = 3, chlagecol = 4, numchild = 2,
                     twinrate = .2, Parents, paridcol = 3, paragecol = 4,
                     minparage = 18, maxparage = 54, hhidstart = 501, hhidvar= "TheHousehold",
                     UserSeed = 4)



###########################################################
# Add people to households
###########################################################
library("dplyr")

# people into new households
NewHouseholds <- otherno(AdultsNoID, pplidcol = 3, pplagecol = 4, numppl = 3, sdused = 3,
                         hhidstart = 1, hhidvar= "TheHousehold", userseed=4, ptostop = .01,
                         numiters = 5000)

# people into existing households
# the people to add data frame is smaller than required, by 40 people
AdultsID <- IntoSchools %>%
  filter(Age > 20)

NoHousehold <- Township %>%
  filter(Age > 20, Relationship == "NonPartnered", !(ID %in% c(AdultsID$ID))) %>%
  slice_sample(n = 1500)

OldHouseholds <- otheryes(AdultsID, exsidcol = 3, exsagecol = 4, hhidcol = 7,
                          NoHousehold, addidcol = 3, addagecol = 4, numppl = 2, sdused = 3,
                          userseed=4, ptostop = .01, numiters = 5000)


########################################################### ##
# add children to schools
########################################################### #

library("dplyr")

SchoolsAdded <- schooladd(IntoSchools, indidcol = 3, indagecol = 4, indsxcol = 8, indstcol = 6, hhidcol = 7,
                          SchoolsToUse, schidcol = 2, schagecol = 4, schrollcol = 5, schtypecol = 3,
                          UserSeed = 4)

# uses a character school ID
SchoolsAdded <- schooladd(IntoSchools, pplidcol = 3, pplagecol = 4, pplsxcol = 8, pplstcol = 6,  hhidcol = 7,
                          SchoolsToUse, schidcol = 2, schagecol = 4, schrollcol = 5, schtypecol = 3,
                          UserSeed = 4)
# # test with numeric school ID
# SchoolsAdded <- schooladd(IntoSchools, pplidcol = 3, pplagecol = 4, pplsxcol = 8, pplstcol = 6,  hhidcol = 7,
#                           SchoolsToUse, schidcol = 1, schagecol = 4, schrollcol = 5, schtypecol = 3,
#                           UserSeed = 4)


table(SchoolsAdded$SchoolID, SchoolsAdded$Sex)








########################################################### ##
# allocate employers to employees
########################################################### #

library("dplyr")

EmployedPeople <- empadd(EmployerSet, empid = 3, empcount = 2, Township, wrkid = 3, hourscol = 5,
                         hoursmin = 2, userseed = 4)






