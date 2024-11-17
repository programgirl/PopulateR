



###########################################################
# addemp example
###########################################################

library(dplyr)

EmployedPeople <- addemp(EmployerSet, empid = "Company", empcount = "NumEmployees", Township,
                          pplid = "ID", wrkhrs = "HoursWorked", hoursmin = 2, missval = "NA", userseed = 4)







###########################################################
# addind example
###########################################################
library("dplyr")

WithInd <- addind(Township, pplid = "ID", pplsx = "Sex", pplage = "Age", pplyear = 2018, minedage = 5,
                  maxedage = 18, LeftSchool, lvrsx = "Sex", lvrage = "Age", lvryear = "YearLeft",
                  lvrcount = "Total", RegionalStructure, pyrsx = "Sex", pyrage = "Age", pyrcount = "Value",
                  stvarname = "Status", userseed = 4)






########################################################### ##
# addnetwork examples
########################################################### #
library("dplyr")

NetworksMadeN <- addnetwork(Ppl4networks, "ID", "Age", NetworkMatrix, sdused=2,
                            probsame = .5, userseed=4, numiters = 10)

NetworksMadeY <- addnetwork(Ppl4networks, "ID", "Age", NetworkMatrix, sdused=2,
                            probsame = .5, userseed=4, numiters = 10, usematrix = "Y")

# smaller examples for the article

set.seed(2) # small datasets can cause problems if a random seed is used for sampling
SmallDemo <- Township %>%
  filter(between(Age, 20, 29)) %>%
  slice_sample(n = 20)

Smallnetwork <- rpois(n = nrow(SmallDemo), lambda = 1.5)

NetworkSmallN <- addnetwork(SmallDemo, "ID", "Age", Smallnetwork, sdused=2,
                            probsame = .5, userseed=4, numiters = 10, usematrix = "N")

plot(NetworkSmallN)







#############################################################
# addschool example
#############################################################

library(dplyr)

SchoolsAdded <- addschool(IntoSchools, pplid = "ID", pplage = "Age", pplsx = "SexCode", pplst = "SchoolStatus",
                          hhid = "HouseholdID", SchoolsToUse, schid = "School.Name", schage = "AgeInRoll",
                          schroll = "RollCount", schtype = "Gender", schmiss = 0, sameprob = .8, userseed = 4)

Population <- SchoolsAdded$Population
Schools <- SchoolsAdded$Schools

KidsInSchool <- Population %>%
  filter(SchoolStatus == "Y")

table(KidsInSchool$School.Name)








#############################################################
# agedis example
#############################################################

library(dplyr)

DisaggregateAge <- agedis(InitialDataframe, indsx = "Sex", minage = "LowerAge", maxage = "UpperAge",
                          pyramid = SingleAges, pyrsx = "Sex", pyrage = "Age", pyrcount = "Value",
                          agevarname = "TheAge", userseed = 4)






############################################################
# createemp example
############################################################
library("dplyr")

TownshipEmployment <- createemp(AllEmployers, industry = "ANZSIC06", indsmin = "minCo", indsmax = "maxCo",
                                pplmin = "minStaff", pplmax = "maxStaff", stffname="NumEmployees",
                                cpyname="Company", userseed = 4)







############################################################
# diffsample examples
############################################################

library("dplyr")

SampleNeeded <- data.frame(Age = c(16, 17, 18),
                           NumNeeded = c(5, 10, 15))

SampledAdolescents <- diffsample(WorkingAdolescents, pplage = "Age", sampledf = SampleNeeded, smplage = "Age",
                                 smplcounts = "NumNeeded", userseed = 4)

table(SampledAdolescents$Age)







############################################################
# fastmatch example
############################################################

library("dplyr")

PersonDataframe <- data.frame(cbind(PersonID = c(1:1000),
                                    PersonAge = c(round(runif(200, min=18, max=23),0),
                                                  round(runif(300, min=24, max=50),0),
                                                  round(runif(500, min=51, max=90),0))))

# unweighted example, probability of being in a same-sex couple is 0.03

Unweighted <- fastmatch(PersonDataframe, pplage = "PersonAge", probSS = 0.03, HHStartNum = 1,
                        HHNumVar = "Household", userseed = 4)

NumUnweighted <- Unweighted %>%
  filter(between(PersonAge, 25, 54))

# prop is
nrow(NumUnweighted)/nrow(Unweighted)

# weighted example, same probability, 66% of people in a same-sex relationship are aged betwene 25 and 54

Weighted <- fastmatch(PersonDataframe, pplage = "PersonAge", probSS = 0.03, uwProp = .66, uwLA = 25,
                      uwUA = 54, HHStartNum = 1, HHNumVar = "Household", userseed = 4)

NumWeighted <- Weighted %>%
  filter(between(PersonAge, 25, 54))

# prop is
nrow(NumWeighted)/nrow(Weighted)








############################################################
# fixhours examples
############################################################

library("dplyr")

# table of hours by schoolstatus
table(WorkingAdolescents$HoursWorked, WorkingAdolescents$SchoolStatus)

# one grouping variable
Group1 <- "Age"
OneGroup <- fixhours(WorkingAdolescents, pplid = "ID", pplstat = "SchoolStatus", pplhours = "HoursWorked",
                     hoursmax = 3, grpdef = Group1, userseed = 4)
table(OneGroup$HoursWorked, OneGroup$SchoolStatus)

# two grouping variables
Group2 <- c("Age", "Sex")
TwoGroups <- fixhours(WorkingAdolescents, pplid = "ID", pplstat = "SchoolStatus", pplhours = "HoursWorked",
                      hoursmax = 3, grpdef = Group2, userseed = 4)
table(TwoGroups$HoursWorked, TwoGroups$SchoolStatus)


table(WorkingAdolescents$HoursWorked, WorkingAdolescents$Age, WorkingAdolescents$SchoolStatus)
table(OneGroup$HoursWorked, OneGroup$Age, OneGroup$SchoolStatus)






###########################################################
# fixrelations
###########################################################

# create the expected proportion of people in relationships, by age within sex
library("dplyr")
thegroups <- as.vector("Sex")

GroupInfo <- rbind(GroupInfo, list("Male", "Under 20 Years", 19, 19, "Partnered", 0, 19),
                   list("Female", "Under 20 Years", 19, 19, "Partnered", 0, 19))

RelProps <- interdiff(GroupInfo, pplage = "MidPoints", pplprop = "RelProps", endmin = "MinAge",
                      endmax = "MaxAge", grpdef = thegroups)
# add in the age groups
RelProps <- RelProps %>%
  mutate(AgeBand = ifelse(Age==19, "Under 20 Years",
                   ifelse(between(Age, 20, 29), "20-29 Years",
                   ifelse(between(Age, 30, 39), "30-39 Years",
                   ifelse(between(Age, 40, 49), "40-49 Years",
                   ifelse(between(Age, 50, 59), "50-59 Years",
                   ifelse(between(Age, 60, 69), "60-69 Years",
                   ifelse(between(Age, 70, 79), "70-79 Years", "80-90 Years"))))))))

# use this to amend the proportion of people within relationships, by age within sex

joinwith <- c("Age", "Sex")
thegroups <- c("Sex", "AgeBand")

FinalRels <- fixrelations(BadRels, pplid = "ID", pplage = "Age", pplstat = "Relationship",
                          stfixval = "Partnered", props = RelProps, propcol = "Fits", grpdef = thegroups,
                          matchdef = joinwith, userseed = 4)









###########################################################
# interdiff
###########################################################

library("dplyr")

thegroups <- as.vector("Sex")
RelProps <- interdiff(GroupInfo, pplage = "MidPoints", pplprop = "RelProps", endmin = "MinAge",
                      endmax = "MaxAge", grpdef = thegroups)






###########################################################
# other example
###########################################################

library("dplyr")

# creating three-person households
NewHouseholds <- other(AdultsNoID, pplid = "ID", pplage = "Age", numppl = 3, sdused = 3, HHStartNum = 1,
                       HHNumVar = "Household", userseed=4, ptostop = .01, numiters = 1000000)

PeopleInHouseholds <- NewHouseholds$Matched
PeopleNot <- NewHouseholds$Unmatched      # 2213 not divisible by 3








###########################################################
# otherNum example
###########################################################
# people into existing households
# the people to add data frame is smaller than required, by 40 people
library("dplyr")

AdultsID <- IntoSchools %>%
  filter(Age > 20) %>%
  select(-c(SchoolStatus, SexCode))

set.seed(2)
NoHousehold <- Township %>%
  filter(Age > 20, Relationship == "NonPartnered", !(ID %in% c(AdultsID$ID))) %>%
  slice_sample(n = 1500)

OldHouseholds <- otherNum(AdultsID, exsid = "ID", exsage = "Age", HHNumVar = "HouseholdID",
                          NoHousehold, addid = "ID", addage = "Age", numadd = 2, sdused = 3,
                          userseed=4, attempts= 10, numiters = 10000)

CompletedHouseholds <- OldHouseholds$Matched
IncompleteHouseholds <- OldHouseholds$Existing
UnmatchedOthers <- OldHouseholds$Additions



###########################################################
###########################################################
# adding children examples
###########################################################
###########################################################


###########################################################
# pairbeta4 example
###########################################################

library(dplyr)

# demonstrate matched dataframe sizes first

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
# no household ID on the parents

ChildAllMatched <- pairbeta4(Children, smlid = "ID", smlage = "Age", Parents, lrgid = "ID", lrgage = "Age",
                           shapeA = 2.2, shapeB = 3.7, locationP = 16.5, scaleP = 40.1, HHStartNum = 1,
                           HHNumVar = "Household", userseed=4, ptostop = .01, numiters = 1000000)


MatchedPairs <- ChildAllMatched$Matched
UnmatchedChildren <- ChildAllMatched$Smaller
UnmatchedAdults <- ChildAllMatched$Larger

# children data frame is larger, the locationP and scaleP values are negative

Parents2 <- Township %>%
  filter(Relationship == "Partnered", Age > 18) %>%
  slice_sample(n = 200)
Children2 <- Township %>%
  filter(Relationship == "NonPartnered", Age < 20) %>%
  slice_sample(n = 500)

# comparison of distributions depending on which data frame contains the oldest ages
test1 <- PearsonDS::rpearsonI(n = 10000, a = 2.2, b = 3.7, location = 16.5, scale = 40.1)
test2 <- PearsonDS::rpearsonI(n = 10000, a = 2.2, b = 3.7, location = -16.5, scale = -40.1)

# negatives for both location and scale are the correct converse for positive location and scale.

ChildMatched <- pairbeta4(Parents2, smlid = "ID", smlage = "Age", Children2, lrgid = "ID", lrgage = "Age",
                             shapeA = 2.2, shapeB = 3.7, locationP = -16.5, scaleP = -40.1, HHStartNum = 1,
                             HHNumVar = "Household", userseed=4, ptostop = .01, numiters = 1000000)

MatchedPairs2 <- ChildMatched$Matched
UnmatchedChildren2 <- ChildMatched$Smaller
UnmatchedAdults2 <- ChildMatched$Larger










###########################################################
# pairbeta4Num example
###########################################################

library(dplyr)

# demonstrate matched dataframe sizes first

set.seed(1)
# sample a combination of females and males to be parents
Parents <- Township %>%
  filter(Relationship == "Partnered", Age > 18) %>%
  slice_sample(n = 500) %>%
  mutate(Household = row_number())
Children <- Township %>%
  filter(Relationship == "NonPartnered", Age < 20) %>%
  slice_sample(n = 200)

# match the children to the parents
ChildAllMatched <- pairbeta4Num(Children, smlid = "ID", smlage = "Age", Parents, lrgid = "ID", lrgage = "Age",
                                shapeA = 2.2, shapeB = 3.7, locationP = 16.5, scaleP = 40.1,
                                HHNumVar = "Household", userseed=4, attempts = 10, numiters = 10000)


MatchedPairs <- ChildAllMatched$Matched
UnmatchedChildren <- ChildAllMatched$Smaller # all children matched
UnmatchedAdults <- ChildAllMatched$Larger

# children data frame is larger, the locationP and scaleP values are negative

Parents2 <- Township %>%
  filter(Relationship == "Partnered", Age > 18) %>%
  slice_sample(n = 200) %>%
  mutate(Household = row_number())
Children2 <- Township %>%
  filter(Relationship == "NonPartnered", Age < 20) %>%
  slice_sample(n = 500)

ChildMatched <- pairbeta4Num(Parents2, smlid = "ID", smlage = "Age", Children2, lrgid = "ID", lrgage = "Age",
                             shapeA = 2.2, shapeB = 3.7, locationP = -16.5, scaleP = -40.1,
                             HHNumVar = "Household", userseed=4, attempts = 10, numiters = 10000)

MatchedPairs2 <- ChildMatched$Matched
UnmatchedChildren2 <- ChildMatched$Smaller
UnmatchedAdults2 <- ChildMatched$Larger











#############################################################
# pairmult
#############################################################

library(dplyr)

# no parental household
set.seed(1)
# sample a combination of females and males to be parents
Parents <- Township %>%
  filter(Relationship == "Partnered", Age > 18) %>%
  slice_sample(n = 500)

Children <- Township %>%
  filter(Relationship == "NonPartnered", Age < 20) %>%
  slice_sample(n = 200)

# example with assigning two children to a parent
# the same number of children is assigned to all parents
# adding two children to each parent

ChildMatched <- pairmult(Children, chlid = "ID", chlage = "Age", numchild = 2, twinprob = 0.03,
                         Parents, parid = "ID", parage = "Age", minparage = 18, maxparage = 54,
                         HHStartNum = 1, HHNumVar = "Household", userseed=4, maxdiff = 3)
MatchedFamilies <- ChildMatched$Matched
UnmatchedChildren <- ChildMatched$Children
UnmatchedAdults <- ChildMatched$Adults

# affected by the permitted age difference between children
ChildMatched2 <- pairmult(Children, chlid = "ID", chlage = "Age", numchild = 2, twinprob = 0.03,
                          Parents, parid = "ID", parage = "Age", minparage = 18, maxparage = 54,
                          HHStartNum = 1, HHNumVar = "Household", userseed=4, maxdiff = 4)
MatchedFamilies2 <- ChildMatched2$Matched
UnmatchedChildren2 <- ChildMatched2$Children
UnmatchedAdults2 <- ChildMatched2$Adults










###########################################################
# pairmultNum example
###########################################################

library(dplyr)

set.seed(1)
# sample a combination of females and males to be parents
Parents <- Township %>%
  filter(Relationship == "Partnered", Age > 18) %>%
  slice_sample(n = 500) %>%
  mutate(Household = row_number())

Children <- Township %>%
  filter(Relationship == "NonPartnered", Age < 20) %>%
  slice_sample(n = 200)

# example with assigning two children to a parent
# the same number of children is assigned to all parents
# adding two children to each parent

ChildMatched <- pairmultNum(Children, chlid = "ID", chlage = "Age", numchild = 2, twinprob = 0.03, Parents,
                            parid = "ID", parage = "Age", minparage = 18, maxparage = 54,
                            HHNumVar = "Household", userseed =4, maxdiff = 3)
MatchedFamilies <- ChildMatched$Matched
UnmatchedChildren <- ChildMatched$Children
UnmatchedAdults <- ChildMatched$Adults

# affected by the permitted age difference between children
ChildMatched2 <- pairmultNum(Children, chlid = "ID", chlage = "Age", numchild = 2, twinprob = 0.03, Parents,
                             parid = "ID", parage = "Age", minparage = 18, maxparage = 54,
                             HHNumVar = "Household", userseed =4, maxdiff = 4)
MatchedFamilies2 <- ChildMatched2$Matched
UnmatchedChildren2 <- ChildMatched2$Children
UnmatchedAdults2 <- ChildMatched2$Adults





###########################################################
###########################################################
# adding children examples finished
###########################################################
###########################################################

















#############################################################
# pairnorm
#############################################################

library(dplyr)

# demonstrate matched dataframe sizes first, using a normal distribution
set.seed(1)
PartneredFemales <- Township %>%
  filter(Sex == "Female", Relationship == "Partnered")
PartneredMalesSmall <- Township %>%
  filter(Sex == "Male", Relationship == "Partnered") %>%
  slice_sample(n = nrow(PartneredFemales))

OppSexCouples1 <- pairnorm(PartneredFemales, smlid = "ID", smlage = "Age", PartneredMalesSmall, lrgid = "ID",
                          lrgage = "Age", directxi = -2, directomega = 3, HHStartNum = 1, HHNumVar = "HouseholdID",
                          userseed = 4, ptostop=.01)

Couples1 <- OppSexCouples1$Matched

# there are more partnered males than partnered females
# so all partnered males will have a matched female partner
# but not all females will be matched
# being the smallest data frame, the female one must be the first

# different size dataframes
PartneredFemales <- Township %>%
  filter(Sex == "Female", Relationship == "Partnered")

PartneredMales <- Township %>%
  filter(Sex == "Male", Relationship == "Partnered")

OppSexCouples2 <- pairnorm(PartneredFemales, smlid = "ID", smlage = "Age", PartneredMales, lrgid = "ID",
                           lrgage = "Age", directxi = -2, directomega = 3, HHStartNum = 1, HHNumVar="HouseholdID",
                           userseed = 4, ptostop=.01)

Couples2 <- OppSexCouples2$Matched

# 21 males not matched
NotMatched2 <- OppSexCouples2$Unmatched

# repeat first example using a skew normal distribution
# doesn't converge
OppSexCouples3 <- pairnorm(PartneredFemales, smlid = "ID", smlage = "Age", PartneredMalesSmall, lrgid = "ID",
                           lrgage = "Age", directxi = -2, directomega = 3, alphaused = 5, HHStartNum = 1,
                           HHNumVar = "HouseholdID", userseed = 4, ptostop=.01)

Couples3 <- OppSexCouples3$Matched

































#############################################################
#############################################################
# not working
#############################################################
#############################################################



#############################################################
# pairln
#############################################################

library(dplyr)

library(dplyr)

# demonstrate matched dataframe sizes first, using a normal distribution
set.seed(1)
PartneredFemales <- Township %>%
  filter(Sex == "Female", Relationship == "Partnered")
PartneredMalesSmall <- Township %>%
  filter(Sex == "Male", Relationship == "Partnered") %>%
  slice_sample(n = nrow(PartneredFemales))

OppSexCouples1 <- pairln(PartneredFemales, smlid = "ID", smlage = "Age", PartneredMalesSmall, lrgid = "ID",
                         lrgage = "Age", lnmean = -0.693, lnsd = 1.386, HHStartNum = 1, HHNumVar = "HouseholdID",
                         userseed = 4, ptostop=.01)

Couples1 <- OppSexCouples1$Matched
