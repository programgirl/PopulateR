################################################################################################
################################################################################################
################################################################################################
################################    Children at School      ####################################
################################################################################################
################################################################################################
################################################################################################
# #get number of schoolchildren, subset into children file
# Master_file_children <- Master_household_file_Timaru_2013_census_data[which((Master_household_file_Timaru_2013_census_data$HRSWORKED=="Not Working" |
#                                                                                Master_household_file_Timaru_2013_census_data$HRSWORKED=="Not Elsewhere Included") &
#                                                                               (Master_household_file_Timaru_2013_census_data$AGEBAND>=2 &
#                                                                                  Master_household_file_Timaru_2013_census_data$AGEBAND<=4)),]
# #sort Master_file_children by household ID
# Master_file_children <- Master_file_children[order(Master_file_children$`Household Number`,Master_file_children$AGEBAND),]

################################################################################################
################################################################################################
################################################################################################
#pro rata the simulated children
#get the age proportions from the school roll data, by simulated data age bands
#see individual age script files
################################################################################################
################################################################################################
################################################################################################
################################################################################################
################################################################################################
##################### Master file of children with individual ages #############################
################################################################################################
###############################################################################################
Master_file_children_with_ages <- rbind(Aged5to9WithAges,Aged10to14WithAges, Aged15to17WithAges)
#remove all construction files
rm(age5to9counts,age5to9vals,Age5to9_twin_possible_households,Age5to9_twins_per_household,Age5to9_which_twins,Age5to9households,Age5to9NoAges,
   Aged5to9MultiplesWithAges,Aged5to9NoDups,Aged5to9NoDups,Aged5to9Twin2Original,Aged5to9TwinOriginal,Aged5to9TwinPairs,
   Aged5to9TwinPairsNoDupAges,Aged5to9Twins1,Aged5to9Twins1NoDups,Aged5to9Twins2,Aged5to9Twins2NoDups,Aged5to9TwinsDifferentAges,Aged5to9TwinsWithAges)
rm(age10to14counts,age10to14vals,Age10to14_twin_possible_households,Age10to14_twins_per_household,Age10to14_which_twins,Age10to14households,Age10to14NoAges,
   Aged10to14MultiplesWithAges,Aged10to14NoDups,Aged10to14NoDups,Aged10to14Twin2Original,Aged10to14TwinOriginal,Aged10to14TwinPairs,
   Aged10to14TwinPairsNoDupAges,Aged10to14Twins1,Aged10to14Twins1NoDups,Aged10to14Twins2,Aged10to14Twins2NoDups,Aged10to14TwinsDifferentAges,Aged10to14TwinsWithAges)
rm(age15to17counts,age15to17vals,Age15to17_twin_possible_households,Age15to17_twins_per_household,Age15to17_which_twins,Age15to17households,Age15to17NoAges,
   Aged15to17MultiplesWithAges,Aged15to17NoDups,Aged15to17NoDups,Aged15to17Twin2Original,Aged15to17TwinOriginal,Aged15to17TwinPairs,
   Aged15to17TwinPairsNoDupAges,Aged15to17Twins1,Aged15to17Twins1NoDups,Aged15to17Twins2,Aged15to17Twins2NoDups,Aged15to17TwinsDifferentAges,Aged15to17TwinsWithAges)
rm(CheckDups2,CheckDupsCount,CheckDupsCount2,Duplicated_children10to14MoreThanTriplets,Duplicated_children10to14MoreThanTriplets_2,Duplicated_children10to14Triplets,
Duplicated_children10to14Triplets_2,Duplicated_children10to14Triplets_2Only,Duplicated_children10to14Try1,Duplicated_children10to14Try1_2,
Duplicated_children15to17MoreThanTriplets,Duplicated_children15to17MoreThanTriplets_2,Duplicated_children15to17Triplets,Duplicated_children15to17Triplets_2,
Duplicated_children15to17Triplets_2Only,Duplicated_children15to17Try1,Duplicated_children15to17Try1_2,Duplicated_children5to9MoreThanTriplets,
Duplicated_children5to9MoreThanTriplets_2,Duplicated_children5to9Triplets,Duplicated_children5to9Triplets_2,Duplicated_children5to9Triplets_2Only,
Duplicated_children5to9Try1,Duplicated_children5to9Try1_2)
# 
# #Ministry of Health data for 2013 indicates a twin rate of 1.5%, so there should be around 3824*0.015=57 twin pairs
# #so expected duplicates = 115
# #what is ageband rate in original data?
# temp <- Master_file_children[duplicated(Master_file_children_with_ages[c(8,10)]) | 
#                                duplicated(Master_file_children_with_ages[c(8,10)], fromLast = TRUE),]
# 
# (nrow(temp)/nrow(Master_file_children_with_ages))*100
# there are 126 duplicates, from a twin rate of 3.29%
# this is acceptable





################################################################################################
################################################################################################
################################################################################################
################################ Assignment to schools      ####################################
################################################################################################
################################################################################################
#process - work with largest age group first
#check overall female and male ratios
sum(TimaruSchoolMeshblocks$Female) #2676 girls across all schools
sum(TimaruSchoolMeshblocks$Male)   #2675 boys across all schools
#assume equal numbers in each age group, except when we come to the colleges - the same sex schools complicate matters
#okay, so if equal numbers boys and girls,
#4.1x2 = 8.2% chance of any 12-year old girl going to the 7-15 college
#5.4x2 = 10.8% chance of any 11-year old girl going to the 7-15 college

################################################################################################
############ Set schools to factor to remove hidden original omitted schools      ##############
################################################################################################
################################################################################################
TimaruSchoolMeshblocks$School.Name <- factor(TimaruSchoolMeshblocks$School.Name)

################################################################################################
################################################################################################
#############################        Use largest count       ###################################
################################################################################################
################################################################################################
colSums(TimaruSchoolMeshblocks[,c(8:20)])
#Age 15 is the largest college age with 561 students, age 6 is the largest primary school age with 364 students

################################################################################################
################################################################################################
#work on colleges first
################################################################################################
################################################################################################
################################################################################################
################################################################################################
# use girls first
################################################################################################
################################################################################################
#get percentages for girls only
#assume 50% of the students at the co-ed schools are girls
#easiest to credit subset to work with
TimaruSchools15Girls <- subset(TimaruSchoolMeshblocks, School.Name %in% c("Craighead Diocesan School","Roncalli College",
                                                                          "Mountainview High School","Timaru Girls' High School"),
                               select=c(School.Name,Age.15))
#remove boy counts from coed school data, assume 50% of students are boys
TimaruSchools15Girls$Age.15 <- with(TimaruSchools15Girls, ifelse(School.Name %in% c("Roncalli College", "Mountainview High School"), 
                                                                 Age.15/2, Age.15))
#get percents
TimaruSchools15Girls$Percent <- TimaruSchools15Girls$Age.15/sum(TimaruSchools15Girls$Age.15)

#Assign the 15-year-old girls to a school based on the proportions in the school roll data
Aged15AssignmentGirls <- subset(Master_file_children_with_ages, AssignedAge==15 & SEX=="Female")

#see if the household numbers are unique
Aged15AssignmentGirls$`Household Number`[duplicated(Aged15AssignmentGirls$`Household Number`)]
#they are, proceed with allocation
#construct temporary Girls College data frame excluding the 15-year olds
WorkingFileCollegeGirls <- subset(Master_file_children_with_ages, AssignedAge >= 11 & AssignedAge!=15 & SEX=="Female")

set.seed(306181056)
Aged15AssignmentGirls$School <- sample(TimaruSchools15Girls$School.Name,
                                       size=nrow(Aged15AssignmentGirls), replace=TRUE, prob=TimaruSchools15Girls$Percent)


#pull in girls from matched household numbers to assign to the same school
# will need to match college ages only, ignore if primary
AdditionalCollegeGirlsMatched <- subset(WorkingFileCollegeGirls, `Household Number` %in% Aged15AssignmentGirls$`Household Number`)
#35 matches on household
#assign the 35 matches to the same school - But!!!
#11 year olds can only go to Craighead
#there is only one 12-year old at a college other than Craighead, and that is a boy so ignore
#check if there are any 11-year and 12-year olds
table(AdditionalCollegeGirlsMatched$AssignedAge)
#there are 3 11-year-olds and 3 12-year-olds
#assign age, will check 11-year-olds and 12-year olds afterwards
AdditionalCollegeGirlsMatched$School <- Aged15AssignmentGirls$School[match(AdditionalCollegeGirlsMatched$`Household Number`,Aged15AssignmentGirls$`Household Number`)]
#remove college matches from the 11-year-olds and 12-year-olds not at Craighead
AdditionalCollegeGirlsMatched$School <- ifelse((AdditionalCollegeGirlsMatched$AssignedAge==11 | AdditionalCollegeGirlsMatched$AssignedAge==12) & 
                                                 AdditionalCollegeGirlsMatched$School!="Craighead Diocesan School", "", as.character(AdditionalCollegeGirlsMatched$School))
#Remove the unmatched 11-year-olds and 12-year olds from the college data, these are in primary school
AdditionalCollegeGirlsMatched <- subset(AdditionalCollegeGirlsMatched, AdditionalCollegeGirlsMatched$School!="")
#need a combined file of all matched girls, updated as we go
#construct a file of all who are matched
WorkingMatchedFile <- rbind(Aged15AssignmentGirls,AdditionalCollegeGirlsMatched)
#decrement the master working file of college girls by those already matched
WorkingFileCollegeGirls <- WorkingFileCollegeGirls[!(WorkingFileCollegeGirls$ID %in% WorkingMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools15Girls,Aged15AssignmentGirls)

################################################################################################
#adjust probabilities and repeat for the 14-year-olds
################################################################################################
TimaruSchools14Girls <- subset(TimaruSchoolMeshblocks, School.Name %in% c("Craighead Diocesan School","Roncalli College",
                                                                          "Mountainview High School","Timaru Girls' High School"),
                               select=c(School.Name,Age.14))
TimaruSchools14Girls$Age.14 <- with(TimaruSchools14Girls, ifelse(School.Name %in% c("Roncalli College", "Mountainview High School"), 
                                                                 Age.14/2, Age.14))
#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingMatchedFile, AssignedAge==14)
Girls14AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools14Girls <- merge(Girls14AlreadyAssigned,TimaruSchools14Girls, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools14Girls) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools14Girls$AlreadyAssigned[is.na(TimaruSchools14Girls$AlreadyAssigned)] <- 0
TimaruSchools14Girls$Age.14 <- TimaruSchools14Girls[,3]-TimaruSchools14Girls[,2]
#get percents
TimaruSchools14Girls$Percent <- TimaruSchools14Girls$Age.14/sum(TimaruSchools14Girls$Age.14)

#Assign the 14-year-old girls to a school based on the proportions in the school roll data
Aged14AssignmentGirls <- subset(WorkingFileCollegeGirls, AssignedAge==14 & SEX=="Female")

#remove the 14-year old girls from the Working File for college-aged girls
WorkingFileCollegeGirls <- subset(WorkingFileCollegeGirls, AssignedAge!=14)

#see if the household numbers are unique
Aged14AssignmentGirls$`Household Number`[duplicated(Aged14AssignmentGirls$`Household Number`)]

#they are unique
set.seed(237181047)
Aged14AssignmentGirls$School <- sample(TimaruSchools14Girls$School.Name,
                                       size=nrow(Aged14AssignmentGirls), replace=TRUE, prob=TimaruSchools14Girls$Percent)

#pull in girls from matched household numbers to assign to the same school
# will need to match college ages only, ignore if primary
AdditionalCollegeGirlsMatched <- subset(WorkingFileCollegeGirls, `Household Number` %in% Aged14AssignmentGirls$`Household Number`)

#53 matches on household
#assign the 53 matches to the same school - But!!!
#11 year olds and 12-year olds can only go to Craighead
#there is only one 12-year old at a college other than Craighead, and that is a boy so ignore
#check if there are any 11-year olds and 12-year-olds 
table(AdditionalCollegeGirlsMatched$AssignedAge)
# there are 16 11-year olds and 16 12-year-olds 
#assign age, will check 11-year-olds afterwards
AdditionalCollegeGirlsMatched$School <- Aged14AssignmentGirls$School[match(AdditionalCollegeGirlsMatched$`Household Number`,
                                                                           Aged14AssignmentGirls$`Household Number`)]
#remove college matches from the 11-year-olds and 12-year olds not at Craighead
AdditionalCollegeGirlsMatched$School <- ifelse((AdditionalCollegeGirlsMatched$AssignedAge==11 | AdditionalCollegeGirlsMatched$AssignedAge==12)
                                               & AdditionalCollegeGirlsMatched$School!="Craighead Diocesan School", "",as.character(AdditionalCollegeGirlsMatched$School))
#Remove the unmatched 11-year-olds and 12-year olds from the college data, these are in primary school
AdditionalCollegeGirlsMatched <- subset(AdditionalCollegeGirlsMatched, AdditionalCollegeGirlsMatched$School!="")
#need a combined file of all matched girls, updated as we go
#construct a file of all who are matched
WorkingMatchedFile <- rbind(WorkingMatchedFile,Aged14AssignmentGirls,AdditionalCollegeGirlsMatched)
#decrement the master working file of college girls by those already matched
WorkingFileCollegeGirls <- WorkingFileCollegeGirls[!(WorkingFileCollegeGirls$ID %in% WorkingMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools14Girls,Girls14AlreadyAssigned,Aged14AssignmentGirls)

################################################################################################
#adjust probabilities and repeat for the 13-year-olds
################################################################################################
TimaruSchools13Girls <- subset(TimaruSchoolMeshblocks, School.Name %in% c("Craighead Diocesan School","Roncalli College",
                                                                          "Mountainview High School","Timaru Girls' High School"),
                               select=c(School.Name,Age.13))
TimaruSchools13Girls$Age.13 <- with(TimaruSchools13Girls, ifelse(School.Name %in% c("Roncalli College", "Mountainview High School"), 
                                                                 Age.13/2, Age.13))
#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingMatchedFile, AssignedAge==13)
Girls13AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools13Girls <- merge(Girls13AlreadyAssigned,TimaruSchools13Girls, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools13Girls) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools13Girls$AlreadyAssigned[is.na(TimaruSchools13Girls$AlreadyAssigned)] <- 0
TimaruSchools13Girls$Age.13 <- TimaruSchools13Girls[,3]-TimaruSchools13Girls[,2]
#get percents
TimaruSchools13Girls$Percent <- TimaruSchools13Girls$Age.13/sum(TimaruSchools13Girls$Age.13)

#Assign the 13-year-old girls to a school based on the proportions in the school roll data
Aged13AssignmentGirls <- subset(WorkingFileCollegeGirls, AssignedAge==13 & SEX=="Female")

#remove the 13-year old girls from the Working File for college-aged girls
WorkingFileCollegeGirls <- subset(WorkingFileCollegeGirls, AssignedAge!=13)

#see if the household numbers are unique
Aged13AssignmentGirls$`Household Number`[duplicated(Aged13AssignmentGirls$`Household Number`)]

#0 duplicates 
set.seed(123711243)
Aged13AssignmentGirls$School <- sample(TimaruSchools13Girls$School.Name,
                                             size=nrow(Aged13AssignmentGirls), replace=TRUE, prob=TimaruSchools13Girls$Percent)

#pull in girls from matched household numbers to assign to the same school
# will need to match college ages only, ignore if primary
AdditionalCollegeGirlsMatched <- subset(WorkingFileCollegeGirls, `Household Number` %in% Aged13AssignmentGirls$`Household Number`)

#42 matches on household
#assign the 42 matches to the same school - But!!!
#11 year olds can only go to Craighead
#there is only one 12-year old at a college other than Craighead, and that is a boy so ignore
#check if there are any 11-year and 12-year olds
table(AdditionalCollegeGirlsMatched$AssignedAge)
# there are 23 11-year olds and 14 12-year-olds
#assign age, will check 11-year-olds and 12-year olds afterwards
AdditionalCollegeGirlsMatched$School <- Aged13AssignmentGirls$School[match(AdditionalCollegeGirlsMatched$`Household Number`,
                                                                           Aged13AssignmentGirls$`Household Number`)]
#remove college matches from the 11-year-olds and 12-year-olds not at Craighead
AdditionalCollegeGirlsMatched$School <- ifelse((AdditionalCollegeGirlsMatched$AssignedAge==11 | AdditionalCollegeGirlsMatched$AssignedAge==12)
                                               & AdditionalCollegeGirlsMatched$School!="Craighead Diocesan School", "",as.character(AdditionalCollegeGirlsMatched$School))
#Remove the unmatched 11-year-olds and 12-year-olds from the college data, these are in primary school
AdditionalCollegeGirlsMatched <- subset(AdditionalCollegeGirlsMatched, AdditionalCollegeGirlsMatched$School!="")
#need a combined file of all matched girls, updated as we go
#construct a file of all who are matched
WorkingMatchedFile <- rbind(WorkingMatchedFile,Aged13AssignmentGirls,AdditionalCollegeGirlsMatched)
#decrement the master working file of college girls by those already matched
WorkingFileCollegeGirls <- WorkingFileCollegeGirls[!(WorkingFileCollegeGirls$ID %in% WorkingMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools13Girls,Girls13AlreadyAssigned, Aged13AssignmentGirls)

################################################################################################
#adjust probabilities and repeat for the 16-year-olds
################################################################################################
TimaruSchools16Girls <- subset(TimaruSchoolMeshblocks, School.Name %in% c("Craighead Diocesan School","Roncalli College",
                                                                          "Mountainview High School","Timaru Girls' High School"),
                               select=c(School.Name,Age.16))
TimaruSchools16Girls$Age.16 <- with(TimaruSchools16Girls, ifelse(School.Name %in% c("Roncalli College", "Mountainview High School"), 
                                                                 Age.16/2, Age.16))
#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingMatchedFile, AssignedAge==16)
Girls16AlreadyAssigned <- as.data.frame(table(temp$School))

#deduct already matched counts from original counts
TimaruSchools16Girls <- merge(Girls16AlreadyAssigned,TimaruSchools16Girls, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools16Girls) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools16Girls$AlreadyAssigned[is.na(TimaruSchools16Girls$AlreadyAssigned)] <- 0
TimaruSchools16Girls$Age.16 <- TimaruSchools16Girls[,3]-TimaruSchools16Girls[,2]
#get percents
TimaruSchools16Girls$Percent <- TimaruSchools16Girls$Age.16/sum(TimaruSchools16Girls$Age.16)

#Assign the 16-year-old girls to a school based on the proportions in the school roll data
Aged16AssignmentGirls <- subset(WorkingFileCollegeGirls, AssignedAge==16 & SEX=="Female")

#remove the 16-year old girls from the Working File for college-aged girls
WorkingFileCollegeGirls <- subset(WorkingFileCollegeGirls, AssignedAge!=16)

#see if the household numbers are unique
Aged16AssignmentGirls$`Household Number`[duplicated(Aged16AssignmentGirls$`Household Number`)]

#1 duplicate so split duplicate and non-duplicates
Aged16AssignmentGirlsDups <- Aged16AssignmentGirls[duplicated(Aged16AssignmentGirls[c(8)]) ,]
Aged16AssignmentGirlsNoDups <- Aged16AssignmentGirls[!(Aged16AssignmentGirls$ID %in% Aged16AssignmentGirlsDups$ID),]


set.seed(237181314)
Aged16AssignmentGirlsNoDups$School <- sample(TimaruSchools16Girls$School.Name,
                                       size=nrow(Aged16AssignmentGirlsNoDups), replace=TRUE, prob=TimaruSchools16Girls$Percent)

#pull in girls from matched household numbers to assign to the same school
# will need to match college ages only, ignore if primary
AdditionalCollegeGirlsMatched <- subset(WorkingFileCollegeGirls, `Household Number` %in% Aged16AssignmentGirlsNoDups$`Household Number`)
#append the duplicate to this
AdditionalCollegeGirlsMatched <- rbind(AdditionalCollegeGirlsMatched,Aged16AssignmentGirlsDups)
#delete dups file
rm(Aged16AssignmentGirlsDups)

#7 matches on household
#assign the 7 matches to the same school - But!!!
#11-year-olds and 12-year-olds can only go to Craighead
#there is only one 12-year old at a college other than Craighead, and that is a boy so ignore
#check if there are any 11-year-olds or 12-year-olds
table(AdditionalCollegeGirlsMatched$AssignedAge)
# there are 3 11-year-olds and 0 12-year-olds but keep removal syntax the same
#assign school, will check 11-year-olds afterwards
AdditionalCollegeGirlsMatched$School <- Aged16AssignmentGirlsNoDups$School[match(AdditionalCollegeGirlsMatched$`Household Number`,
                                                                                 Aged16AssignmentGirlsNoDups$`Household Number`)]
#remove college matches from the 11-year-olds not at Craighead
AdditionalCollegeGirlsMatched$School <- ifelse((AdditionalCollegeGirlsMatched$AssignedAge==11 | AdditionalCollegeGirlsMatched$AssignedAge==12)
                                               & AdditionalCollegeGirlsMatched$School!="Craighead Diocesan School", "",as.character(AdditionalCollegeGirlsMatched$School))
#Remove the unmatched 11-year-olds from the college data, these are in primary school
AdditionalCollegeGirlsMatched <- subset(AdditionalCollegeGirlsMatched, AdditionalCollegeGirlsMatched$School!="")
#need a combined file of all matched girls, updated as we go
#construct a file of all who are matched
WorkingMatchedFile <- rbind(WorkingMatchedFile,Aged16AssignmentGirlsNoDups,AdditionalCollegeGirlsMatched)
#decrement the master working file of college girls by those already matched
WorkingFileCollegeGirls <- WorkingFileCollegeGirls[!(WorkingFileCollegeGirls$ID %in% WorkingMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools16Girls,Aged16AssignmentGirls,Girls16AlreadyAssigned,Aged16AssignmentGirlsNoDups)

################################################################################################
#adjust probabilities and repeat for the 17-year-olds
################################################################################################
TimaruSchools17Girls <- subset(TimaruSchoolMeshblocks, School.Name %in% c("Craighead Diocesan School","Roncalli College",
                                                                          "Mountainview High School","Timaru Girls' High School"),
                               select=c(School.Name,Age.17))
TimaruSchools17Girls$Age.17 <- with(TimaruSchools17Girls, ifelse(School.Name %in% c("Roncalli College", "Mountainview High School"), 
                                                                 Age.17/2, Age.17))
#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingMatchedFile, AssignedAge==17)
Girls17AlreadyAssigned <- as.data.frame(table(temp$School))

#deduct already matched counts from original counts
TimaruSchools17Girls <- merge(Girls17AlreadyAssigned,TimaruSchools17Girls, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools17Girls) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools17Girls$AlreadyAssigned[is.na(TimaruSchools17Girls$AlreadyAssigned)] <- 0
TimaruSchools17Girls$Age.17 <- TimaruSchools17Girls[,3]-TimaruSchools17Girls[,2]
#get percents
TimaruSchools17Girls$Percent <- TimaruSchools17Girls$Age.17/sum(TimaruSchools17Girls$Age.17)

#Assign the 17-year-old girls to a school based on the proportions in the school roll data
Aged17AssignmentGirls <- subset(WorkingFileCollegeGirls, AssignedAge==17 & SEX=="Female")

#remove the 17-year old girls from the Working File for college-aged girls
WorkingFileCollegeGirls <- subset(WorkingFileCollegeGirls, AssignedAge!=17)

#see if the household numbers are unique
Aged17AssignmentGirls$`Household Number`[duplicated(Aged17AssignmentGirls$`Household Number`)]

#they are unique
set.seed(237181325)
Aged17AssignmentGirls$School <- sample(TimaruSchools17Girls$School.Name,
                                             size=nrow(Aged17AssignmentGirls), replace=TRUE, prob=TimaruSchools17Girls$Percent)

#pull in girls from matched household numbers to assign to the same school
# will need to match college ages only, ignore if primary
AdditionalCollegeGirlsMatched <- subset(WorkingFileCollegeGirls, `Household Number` %in% Aged17AssignmentGirls$`Household Number`)

#4 matches on household
#assign the 4 matches to the same school - But!!!
#11 year olds can only go to Craighead
#there is only one 12-year old at a college other than Craighead, and that is a boy so ignore
#check if there are any 11-year olds
table(AdditionalCollegeGirlsMatched$AssignedAge)
# there is 1 x  11-year old and 3 x 12-year olds
#assign age, will check 11-year-olds and 12-year olds afterwards
AdditionalCollegeGirlsMatched$School <- Aged17AssignmentGirls$School[match(AdditionalCollegeGirlsMatched$`Household Number`,
                                                                           Aged17AssignmentGirls$`Household Number`)]
#remove college matches from the 11-year-olds and 12-year-olds not at Craighead
AdditionalCollegeGirlsMatched$School <- ifelse((AdditionalCollegeGirlsMatched$AssignedAge==11 | AdditionalCollegeGirlsMatched$AssignedAge==12)
                                               & AdditionalCollegeGirlsMatched$School!="Craighead Diocesan School", "",as.character(AdditionalCollegeGirlsMatched$School))
#Remove the unmatched 11-year-olds and 12-year olds from the college data, these are in primary school
AdditionalCollegeGirlsMatched <- subset(AdditionalCollegeGirlsMatched, AdditionalCollegeGirlsMatched$School!="")
#need a combined file of all matched girls, updated as we go
#construct a file of all who are matched
WorkingMatchedFile <- rbind(WorkingMatchedFile,Aged17AssignmentGirls,AdditionalCollegeGirlsMatched)
#decrement the master working file of college girls by those already matched
WorkingFileCollegeGirls <- WorkingFileCollegeGirls[!(WorkingFileCollegeGirls$ID %in% WorkingMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools17Girls, Girls17AlreadyAssigned,Aged17AssignmentGirls)

################################################################################################
#check remaining college age students
################################################################################################
table(WorkingFileCollegeGirls$AssignedAge)
#contains only unmatched 11-year-olds and 12-year-olds
#need to add the unallocated ones of these once we get to the 11- and 12-year olds
################################################################################################
################################################################################################
#work on primary schools now
################################################################################################
################################################################################################
################################################################################################
################################################################################################
# can do co-ed as no single sex schools occur until age 11
################################################################################################
################################################################################################
#use largest count
colSums(TimaruSchoolMeshblocks[,c(8:15)])
################################################################################################
#start with age 6
################################################################################################
#easiest to create subset to work with
TimaruSchools6Coed <- subset(TimaruSchoolMeshblocks, select=c(School.Name,Age.6))
#remove colleges and assume 50% of students are boys
TimaruSchools6Coed <- subset(TimaruSchools6Coed, !(School.Name %in% c("Craighead Diocesan School", "Mountainview High School","Roncalli College", "Timaru Girls' High School",
                                                                "Timaru Boys' High School")))
#get percents
TimaruSchools6Coed$Percent <- TimaruSchools6Coed$Age.6/sum(TimaruSchools6Coed$Age.6)

#Assign the 6-year-olds to a school based on the proportions in the school roll data
Aged6AssignmentCoed <- subset(Master_file_children_with_ages, AssignedAge==6)

#see if the household numbers are unique
Aged6AssignmentCoed$`Household Number`[duplicated(Aged6AssignmentCoed$`Household Number`)]
# 9 households with more than one child aged 6
# so split duplicate and non-duplicates
Aged6AssignmentCoedDups <- Aged6AssignmentCoed[duplicated(Aged6AssignmentCoed[c(8)]) ,]
Aged6AssignmentCoedNoDups <- Aged6AssignmentCoed[!(Aged6AssignmentCoed$ID %in% Aged6AssignmentCoedDups$ID),]


set.seed(267181514)
Aged6AssignmentCoedNoDups$School <- sample(TimaruSchools6Coed$School.Name,
                                             size=nrow(Aged6AssignmentCoedNoDups), replace=TRUE, prob=TimaruSchools6Coed$Percent)


#construct temporary coed primary school data frame excluding the 6-year olds
#but must add the unallocated female 11- and 12-year olds
#plus all the male 11- and 12-year olds
#first, grab all the 5- to 10-year olds, except the 6-year olds as we are currently allocating those
WorkingFilePrimaryChildren <- subset(Master_file_children_with_ages, AssignedAge <=10 & AssignedAge!=6)
#now pull the unallocated 11- and 12-year old girls
WorkingFilePrimaryChildren <- rbind(WorkingFilePrimaryChildren,WorkingFileCollegeGirls)

#add in the 11- and 12-year old boys
temp <- subset(Master_file_children_with_ages, AssignedAge %in% c(11,12) & SEX=="Male")
#add these boys to the working primary school children file
WorkingFilePrimaryChildren <- rbind(WorkingFilePrimaryChildren,temp)

#pull in primary children from matched household numbers to assign to the same school
# will need to match primary ages only
AdditionalPrimaryCoedMatched <- subset(WorkingFilePrimaryChildren, `Household Number` %in% Aged6AssignmentCoedNoDups$`Household Number`)
#append the duplicate to this
AdditionalPrimaryCoedMatched <- rbind(AdditionalPrimaryCoedMatched,Aged6AssignmentCoedDups)
#delete dups file
rm(Aged6AssignmentCoedDups)

#206 matches on household
# assign the 206 matches to the same school
#assign age, will check 11-year-olds afterwards
AdditionalPrimaryCoedMatched$School <- Aged6AssignmentCoedNoDups$School[match(AdditionalPrimaryCoedMatched$`Household Number`,
                                                                                  Aged6AssignmentCoedNoDups$`Household Number`)]

#need a combined file of all matched primary children, updated as we go
#construct a file of all who are matched
WorkingPrimaryMatchedFile <- rbind(Aged6AssignmentCoedNoDups,AdditionalPrimaryCoedMatched)
#decrement the master working file of primary children by those already matched
WorkingFilePrimaryChildren <- WorkingFilePrimaryChildren[!(WorkingFilePrimaryChildren$ID %in% WorkingPrimaryMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools6Coed,Aged6AssignmentCoedNoDups,Aged6AssignmentCoed)

################################################################################################
#adjust probabilities and repeat for the 5-year-olds
################################################################################################
#easiest to create subset to work with
TimaruSchools5Coed <- subset(TimaruSchoolMeshblocks, select=c(School.Name,Age.5))
#remove colleges and assume 50% of students are boys
TimaruSchools5Coed <- subset(TimaruSchools5Coed, !(School.Name %in% c("Craighead Diocesan School", "Mountainview High School","Roncalli College", "Timaru Girls' High School",
                                                                      "Timaru Boys' High School")))
#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingPrimaryMatchedFile, AssignedAge==5)
Primary5AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools5Coed <- merge(Primary5AlreadyAssigned,TimaruSchools5Coed, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools5Coed) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools5Coed$AlreadyAssigned[is.na(TimaruSchools5Coed$AlreadyAssigned)] <- 0
TimaruSchools5Coed$Age.5 <- TimaruSchools5Coed[,3]-TimaruSchools5Coed[,2]
#get percents
TimaruSchools5Coed$Percent <- TimaruSchools5Coed$Age.5/sum(TimaruSchools5Coed$Age.5)

#Assign the 5-year-olds to a school based on the proportions in the school roll data
Aged5AssignmentCoed <- subset(WorkingFilePrimaryChildren, AssignedAge==5)

#remove the 5-year-olds from the Working File for primary-aged children
WorkingFilePrimaryChildren <- subset(WorkingFilePrimaryChildren, AssignedAge!=5)

#see if the household numbers are unique
Aged5AssignmentCoed$`Household Number`[duplicated(Aged5AssignmentCoed$`Household Number`)]

#4 duplicates so split duplicate and non-duplicates
Aged5AssignmentCoedDups <- Aged5AssignmentCoed[duplicated(Aged5AssignmentCoed[c(8)]) ,]
Aged5AssignmentCoedNoDups <- Aged5AssignmentCoed[!(Aged5AssignmentCoed$ID %in% Aged5AssignmentCoedDups$ID),]
set.seed(307181331)
Aged5AssignmentCoedNoDups$School <- sample(TimaruSchools5Coed$School.Name,
                                             size=nrow(Aged5AssignmentCoedNoDups), replace=TRUE, prob=TimaruSchools5Coed$Percent)

#pull in primary children from matched household numbers to assign to the same school
AdditionalPrimaryCoedMatched <- subset(WorkingFilePrimaryChildren, `Household Number` %in% Aged5AssignmentCoedNoDups$`Household Number`)
#append the duplicate to this
AdditionalPrimaryCoedMatched <- rbind(AdditionalPrimaryCoedMatched,Aged5AssignmentCoedDups)
#delete dups file
rm(Aged5AssignmentCoedDups)
#120 matches on household
#assign the 120 matches to the same school
AdditionalPrimaryCoedMatched$School <- Aged5AssignmentCoedNoDups$School[match(AdditionalPrimaryCoedMatched$`Household Number`,
                                                                              Aged5AssignmentCoedNoDups$`Household Number`)]
#need a combined file of all matched children, updated as we go
#construct a file of all who are matched
WorkingPrimaryMatchedFile <- rbind(WorkingPrimaryMatchedFile,Aged5AssignmentCoedNoDups,AdditionalPrimaryCoedMatched)
#decrement the master working file of primary children by those already matched
WorkingFilePrimaryChildren <- WorkingFilePrimaryChildren[!(WorkingFilePrimaryChildren$ID %in% WorkingPrimaryMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools5Coed,Primary5AlreadyAssigned,Aged5AssignmentCoed,Aged5AssignmentCoedNoDups)

################################################################################################
#adjust probabilities and repeat for the 7-year-olds
################################################################################################
#easiest to create subset to work with
TimaruSchools7Coed <- subset(TimaruSchoolMeshblocks, select=c(School.Name,Age.7))
#remove colleges and assume 50% of students are boys
TimaruSchools7Coed <- subset(TimaruSchools7Coed, !(School.Name %in% c("Craighead Diocesan School", "Mountainview High School","Roncalli College", "Timaru Girls' High School",
                                                                      "Timaru Boys' High School")))
#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingPrimaryMatchedFile, AssignedAge==7)
Primary7AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools7Coed <- merge(Primary7AlreadyAssigned,TimaruSchools7Coed, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools7Coed) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools7Coed$AlreadyAssigned[is.na(TimaruSchools7Coed$AlreadyAssigned)] <- 0
TimaruSchools7Coed$Age.7 <- TimaruSchools7Coed[,3]-TimaruSchools7Coed[,2]
#get percents
TimaruSchools7Coed$Percent <- TimaruSchools7Coed$Age.7/sum(TimaruSchools7Coed$Age.7)

#Assign the 7-year-olds to a school based on the proportions in the school roll data
Aged7AssignmentCoed <- subset(WorkingFilePrimaryChildren, AssignedAge==7)

#remove the 7-year-olds from the Working File for primary-aged children
WorkingFilePrimaryChildren <- subset(WorkingFilePrimaryChildren, AssignedAge!=7)

#see if the household numbers are unique
Aged7AssignmentCoed$`Household Number`[duplicated(Aged7AssignmentCoed$`Household Number`)]

#3 duplicates so split duplicate and non-duplicates
Aged7AssignmentCoedDups <- Aged7AssignmentCoed[duplicated(Aged7AssignmentCoed[c(8)]) ,]
Aged7AssignmentCoedNoDups <- Aged7AssignmentCoed[!(Aged7AssignmentCoed$ID %in% Aged7AssignmentCoedDups$ID),]
set.seed(307181358)
Aged7AssignmentCoedNoDups$School <- sample(TimaruSchools7Coed$School.Name,
                                           size=nrow(Aged7AssignmentCoedNoDups), replace=TRUE, prob=TimaruSchools7Coed$Percent)

#pull in primary children from matched household numbers to assign to the same school
AdditionalPrimaryCoedMatched <- subset(WorkingFilePrimaryChildren, `Household Number` %in% Aged7AssignmentCoedNoDups$`Household Number`)
#append the duplicate to this
AdditionalPrimaryCoedMatched <- rbind(AdditionalPrimaryCoedMatched,Aged7AssignmentCoedDups)
#delete dups file
rm(Aged7AssignmentCoedDups)
#93 matches on household
#assign the 93 matches to the same school
AdditionalPrimaryCoedMatched$School <- Aged7AssignmentCoedNoDups$School[match(AdditionalPrimaryCoedMatched$`Household Number`,
                                                                              Aged7AssignmentCoedNoDups$`Household Number`)]
#need a combined file of all matched children, updated as we go
#construct a file of all who are matched
WorkingPrimaryMatchedFile <- rbind(WorkingPrimaryMatchedFile,Aged7AssignmentCoedNoDups,AdditionalPrimaryCoedMatched)
#decrement the master working file of primary children by those already matched
WorkingFilePrimaryChildren <- WorkingFilePrimaryChildren[!(WorkingFilePrimaryChildren$ID %in% WorkingPrimaryMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools7Coed,Primary7AlreadyAssigned,Aged7AssignmentCoed,Aged7AssignmentCoedNoDups)

################################################################################################
#adjust probabilities and repeat for the 8-year-olds
################################################################################################
#easiest to create subset to work with
TimaruSchools8Coed <- subset(TimaruSchoolMeshblocks, select=c(School.Name,Age.8))
#remove colleges and assume 50% of students are boys
TimaruSchools8Coed <- subset(TimaruSchools8Coed, !(School.Name %in% c("Craighead Diocesan School", "Mountainview High School","Roncalli College", "Timaru Girls' High School",
                                                                      "Timaru Boys' High School")))
#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingPrimaryMatchedFile, AssignedAge==8)
Primary8AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools8Coed <- merge(Primary8AlreadyAssigned,TimaruSchools8Coed, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools8Coed) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools8Coed$AlreadyAssigned[is.na(TimaruSchools8Coed$AlreadyAssigned)] <- 0
TimaruSchools8Coed$Age.8 <- TimaruSchools8Coed[,3]-TimaruSchools8Coed[,2]
#get percents
TimaruSchools8Coed$Percent <- TimaruSchools8Coed$Age.8/sum(TimaruSchools8Coed$Age.8)

#Assign the 8-year-olds to a school based on the proportions in the school roll data
Aged8AssignmentCoed <- subset(WorkingFilePrimaryChildren, AssignedAge==8)

#remove the 8-year-olds from the Working File for primary-aged children
WorkingFilePrimaryChildren <- subset(WorkingFilePrimaryChildren, AssignedAge!=8)

#see if the household numbers are unique
Aged8AssignmentCoed$`Household Number`[duplicated(Aged8AssignmentCoed$`Household Number`)]

#4 duplicates so split duplicate and non-duplicates
Aged8AssignmentCoedDups <- Aged8AssignmentCoed[duplicated(Aged8AssignmentCoed[c(8)]) ,]
Aged8AssignmentCoedNoDups <- Aged8AssignmentCoed[!(Aged8AssignmentCoed$ID %in% Aged8AssignmentCoedDups$ID),]
set.seed(308181403)
Aged8AssignmentCoedNoDups$School <- sample(TimaruSchools8Coed$School.Name,
                                           size=nrow(Aged8AssignmentCoedNoDups), replace=TRUE, prob=TimaruSchools8Coed$Percent)

#pull in primary children from matched household numbers to assign to the same school
AdditionalPrimaryCoedMatched <- subset(WorkingFilePrimaryChildren, `Household Number` %in% Aged8AssignmentCoedNoDups$`Household Number`)
#append the duplicate to this
AdditionalPrimaryCoedMatched <- rbind(AdditionalPrimaryCoedMatched,Aged8AssignmentCoedDups)
#delete dups file
rm(Aged8AssignmentCoedDups)
#47 matches on household
#assign the 47 matches to the same school
AdditionalPrimaryCoedMatched$School <- Aged8AssignmentCoedNoDups$School[match(AdditionalPrimaryCoedMatched$`Household Number`,
                                                                              Aged8AssignmentCoedNoDups$`Household Number`)]
#need a combined file of all matched children, updated as we go
#construct a file of all who are matched
WorkingPrimaryMatchedFile <- rbind(WorkingPrimaryMatchedFile,Aged8AssignmentCoedNoDups,AdditionalPrimaryCoedMatched)
#decrement the master working file of primary children by those already matched
WorkingFilePrimaryChildren <- WorkingFilePrimaryChildren[!(WorkingFilePrimaryChildren$ID %in% WorkingPrimaryMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools8Coed,Primary8AlreadyAssigned,Aged8AssignmentCoed,Aged8AssignmentCoedNoDups)

################################################################################################
#adjust probabilities and repeat for the 9-year-olds
################################################################################################
#easiest to create subset to work with
TimaruSchools9Coed <- subset(TimaruSchoolMeshblocks, select=c(School.Name,Age.9))
#remove colleges and assume 50% of students are boys
TimaruSchools9Coed <- subset(TimaruSchools9Coed, !(School.Name %in% c("Craighead Diocesan School", "Mountainview High School","Roncalli College", "Timaru Girls' High School",
                                                                      "Timaru Boys' High School")))
#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingPrimaryMatchedFile, AssignedAge==9)
Primary9AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools9Coed <- merge(Primary9AlreadyAssigned,TimaruSchools9Coed, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools9Coed) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools9Coed$AlreadyAssigned[is.na(TimaruSchools9Coed$AlreadyAssigned)] <- 0
TimaruSchools9Coed$Age.9 <- TimaruSchools9Coed[,3]-TimaruSchools9Coed[,2]
#get percents
TimaruSchools9Coed$Percent <- TimaruSchools9Coed$Age.9/sum(TimaruSchools9Coed$Age.9)

#Assign the 9-year-olds to a school based on the proportions in the school roll data
Aged9AssignmentCoed <- subset(WorkingFilePrimaryChildren, AssignedAge==9)

#remove the 9-year-olds from the Working File for primary-aged children
WorkingFilePrimaryChildren <- subset(WorkingFilePrimaryChildren, AssignedAge!=9)

#see if the household numbers are unique
Aged9AssignmentCoed$`Household Number`[duplicated(Aged9AssignmentCoed$`Household Number`)]

#3 duplicates so split duplicate and non-duplicates
Aged9AssignmentCoedDups <- Aged9AssignmentCoed[duplicated(Aged9AssignmentCoed[c(8)]) ,]
Aged9AssignmentCoedNoDups <- Aged9AssignmentCoed[!(Aged9AssignmentCoed$ID %in% Aged9AssignmentCoedDups$ID),]

set.seed(108181143)
Aged9AssignmentCoedNoDups$School <- sample(TimaruSchools9Coed$School.Name,
                                           size=nrow(Aged9AssignmentCoedNoDups), replace=TRUE, prob=TimaruSchools9Coed$Percent)

#pull in primary children from matched household numbers to assign to the same school
AdditionalPrimaryCoedMatched <- subset(WorkingFilePrimaryChildren, `Household Number` %in% Aged9AssignmentCoedNoDups$`Household Number`)
#append the duplicate to this
AdditionalPrimaryCoedMatched <- rbind(AdditionalPrimaryCoedMatched,Aged9AssignmentCoedDups)
#delete dups file
rm(Aged9AssignmentCoedDups)
#23 matches on household
#assign the 23 matches to the same school
AdditionalPrimaryCoedMatched$School <- Aged9AssignmentCoedNoDups$School[match(AdditionalPrimaryCoedMatched$`Household Number`,
                                                                              Aged9AssignmentCoedNoDups$`Household Number`)]
#need a combined file of all matched children, updated as we go
#construct a file of all who are matched
WorkingPrimaryMatchedFile <- rbind(WorkingPrimaryMatchedFile,Aged9AssignmentCoedNoDups,AdditionalPrimaryCoedMatched)
#decrement the master working file of primary children by those already matched
WorkingFilePrimaryChildren <- WorkingFilePrimaryChildren[!(WorkingFilePrimaryChildren$ID %in% WorkingPrimaryMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools9Coed,Primary9AlreadyAssigned,Aged9AssignmentCoed,Aged9AssignmentCoedNoDups)

################################################################################################
#adjust probabilities and repeat for the 10-year-olds
################################################################################################
#easiest to create subset to work with
TimaruSchools10Coed <- subset(TimaruSchoolMeshblocks, select=c(School.Name,Age.10))
#remove colleges and assume 50% of students are boys
TimaruSchools10Coed <- subset(TimaruSchools10Coed, !(School.Name %in% c("Craighead Diocesan School", "Mountainview High School","Roncalli College", "Timaru Girls' High School",
                                                                      "Timaru Boys' High School")))
#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingPrimaryMatchedFile, AssignedAge==10)
Primary10AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools10Coed <- merge(Primary10AlreadyAssigned,TimaruSchools10Coed, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools10Coed) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools10Coed$AlreadyAssigned[is.na(TimaruSchools10Coed$AlreadyAssigned)] <- 0
TimaruSchools10Coed$Age.10 <- TimaruSchools10Coed[,3]-TimaruSchools10Coed[,2]
#get percents
TimaruSchools10Coed$Percent <- TimaruSchools10Coed$Age.10/sum(TimaruSchools10Coed$Age.10)

#Assign the 10-year-olds to a school based on the proportions in the school roll data
Aged10AssignmentCoed <- subset(WorkingFilePrimaryChildren, AssignedAge==10)

#remove the 10-year-olds from the Working File for primary-aged children
WorkingFilePrimaryChildren <- subset(WorkingFilePrimaryChildren, AssignedAge!=10)

#see if the household numbers are unique
Aged10AssignmentCoed$`Household Number`[duplicated(Aged10AssignmentCoed$`Household Number`)]

#4 duplicates so split duplicate and non-duplicates
Aged10AssignmentCoedDups <- Aged10AssignmentCoed[duplicated(Aged10AssignmentCoed[c(8)]) ,]
Aged10AssignmentCoedNoDups <- Aged10AssignmentCoed[!(Aged10AssignmentCoed$ID %in% Aged10AssignmentCoedDups$ID),]

set.seed(108181143)
Aged10AssignmentCoedNoDups$School <- sample(TimaruSchools10Coed$School.Name,
                                           size=nrow(Aged10AssignmentCoedNoDups), replace=TRUE, prob=TimaruSchools10Coed$Percent)

#pull in primary children from matched household numbers to assign to the same school
AdditionalPrimaryCoedMatched <- subset(WorkingFilePrimaryChildren, `Household Number` %in% Aged10AssignmentCoedNoDups$`Household Number`)
#append the duplicate to this
AdditionalPrimaryCoedMatched <- rbind(AdditionalPrimaryCoedMatched,Aged10AssignmentCoedDups)
#delete dups file
rm(Aged10AssignmentCoedDups)
#23 matches on household
#assign the 23 matches to the same school
AdditionalPrimaryCoedMatched$School <- Aged10AssignmentCoedNoDups$School[match(AdditionalPrimaryCoedMatched$`Household Number`,
                                                                              Aged10AssignmentCoedNoDups$`Household Number`)]
#need a combined file of all matched children, updated as we go
#construct a file of all who are matched
WorkingPrimaryMatchedFile <- rbind(WorkingPrimaryMatchedFile,Aged10AssignmentCoedNoDups,AdditionalPrimaryCoedMatched)
#decrement the master working file of primary children by those already matched
WorkingFilePrimaryChildren <- WorkingFilePrimaryChildren[!(WorkingFilePrimaryChildren$ID %in% WorkingPrimaryMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools10Coed,Primary10AlreadyAssigned,Aged10AssignmentCoed,Aged10AssignmentCoedNoDups)

################################################################################################
################################################################################################
#at this point we need to merge the primary and college files so Craighead is assigned appropriately
################################################################################################
################################################################################################
AllMatchedChildren <- rbind(WorkingMatchedFile,WorkingPrimaryMatchedFile)


################################################################################################
#adjust probabilities and repeat for the 11-year-olds
################################################################################################
################################################################################################
#need to split out the 11-year-olds by sex
################################################################################################
################################################################################################
#11-year-old girls
################################################################################################
################################################################################################
#make sure that no 11-year-old girls who have older sisters at college are put into Craigshead
#except for 12-year-old sisters who will also need to go to Craigshead
################################################################################################
################################################################################################
#easiest to create subset to work with
TimaruSchools11Coed <- subset(TimaruSchoolMeshblocks, select=c(School.Name,Age.11))
#remove all colleges except Craighead, and assume 50% of students are boys except for Craighead
TimaruSchools11Coed <- subset(TimaruSchools11Coed, !(School.Name %in% c("Mountainview High School","Roncalli College", "Timaru Girls' High School",
                                                                        "Timaru Boys' High School")))
#remove boy counts from coed school data, assume 50% of students are boys
TimaruSchools11Coed$Age.11 <- with(TimaruSchools11Coed, ifelse(School.Name!="Craighead Diocesan School", Age.11/2, Age.11))

#decrease counts by those already assigned, use master matched file
temp <- subset(AllMatchedChildren, AssignedAge==11)
Primary11AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools11Coed <- merge(Primary11AlreadyAssigned,TimaruSchools11Coed, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools11Coed) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools11Coed$AlreadyAssigned[is.na(TimaruSchools11Coed$AlreadyAssigned)] <- 0
TimaruSchools11Coed$Age.11 <- TimaruSchools11Coed[,3]-TimaruSchools11Coed[,2]
#get percents
TimaruSchools11Coed$Percent <- TimaruSchools11Coed$Age.11/sum(TimaruSchools11Coed$Age.11)

#Assign the 11-year-old girls to a school based on the proportions in the school roll data
Aged11AssignmentGirls <- subset(WorkingFilePrimaryChildren, AssignedAge==11 & SEX=="Female")

#remove the 11-year-old girls from the Working File for primary-aged children
WorkingFilePrimaryChildren <- subset(WorkingFilePrimaryChildren, !(AssignedAge==11 & SEX=="Female"))

#see if any 11-year-old girls have an older college aged sister
#because Craighead has already been done, no girl with an older college aged sister
#can be assigned to Craighead
Girls11YearsOldCollegeSister <- Aged11AssignmentGirls[Aged11AssignmentGirls$`Household Number` %in% WorkingMatchedFile$`Household Number`,]
#there are 21 11-year-old girls who can't go to Craighead out of the 94 unassigned 11-year-old girls
#split these girls out of the main to-be-assigned file
Aged11AssignmentGirls <- Aged11AssignmentGirls[!(Aged11AssignmentGirls$ID %in% Girls11YearsOldCollegeSister$ID),]

#assign the 11-year-old girls with the older college-aged sister a primary school (which won't be Craighead)
#therefore must redo the TimaruSchools11Coed to exclude Craighead and redo the percentages
TimaruSchools11CoedNoCraighead <- subset(TimaruSchools11Coed,School.Name!="Craighead Diocesan School")
#and redo percents
TimaruSchools11CoedNoCraighead$Percent <- TimaruSchools11CoedNoCraighead$Age.11/sum(TimaruSchools11CoedNoCraighead$Age.11)
#assign the girls with the college aged sisters to a primary school
set.seed(608181335)
Girls11YearsOldCollegeSister$School <- sample(TimaruSchools11CoedNoCraighead$School.Name,
                                            size=nrow(Girls11YearsOldCollegeSister), replace=TRUE, prob=TimaruSchools11CoedNoCraighead$Percent)

#see if the household numbers are unique for remaining girls (no college-aged sister)
Aged11AssignmentGirls$`Household Number`[duplicated(Aged11AssignmentGirls$`Household Number`)]
#no duplicates

#update primary school assignment probabilities based on above allocations
temp <- subset(Girls11YearsOldCollegeSister)
Primary11AlreadyAssigned <- as.data.frame(table(temp$School))
colnames(Primary11AlreadyAssigned)[colnames(Primary11AlreadyAssigned)=="Freq"] <- "MoreToDeduct"
TimaruSchools11Coed <- merge(Primary11AlreadyAssigned,TimaruSchools11Coed, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools11Coed)[colnames(TimaruSchools11Coed)=="Age.11"] <- "FirstDeduction"
colnames(TimaruSchools11Coed)[colnames(TimaruSchools11Coed)=="Var1"] <- "School.Name"
TimaruSchools11Coed$Age.11 <- TimaruSchools11Coed[,5]-TimaruSchools11Coed[,2]
#replace any NA counts with 0
TimaruSchools11Coed$AlreadyAssigned[is.na(TimaruSchools11Coed$AlreadyAssigned)] <- 0
#replace any negatives with a 0 count
TimaruSchools11Coed$Age.11 <- ifelse(TimaruSchools11Coed$Age.11 <0, 0, TimaruSchools11Coed$Age.11)
#update percents
TimaruSchools11Coed$Percent <- TimaruSchools11Coed$Age.11/sum(TimaruSchools11Coed$Age.11)

#assign the girls without college-aged sisters
set.seed(608181409)
Aged11AssignmentGirls$School <- sample(TimaruSchools11Coed$School.Name,
                                            size=nrow(Aged11AssignmentGirls), replace=TRUE, prob=TimaruSchools11Coed$Percent)


#pull in primary children from matched household numbers to assign to the same school
#but remember that only girls can be put into Craighead
#easiest thing to do is to match both sexes and reset the boys to no assigned school
#if sister is at Craighead
#merge both sets of matched 11-year-old girls
Aged11AssignmentGirls <- rbind(Girls11YearsOldCollegeSister,Aged11AssignmentGirls)
#locate boys and 12-year-olds matches
AdditionalPrimaryCoedMatched <- subset(WorkingFilePrimaryChildren, `Household Number` %in% Aged11AssignmentGirls$`Household Number`)

#14 matches on household
#assign the 14 matches to the same school
AdditionalPrimaryCoedMatched$School <- Aged11AssignmentGirls$School[match(AdditionalPrimaryCoedMatched$`Household Number`,
                                                                          Aged11AssignmentGirls$`Household Number`)]

#remove Craighead from any boy matched to that school
AdditionalPrimaryCoedMatched$School <- ifelse((AdditionalPrimaryCoedMatched$SEX=="Male" & AdditionalPrimaryCoedMatched$School=="Craighead Diocesan School"), 
                                               "",as.character(AdditionalPrimaryCoedMatched$School))
#Remove the unmatched boys from the primary school
AdditionalPrimaryCoedMatched <- subset(AdditionalPrimaryCoedMatched, AdditionalPrimaryCoedMatched$School!="")


#need a combined file of all matched children, updated as we go
#construct a file of all who are matched
WorkingPrimaryMatchedFile <- rbind(WorkingPrimaryMatchedFile,Aged11AssignmentGirls,AdditionalPrimaryCoedMatched)
#decrement the master working file of primary children by those already matched
WorkingFilePrimaryChildren <- WorkingFilePrimaryChildren[!(WorkingFilePrimaryChildren$ID %in% WorkingPrimaryMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools11Coed,TimaruSchools11CoedNoCraighead,Primary11AlreadyAssigned,Aged11AssignmentGirls,Girls11YearsOldCollegeSister)

################################################################################################
################################################################################################
#11-year-old boys
################################################################################################
################################################################################################
#only use the coed primary schools
################################################################################################
################################################################################################
TimaruSchools11Coed <- subset(TimaruSchoolMeshblocks, select=c(School.Name,Age.11))
#remove all colleges and assume 50% of students are boys
TimaruSchools11Coed <- subset(TimaruSchools11Coed, !(School.Name %in% c("Craighead Diocesan School","Mountainview High School","Roncalli College", 
                                                                        "Timaru Girls' High School","Timaru Boys' High School")))
#remove girl counts from coed school data, assume 50% of students are boys
TimaruSchools11Coed$Age.11 <- with(TimaruSchools11Coed, Age.11/2)

#decrease counts by those already assigned, use master matched file
temp <- subset(AllMatchedChildren, AssignedAge==11)
Primary11AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools11Coed <- merge(Primary11AlreadyAssigned,TimaruSchools11Coed, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools11Coed) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools11Coed$AlreadyAssigned[is.na(TimaruSchools11Coed$AlreadyAssigned)] <- 0
TimaruSchools11Coed$Age.11 <- TimaruSchools11Coed[,3]-TimaruSchools11Coed[,2]
#get percents
TimaruSchools11Coed$Percent <- TimaruSchools11Coed$Age.11/sum(TimaruSchools11Coed$Age.11)

#Assign the 11-year-old boys to a school based on the proportions in the school roll data
Aged11AssignmentBoys <- subset(WorkingFilePrimaryChildren, AssignedAge==11 & SEX=="Male")
#remove the 11-year-old boys from the Working File for primary-aged children
WorkingFilePrimaryChildren <- subset(WorkingFilePrimaryChildren, !(AssignedAge==11 & SEX=="Male"))

#see if the household numbers are unique
Aged11AssignmentBoys$`Household Number`[duplicated(Aged11AssignmentBoys$`Household Number`)]

#3 duplicates so split duplicate and non-duplicates
Aged11AssignmentBoysDups <- Aged11AssignmentBoys[duplicated(Aged11AssignmentBoys[c(8)]) ,]
Aged11AssignmentBoysNoDups <- Aged11AssignmentBoys[!(Aged11AssignmentBoys$ID %in% Aged11AssignmentBoysDups$ID),]

set.seed(70818641)
Aged11AssignmentBoysNoDups$School <- sample(TimaruSchools11Coed$School.Name,
                                            size=nrow(Aged11AssignmentBoysNoDups), replace=TRUE, prob=TimaruSchools11Coed$Percent)

#pull in primary children from matched household numbers to assign to the same school
AdditionalPrimaryCoedMatched <- subset(WorkingFilePrimaryChildren, `Household Number` %in% TimaruSchools11Coed$`Household Number`)
#0 matches so only need the dups file
#append the duplicate to this
AdditionalPrimaryCoedMatched <- Aged11AssignmentBoysDups
#delete dups file
rm(Aged11AssignmentBoysDups)
#assign the 3 duplicates to the same school
AdditionalPrimaryCoedMatched$School <- Aged11AssignmentBoysNoDups$School[match(AdditionalPrimaryCoedMatched$`Household Number`,
                                                                               Aged11AssignmentBoysNoDups$`Household Number`)]
#need a combined file of all matched children, updated as we go
#construct a file of all who are matched
WorkingPrimaryMatchedFile <- rbind(WorkingPrimaryMatchedFile,Aged11AssignmentBoysNoDups,AdditionalPrimaryCoedMatched)
#decrement the master working file of primary children by those already matched
WorkingFilePrimaryChildren <- WorkingFilePrimaryChildren[!(WorkingFilePrimaryChildren$ID %in% WorkingPrimaryMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools11Coed,Primary11AlreadyAssigned,Aged11AssignmentBoysNoDups,Aged11AssignmentBoys)

################################################################################################
#need to split out the 12-year-olds by sex
################################################################################################
################################################################################################
#make sure that no 12-year-old girls who have older sisters at college are put into Craigshead
################################################################################################
#easiest to create subset to work with
TimaruSchools12Coed <- subset(TimaruSchoolMeshblocks, select=c(School.Name,Age.12))
#remove all colleges except Craighead, and assume 50% of students are boys except for Craighead
TimaruSchools12Coed <- subset(TimaruSchools12Coed, !(School.Name %in% c("Mountainview High School","Roncalli College", "Timaru Girls' High School",
                                                                        "Timaru Boys' High School")))
#remove boy counts from coed school data, assume 50% of students are boys
TimaruSchools12Coed$Age.12 <- with(TimaruSchools12Coed, ifelse(School.Name!="Craighead Diocesan School", Age.12/2, Age.12))

#decrease counts by those already assigned, use master matched file
temp <- subset(AllMatchedChildren, AssignedAge==12)
Primary12AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools12Coed <- merge(Primary12AlreadyAssigned,TimaruSchools12Coed, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools12Coed) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools12Coed$AlreadyAssigned[is.na(TimaruSchools12Coed$AlreadyAssigned)] <- 0
TimaruSchools12Coed$Age.12 <- TimaruSchools12Coed[,3]-TimaruSchools12Coed[,2]
#get percents
TimaruSchools12Coed$Percent <- TimaruSchools12Coed$Age.12/sum(TimaruSchools12Coed$Age.12)

#Assign the 12-year-old girls to a school based on the proportions in the school roll data
Aged12AssignmentGirls <- subset(WorkingFilePrimaryChildren, AssignedAge==12 & SEX=="Female")

#remove the 12-year-old girls from the Working File for primary-aged children
WorkingFilePrimaryChildren <- subset(WorkingFilePrimaryChildren, !(AssignedAge==12 & SEX=="Female"))

#see if any 12-year-old girls have an older college aged sister
#because Craighead has already been done, no girl with an older college aged sister
#can be assigned to Craighead
Girls12YearsOldCollegeSister <- Aged12AssignmentGirls[Aged12AssignmentGirls$`Household Number` %in% WorkingMatchedFile$`Household Number`,]
#there are 16 12-year-old girls who can't go to Craighead out of the 88 unassigned 12-year-old girls
#split these girls out of the main to-be-assigned file
Aged12AssignmentGirls <- Aged12AssignmentGirls[!(Aged12AssignmentGirls$ID %in% Girls12YearsOldCollegeSister$ID),]

#assign the 12-year-old girls with the older college-aged sister a primary school (which won't be Craighead)
#therefore must redo the TimaruSchools12Coed to exclude Craighead and redo the percentages
TimaruSchools12CoedNoCraighead <- subset(TimaruSchools12Coed,School.Name!="Craighead Diocesan School")
#and redo percents
TimaruSchools12CoedNoCraighead$Percent <- TimaruSchools12CoedNoCraighead$Age.12/sum(TimaruSchools12CoedNoCraighead$Age.12)
#assign the girls with the college aged sisters to a primary school
set.seed(708181147)
Girls12YearsOldCollegeSister$School <- sample(TimaruSchools12CoedNoCraighead$School.Name,
                                              size=nrow(Girls12YearsOldCollegeSister), replace=TRUE, prob=TimaruSchools12CoedNoCraighead$Percent)

#assign the girls without college-aged sisters
#see if the household numbers are unique for remaining girls (no college-aged sister)
Aged12AssignmentGirls$`Household Number`[duplicated(Aged12AssignmentGirls$`Household Number`)]
#1 duplicate
#split the duplicate away from the non-duplicates
#3 duplicates so split duplicate and non-duplicates
Aged12AssignmentGirlsDups <- Aged12AssignmentGirls[duplicated(Aged12AssignmentGirls[c(8)]) ,]
Aged12AssignmentGirlsNoDups <- Aged12AssignmentGirls[!(Aged12AssignmentGirls$ID %in% Aged12AssignmentGirlsDups$ID),]

#update primary school assignment probabilities based on above allocations
temp <- subset(Girls12YearsOldCollegeSister)
Primary12AlreadyAssigned <- as.data.frame(table(temp$School))
colnames(Primary12AlreadyAssigned)[colnames(Primary12AlreadyAssigned)=="Freq"] <- "MoreToDeduct"
TimaruSchools12Coed <- merge(Primary12AlreadyAssigned,TimaruSchools12Coed, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools12Coed)[colnames(TimaruSchools12Coed)=="Age.12"] <- "FirstDeduction"
colnames(TimaruSchools12Coed)[colnames(TimaruSchools12Coed)=="Var1"] <- "School.Name"
TimaruSchools12Coed$Age.12 <- TimaruSchools12Coed[,5]-TimaruSchools12Coed[,2]
#replace any NA counts with 0
TimaruSchools12Coed$AlreadyAssigned[is.na(TimaruSchools12Coed$AlreadyAssigned)] <- 0
#replace any negatives with a 0 count
TimaruSchools12Coed$Age.12 <- ifelse(TimaruSchools12Coed$Age.12 <0, 0, TimaruSchools12Coed$Age.12)
#update percents
TimaruSchools12Coed$Percent <- TimaruSchools12Coed$Age.12/sum(TimaruSchools12Coed$Age.12)

set.seed(708181158)
Aged12AssignmentGirlsNoDups$School <- sample(TimaruSchools12Coed$School.Name,
                                            size=nrow(Aged12AssignmentGirlsNoDups), replace=TRUE, prob=TimaruSchools12Coed$Percent)


#pull in primary children from matched household numbers to assign to the same school
AdditionalPrimaryCoedMatched <- subset(WorkingFilePrimaryChildren, `Household Number` %in% Aged12AssignmentGirlsNoDups$`Household Number`)
#2 matches 
#append the duplicate to this
AdditionalPrimaryCoedMatched <- rbind(Aged12AssignmentGirlsDups,AdditionalPrimaryCoedMatched)
#delete dups file
rm(Aged12AssignmentGirlsDups)
#assign the 2 duplicates to the same school
AdditionalPrimaryCoedMatched$School <- Aged12AssignmentGirlsNoDups$School[match(AdditionalPrimaryCoedMatched$`Household Number`,
                                                                                Aged12AssignmentGirlsNoDups$`Household Number`)]

#need a combined file of all matched children, updated as we go
#construct a file of all who are matched
WorkingPrimaryMatchedFile <- rbind(WorkingPrimaryMatchedFile,Girls12YearsOldCollegeSister,Aged12AssignmentGirlsNoDups,AdditionalPrimaryCoedMatched)
#decrement the master working file of primary children by those already matched
WorkingFilePrimaryChildren <- WorkingFilePrimaryChildren[!(WorkingFilePrimaryChildren$ID %in% WorkingPrimaryMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools12Coed,TimaruSchools12CoedNoCraighead, Primary12AlreadyAssigned,Aged12AssignmentGirls,Aged12AssignmentGirlsNoDups,
   Girls12YearsOldCollegeSister)

################################################################################################
################################################################################################
#12-year-old boys
################################################################################################
################################################################################################
#only use the coed primary schools
################################################################################################
################################################################################################
TimaruSchools12Coed <- subset(TimaruSchoolMeshblocks, select=c(School.Name,Age.12))
#remove all colleges and assume 50% of students are boys
TimaruSchools12Coed <- subset(TimaruSchools12Coed, !(School.Name %in% c("Craighead Diocesan School","Mountainview High School","Roncalli College", 
                                                                        "Timaru Girls' High School","Timaru Boys' High School")))
#remove girl counts from coed school data, assume 50% of students are boys
TimaruSchools12Coed$Age.12 <- with(TimaruSchools12Coed, Age.12/2)

#decrease counts by those already assigned, use master matched file
temp <- subset(AllMatchedChildren, AssignedAge==12)
Primary12AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools12Coed <- merge(Primary12AlreadyAssigned,TimaruSchools12Coed, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools12Coed) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools12Coed$AlreadyAssigned[is.na(TimaruSchools12Coed$AlreadyAssigned)] <- 0
TimaruSchools12Coed$Age.12 <- TimaruSchools12Coed[,3]-TimaruSchools12Coed[,2]
#get percents
TimaruSchools12Coed$Percent <- TimaruSchools12Coed$Age.12/sum(TimaruSchools12Coed$Age.12)

#Assign the 12-year-old boys to a school based on the proportions in the school roll data
Aged12AssignmentBoys <- subset(WorkingFilePrimaryChildren, AssignedAge==12 & SEX=="Male")
#remove the 12-year-old boys from the Working File for primary-aged children
WorkingFilePrimaryChildren <- subset(WorkingFilePrimaryChildren, !(AssignedAge==12 & SEX=="Male"))
#as expected, the working file now contains 0 children

#see if the household numbers are unique
Aged12AssignmentBoys$`Household Number`[duplicated(Aged12AssignmentBoys$`Household Number`)]
#0 duplicates

set.seed(808181329)
Aged12AssignmentBoys$School <- sample(TimaruSchools12Coed$School.Name,
                                            size=nrow(Aged12AssignmentBoys), replace=TRUE, prob=TimaruSchools12Coed$Percent)

#need a combined file of all matched children, updated as we go
#construct a file of all who are matched
WorkingPrimaryMatchedFile <- rbind(WorkingPrimaryMatchedFile,Aged12AssignmentBoys)

#clean up interim files
rm(TimaruSchools12Coed,Primary12AlreadyAssigned,Aged12AssignmentBoys)

################################################################################################
################################################################################################
# for boys, if any girl in the family is in a school, the boy is as well
# in addition, for any girl in a girls college, college-aged boys are in the equivalent college
# so Timaru Boys' High goes with Timaru Girls' High
################################################################################################
################################################################################################
# easiest first thing is to allocate the college boys with a Timaru Girls' High sister to Timaru Boys's High
#extract the college-aged boys, all ages
CollegeAgedBoys <- subset(Master_file_children_with_ages, AssignedAge>12 & SEX=="Male")
#extract out boys who have a sister already assigned to a college
CollegeBoysWithCollegeSisters <- CollegeAgedBoys[CollegeAgedBoys$`Household Number` %in% WorkingMatchedFile$`Household Number`,]
# there are 133 of them
# decrease working college boys file by these
CollegeAgedBoys <- CollegeAgedBoys[!(CollegeAgedBoys$ID %in% CollegeBoysWithCollegeSisters$ID), ]
# assign the college boys to the same school
# because the girls' data was already controlled for household, we don't need to do that here
CollegeBoysWithCollegeSisters$School <- WorkingMatchedFile$School[match(CollegeBoysWithCollegeSisters$`Household Number`,
                                                                        WorkingMatchedFile$`Household Number`)]

table(CollegeBoysWithCollegeSisters$School)

#replace any Timaru Girls's School with Timaru Boys' School (52 boys)
CollegeBoysWithCollegeSisters$School <- ifelse((CollegeBoysWithCollegeSisters$School=="Timaru Girls' High School"), 
                                               "Timaru Boys' High School",as.character(CollegeBoysWithCollegeSisters$School))

# remove school assigned if it is Craighead (25 boys)
CollegeBoysWithCollegeSisters$School <- ifelse((CollegeBoysWithCollegeSisters$School=="Craighead Diocesan School"), 
                                              "",as.character(CollegeBoysWithCollegeSisters$School))

# assume the boys that were assigned to Craighead went to a boys' school other than Craighead
#look at ages
table(CollegeBoysWithCollegeSisters$AssignedAge, CollegeBoysWithCollegeSisters$School)
# they are spread 13 through 17 years old
# they can go to either Roncalli or Mountainview
# will be a PITA but need to do the assignment on probability by age
# but only need to consider those two colleges, not the others as well
# get the Roncalli and Mountainview counts by age 13 through 17
# will need to decrement both by boys already assigned, from above, as well as the college girls
# easiest to do this decrementing if both college girls and college boys are in the same working matched file
# will need to ensure that any duplicate boys are assigned to the same college
# extract unassigned boys with college sisters
UnassignedBoysWithCollegeSisters <- subset(CollegeBoysWithCollegeSisters, School=="")
# decrement the college boys with college sisters file and append to working matched file
CollegeBoysWithCollegeSisters <- CollegeBoysWithCollegeSisters[!(CollegeBoysWithCollegeSisters$ID %in% UnassignedBoysWithCollegeSisters$ID), ]
WorkingMatchedFile <- rbind(WorkingMatchedFile,CollegeBoysWithCollegeSisters)
rm(CollegeBoysWithCollegeSisters)

#get counts for Roncalli and Mountainview
# THIS IS EXTRACTING BY ROW INDEX SO MAKE SURE THAT THE ROW INDEXES ARE CORRECT **********************************
MountainviewRoncalli <- TimaruSchoolMeshblocks[c(2,3),c(2,16:20)]
# extract counts for assigned students for all colleges, by age, will remove unneeded colleges later
CollegesAlreadyAssigned <- as.data.frame(table(WorkingMatchedFile[,c("AssignedAge", "School")]))
#remove unwanted schools and unwanted ages, have to do this in two steps as R is hating !(%in%) when added as second condition below
CollegesAlreadyAssigned <- subset(CollegesAlreadyAssigned, School %in% c("Mountainview High School", "Roncalli College") & !(AssignedAge %in% c("11", "12")))
#convert age (which is currently factor) into number
CollegesAlreadyAssigned$AssignedAge <- as.numeric(levels(CollegesAlreadyAssigned$AssignedAge))[CollegesAlreadyAssigned$AssignedAge]

#check if any duplicates
UnassignedBoysWithCollegeSisters$`Household Number`[duplicated(UnassignedBoysWithCollegeSisters$`Household Number`)]
#1 duplicate
#both are 16-year-olds, wait until that age to fix

#work on this by age
#create subset of college probabilities for 13-year-olds
CollegesBoys13Probs <- subset(CollegesAlreadyAssigned,AssignedAge==13)
#get percents
CollegesBoys13Probs$Percent <- CollegesBoys13Probs$Freq/sum(CollegesBoys13Probs$Freq)
UnassignedBoys13 <- subset(UnassignedBoysWithCollegeSisters, AssignedAge=="13")
#4 obs
set.seed(128181154)
UnassignedBoys13$School <- sample(CollegesBoys13Probs$School,
                                 size=nrow(UnassignedBoys13), replace=TRUE, prob=CollegesBoys13Probs$Percent)
WorkingMatchedFile <- rbind(WorkingMatchedFile,UnassignedBoys13)
rm(CollegesBoys13Probs,UnassignedBoys13)

#create subset of college probabilities for 14-year-olds
CollegesBoys14Probs <- subset(CollegesAlreadyAssigned,AssignedAge==14)
#get percents
CollegesBoys14Probs$Percent <- CollegesBoys14Probs$Freq/sum(CollegesBoys14Probs$Freq)
UnassignedBoys14 <- subset(UnassignedBoysWithCollegeSisters, AssignedAge=="14")
#5 obs
set.seed(128181155)
UnassignedBoys14$School <- sample(CollegesBoys14Probs$School,
                                  size=nrow(UnassignedBoys14), replace=TRUE, prob=CollegesBoys14Probs$Percent)
WorkingMatchedFile <- rbind(WorkingMatchedFile,UnassignedBoys14)
rm(CollegesBoys14Probs,UnassignedBoys14)

#create subset of college probabilities for 15-year-olds
CollegesBoys15Probs <- subset(CollegesAlreadyAssigned,AssignedAge==15)
#get percents
CollegesBoys15Probs$Percent <- CollegesBoys15Probs$Freq/sum(CollegesBoys15Probs$Freq)
UnassignedBoys15 <- subset(UnassignedBoysWithCollegeSisters, AssignedAge=="15")
#5 obs
set.seed(128181156)
UnassignedBoys15$School <- sample(CollegesBoys15Probs$School,
                                  size=nrow(UnassignedBoys15), replace=TRUE, prob=CollegesBoys15Probs$Percent)
WorkingMatchedFile <- rbind(WorkingMatchedFile,UnassignedBoys15)
rm(CollegesBoys15Probs,UnassignedBoys15)

#create subset of college probabilities for 16-year-olds
CollegesBoys16Probs <- subset(CollegesAlreadyAssigned,AssignedAge==16)
#get percents
CollegesBoys16Probs$Percent <- CollegesBoys16Probs$Freq/sum(CollegesBoys16Probs$Freq)
UnassignedBoys16 <- subset(UnassignedBoysWithCollegeSisters, AssignedAge=="16")
#7 obs
#split out duplicate
UnassignedBoys16Dups <- UnassignedBoys16[duplicated(UnassignedBoys16[c(8)]) ,]
UnassignedBoys16NoDups <- UnassignedBoys16[!(UnassignedBoys16$ID %in% UnassignedBoys16Dups$ID),]
set.seed(128181159)
UnassignedBoys16NoDups$School <- sample(CollegesBoys16Probs$School,
                                  size=nrow(UnassignedBoys16NoDups), replace=TRUE, prob=CollegesBoys16Probs$Percent)
#match duplicate's school
UnassignedBoys16Dups$School <- UnassignedBoys16NoDups$School[match(UnassignedBoys16Dups$`Household Number`,
                                                                   UnassignedBoys16NoDups$`Household Number`)]
WorkingMatchedFile <- rbind(WorkingMatchedFile,UnassignedBoys16NoDups,UnassignedBoys16Dups)
rm(CollegesBoys16Probs,UnassignedBoys16,UnassignedBoys16NoDups,UnassignedBoys16Dups)

#create subset of college probabilities for 17-year-olds
CollegesBoys17Probs <- subset(CollegesAlreadyAssigned,AssignedAge==17)
#get percents
CollegesBoys17Probs$Percent <- CollegesBoys17Probs$Freq/sum(CollegesBoys17Probs$Freq)
UnassignedBoys17 <- subset(UnassignedBoysWithCollegeSisters, AssignedAge=="17")
#4 obs
set.seed(128181204)
UnassignedBoys17$School <- sample(CollegesBoys17Probs$School,
                                  size=nrow(UnassignedBoys17), replace=TRUE, prob=CollegesBoys17Probs$Percent)
WorkingMatchedFile <- rbind(WorkingMatchedFile,UnassignedBoys17)
rm(CollegesBoys17Probs,UnassignedBoys17,UnassignedBoysWithCollegeSisters)

################################################################################################
################################################################################################
# now work on boys with no siblings at college
################################################################################################
################################################################################################
################################################################################################
# girls already matched so take school role counts as they are, no division by 2
################################################################################################
################################################################################################
################################################################################################
#13-year-old boys
################################################################################################
################################################################################################
#easiest to credit subset to work with
TimaruSchools13Boys <- subset(TimaruSchoolMeshblocks, School.Name %in% c("Roncalli College", "Mountainview High School","Timaru Boys' High School"),
                              select=c(School.Name,Age.13))


#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingMatchedFile, AssignedAge==13)
Children13AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools13Boys <- merge(Children13AlreadyAssigned,TimaruSchools13Boys, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools13Boys) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools13Boys$AlreadyAssigned[is.na(TimaruSchools13Boys$AlreadyAssigned)] <- 0
TimaruSchools13Boys$Age.13 <- TimaruSchools13Boys[,3]-TimaruSchools13Boys[,2]
#get percents
TimaruSchools13Boys$Percent <- TimaruSchools13Boys$Age.13/sum(TimaruSchools13Boys$Age.13)

#Assign the 13-year-old boys to a school based on the proportions in the school roll data
Aged13AssignmentBoys <- subset(CollegeAgedBoys, AssignedAge==13)

#remove the 13-year old boys from the Working File for college-aged boys
CollegeAgedBoys <- subset(CollegeAgedBoys, AssignedAge!=13)

#see if the household numbers are unique
Aged13AssignmentBoys$`Household Number`[duplicated(Aged13AssignmentBoys$`Household Number`)]
# 4 duplicates
#split out duplicates
Aged13AssignmentBoysDups <- Aged13AssignmentBoys[duplicated(Aged13AssignmentBoys[c(8)]) ,]
Aged13AssignmentBoysNoDups <- Aged13AssignmentBoys[!(Aged13AssignmentBoys$ID %in% Aged13AssignmentBoysDups$ID),]

set.seed(128181222)
Aged13AssignmentBoysNoDups$School <- sample(TimaruSchools13Boys$School,
                                        size=nrow(Aged13AssignmentBoysNoDups), replace=TRUE, prob=TimaruSchools13Boys$Percent)

#pull in college boys from matched household numbers to assign to the same school
AdditionalCollegeBoysMatched <- subset(CollegeAgedBoys, `Household Number` %in% Aged13AssignmentBoysNoDups$`Household Number`)
#34 matches on household
#append the duplicates
AdditionalCollegeBoysMatched <- rbind(AdditionalCollegeBoysMatched,Aged13AssignmentBoysDups)
#delete dups file
rm(Aged13AssignmentBoysDups)
#match duplicate's school
AdditionalCollegeBoysMatched$School <- Aged13AssignmentBoysNoDups$School[match(AdditionalCollegeBoysMatched$`Household Number`,
                                                                               Aged13AssignmentBoysNoDups$`Household Number`)]

#need a combined file of all matched college children, updated as we go
#construct a file of all who are matched
WorkingMatchedFile <- rbind(WorkingMatchedFile,Aged13AssignmentBoysNoDups,AdditionalCollegeBoysMatched)
#decrement the master working file of college girls by those already matched
CollegeAgedBoys <- CollegeAgedBoys[!(CollegeAgedBoys$ID %in% WorkingMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools13Boys,Children13AlreadyAssigned,Aged13AssignmentBoys,Aged13AssignmentBoysNoDups)

################################################################################################
################################################################################################
#14-year-old boys
################################################################################################
################################################################################################
#easiest to credit subset to work with
TimaruSchools14Boys <- subset(TimaruSchoolMeshblocks, School.Name %in% c("Roncalli College", "Mountainview High School","Timaru Boys' High School"),
                              select=c(School.Name,Age.14))

#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingMatchedFile, AssignedAge==14)
Children14AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools14Boys <- merge(Children14AlreadyAssigned,TimaruSchools14Boys, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools14Boys) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools14Boys$AlreadyAssigned[is.na(TimaruSchools14Boys$AlreadyAssigned)] <- 0
TimaruSchools14Boys$Age.14 <- TimaruSchools14Boys[,3]-TimaruSchools14Boys[,2]
#get percents
TimaruSchools14Boys$Percent <- TimaruSchools14Boys$Age.14/sum(TimaruSchools14Boys$Age.14)

#Assign the 14-year-old boys to a school based on the proportions in the school roll data
Aged14AssignmentBoys <- subset(CollegeAgedBoys, AssignedAge==14)

#remove the 14-year old boys from the Working File for college-aged boys
CollegeAgedBoys <- subset(CollegeAgedBoys, AssignedAge!=14)

#see if the household numbers are unique
Aged14AssignmentBoys$`Household Number`[duplicated(Aged14AssignmentBoys$`Household Number`)]
# 3 duplicates
# split out duplicates
Aged14AssignmentBoysDups <- Aged14AssignmentBoys[duplicated(Aged14AssignmentBoys[c(8)]) ,]
Aged14AssignmentBoysNoDups <- Aged14AssignmentBoys[!(Aged14AssignmentBoys$ID %in% Aged14AssignmentBoysDups$ID),]

set.seed(128181242)
Aged14AssignmentBoysNoDups$School <- sample(TimaruSchools14Boys$School,
                                            size=nrow(Aged14AssignmentBoysNoDups), replace=TRUE, prob=TimaruSchools14Boys$Percent)

#pull in college boys from matched household numbers to assign to the same school
AdditionalCollegeBoysMatched <- subset(CollegeAgedBoys, `Household Number` %in% Aged14AssignmentBoysNoDups$`Household Number`)
#13 matches on household
#append the duplicates
AdditionalCollegeBoysMatched <- rbind(AdditionalCollegeBoysMatched,Aged14AssignmentBoysDups)
#delete dups file
rm(Aged14AssignmentBoysDups)
#match duplicate's school
AdditionalCollegeBoysMatched$School <- Aged14AssignmentBoysNoDups$School[match(AdditionalCollegeBoysMatched$`Household Number`,
                                                                               Aged14AssignmentBoysNoDups$`Household Number`)]

#need a combined file of all matched college children, updated as we go
#construct a file of all who are matched
WorkingMatchedFile <- rbind(WorkingMatchedFile,Aged14AssignmentBoysNoDups,AdditionalCollegeBoysMatched)
#decrement the master working file of college girls by those already matched
CollegeAgedBoys <- CollegeAgedBoys[!(CollegeAgedBoys$ID %in% WorkingMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools14Boys,Children14AlreadyAssigned,Aged14AssignmentBoys,Aged14AssignmentBoysNoDups)

################################################################################################
################################################################################################
#15-year-old boys
################################################################################################
################################################################################################
#easiest to credit subset to work with
TimaruSchools15Boys <- subset(TimaruSchoolMeshblocks, School.Name %in% c("Roncalli College", "Mountainview High School","Timaru Boys' High School"),
                              select=c(School.Name,Age.15))

#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingMatchedFile, AssignedAge==15)
Children15AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools15Boys <- merge(Children15AlreadyAssigned,TimaruSchools15Boys, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools15Boys) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools15Boys$AlreadyAssigned[is.na(TimaruSchools15Boys$AlreadyAssigned)] <- 0
TimaruSchools15Boys$Age.15 <- TimaruSchools15Boys[,3]-TimaruSchools15Boys[,2]
#get percents
TimaruSchools15Boys$Percent <- TimaruSchools15Boys$Age.15/sum(TimaruSchools15Boys$Age.15)

#Assign the 15-year-old boys to a school based on the proportions in the school roll data
Aged15AssignmentBoys <- subset(CollegeAgedBoys, AssignedAge==15)

#remove the 15-year old boys from the Working File for college-aged boys
CollegeAgedBoys <- subset(CollegeAgedBoys, AssignedAge!=15)

#see if the household numbers are unique
Aged15AssignmentBoys$`Household Number`[duplicated(Aged15AssignmentBoys$`Household Number`)]
# 2 duplicates
# split out duplicates
Aged15AssignmentBoysDups <- Aged15AssignmentBoys[duplicated(Aged15AssignmentBoys[c(8)]) ,]
Aged15AssignmentBoysNoDups <- Aged15AssignmentBoys[!(Aged15AssignmentBoys$ID %in% Aged15AssignmentBoysDups$ID),]

set.seed(128181257)
Aged15AssignmentBoysNoDups$School <- sample(TimaruSchools15Boys$School,
                                            size=nrow(Aged15AssignmentBoysNoDups), replace=TRUE, prob=TimaruSchools15Boys$Percent)

#pull in college boys from matched household numbers to assign to the same school
AdditionalCollegeBoysMatched <- subset(CollegeAgedBoys, `Household Number` %in% Aged15AssignmentBoysNoDups$`Household Number`)
#15 matches on household
#append the duplicates
AdditionalCollegeBoysMatched <- rbind(AdditionalCollegeBoysMatched,Aged15AssignmentBoysDups)
#delete dups file
rm(Aged15AssignmentBoysDups)
#match duplicate's school
AdditionalCollegeBoysMatched$School <- Aged15AssignmentBoysNoDups$School[match(AdditionalCollegeBoysMatched$`Household Number`,
                                                                               Aged15AssignmentBoysNoDups$`Household Number`)]

#need a combined file of all matched college children, updated as we go
#construct a file of all who are matched
WorkingMatchedFile <- rbind(WorkingMatchedFile,Aged15AssignmentBoysNoDups,AdditionalCollegeBoysMatched)
#decrement the master working file of college girls by those already matched
CollegeAgedBoys <- CollegeAgedBoys[!(CollegeAgedBoys$ID %in% WorkingMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools15Boys,Children15AlreadyAssigned,Aged15AssignmentBoys,Aged15AssignmentBoysNoDups)

################################################################################################
################################################################################################
#16-year-old boys
################################################################################################
################################################################################################
#easiest to credit subset to work with
TimaruSchools16Boys <- subset(TimaruSchoolMeshblocks, School.Name %in% c("Roncalli College", "Mountainview High School","Timaru Boys' High School"),
                              select=c(School.Name,Age.16))

#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingMatchedFile, AssignedAge==16)
Children16AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools16Boys <- merge(Children16AlreadyAssigned,TimaruSchools16Boys, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools16Boys) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools16Boys$AlreadyAssigned[is.na(TimaruSchools16Boys$AlreadyAssigned)] <- 0
TimaruSchools16Boys$Age.16 <- TimaruSchools16Boys[,3]-TimaruSchools16Boys[,2]
#get percents
TimaruSchools16Boys$Percent <- TimaruSchools16Boys$Age.16/sum(TimaruSchools16Boys$Age.16)

#Assign the 16-year-old boys to a school based on the proportions in the school roll data
Aged16AssignmentBoys <- subset(CollegeAgedBoys, AssignedAge==16)

#remove the 16-year old boys from the Working File for college-aged boys
CollegeAgedBoys <- subset(CollegeAgedBoys, AssignedAge!=16)

#see if the household numbers are unique
Aged16AssignmentBoys$`Household Number`[duplicated(Aged16AssignmentBoys$`Household Number`)]
# 1 duplicates
# split out duplicate
Aged16AssignmentBoysDups <- Aged16AssignmentBoys[duplicated(Aged16AssignmentBoys[c(8)]) ,]
Aged16AssignmentBoysNoDups <- Aged16AssignmentBoys[!(Aged16AssignmentBoys$ID %in% Aged16AssignmentBoysDups$ID),]

set.seed(128181306)
Aged16AssignmentBoysNoDups$School <- sample(TimaruSchools16Boys$School,
                                            size=nrow(Aged16AssignmentBoysNoDups), replace=TRUE, prob=TimaruSchools16Boys$Percent)

#pull in college boys from matched household numbers to assign to the same school
AdditionalCollegeBoysMatched <- subset(CollegeAgedBoys, `Household Number` %in% Aged16AssignmentBoysNoDups$`Household Number`)
#3 matches on household
#append the duplicates
AdditionalCollegeBoysMatched <- rbind(AdditionalCollegeBoysMatched,Aged16AssignmentBoysDups)
#delete dups file
rm(Aged16AssignmentBoysDups)
#match duplicate's school
AdditionalCollegeBoysMatched$School <- Aged16AssignmentBoysNoDups$School[match(AdditionalCollegeBoysMatched$`Household Number`,
                                                                               Aged16AssignmentBoysNoDups$`Household Number`)]

#need a combined file of all matched college children, updated as we go
#construct a file of all who are matched
WorkingMatchedFile <- rbind(WorkingMatchedFile,Aged16AssignmentBoysNoDups,AdditionalCollegeBoysMatched)
#decrement the master working file of college girls by those already matched
CollegeAgedBoys <- CollegeAgedBoys[!(CollegeAgedBoys$ID %in% WorkingMatchedFile$ID), ]

#clean up interim files
rm(TimaruSchools16Boys,Children16AlreadyAssigned,Aged16AssignmentBoys,Aged16AssignmentBoysNoDups)

################################################################################################
################################################################################################
#17-year-old boys
################################################################################################
################################################################################################
#easiest to credit subset to work with
TimaruSchools17Boys <- subset(TimaruSchoolMeshblocks, School.Name %in% c("Roncalli College", "Mountainview High School","Timaru Boys' High School"),
                              select=c(School.Name,Age.17))

#decrease counts by those already assigned, use master matched file
temp <- subset(WorkingMatchedFile, AssignedAge==17)
Children17AlreadyAssigned <- as.data.frame(table(temp$School))
#deduct already matched counts from original counts
TimaruSchools17Boys <- merge(Children17AlreadyAssigned,TimaruSchools17Boys, by.x="Var1",by.y="School.Name")
colnames(TimaruSchools17Boys) <- c("School.Name", "AlreadyAssigned","OriginalCounts")
#replace any NA counts with 0
TimaruSchools17Boys$AlreadyAssigned[is.na(TimaruSchools17Boys$AlreadyAssigned)] <- 0
TimaruSchools17Boys$Age.17 <- TimaruSchools17Boys[,3]-TimaruSchools17Boys[,2]
#get percents
TimaruSchools17Boys$Percent <- TimaruSchools17Boys$Age.17/sum(TimaruSchools17Boys$Age.17)

#Assign the 17-year-old boys to a school based on the proportions in the school roll data
Aged17AssignmentBoys <- subset(CollegeAgedBoys, AssignedAge==17)

#remove the 17-year old boys from the Working File for college-aged boys
CollegeAgedBoys <- subset(CollegeAgedBoys, AssignedAge!=17)

#see if the household numbers are unique
Aged17AssignmentBoys$`Household Number`[duplicated(Aged17AssignmentBoys$`Household Number`)]
# no duplicates
set.seed(128181318)
Aged17AssignmentBoys$School <- sample(TimaruSchools17Boys$School,
                                            size=nrow(Aged17AssignmentBoys), replace=TRUE, prob=TimaruSchools17Boys$Percent)

#need a combined file of all matched college children, updated as we go
#construct a file of all who are matched
WorkingMatchedFile <- rbind(WorkingMatchedFile,Aged17AssignmentBoys)

#clean up interim files
rm(TimaruSchools17Boys,Children17AlreadyAssigned,Aged17AssignmentBoys)

################################################################################################
################################################################################################
# create master file of children with assigned schools
################################################################################################
################################################################################################
#merge the various data.frames: working matched file, working primary matched file
WorkingMatchedFile <- rbind(WorkingMatchedFile,WorkingPrimaryMatchedFile)
# error checking
# check for missing school
temp <- subset(WorkingMatchedFile, School=="")
#all allocated a school
#check for no missing ages
temp <- subset(WorkingMatchedFile, AssignedAge=="")
#check for duplicates of children
temp <- WorkingMatchedFile$ID[duplicated(WorkingMatchedFile$ID)]
#no duplicate children
#check variable types on working file
str(WorkingMatchedFile)
#create masterfile with schools
master_file_children_with_schools <- WorkingMatchedFile

#delete all temporary files
rm(AdditionalCollegeBoysMatched,AdditionalCollegeGirlsMatched,AdditionalPrimaryCoedMatched,AllMatchedChildren,Assignment,CollegeAgedBoys,CollegesAlreadyAssigned,
   MissingChildren,MountainviewRoncalli,WorkingFileCollegeGirls,WorkingFilePrimaryChildren,WorkingMatchedFile,WorkingPrimaryMatchedFile,temp)

#check how many school kids are employed by employment category, and by age
table(master_file_children_with_schools$HRSWORKED,master_file_children_with_schools$AssignedAge)


#show expected against actual
# #but first, randomly assign some 18-year olds to the school data.
# #use the rest of the 25-17 ones to decide the proportion of 18-year-olds coming from the census data
# #the remainder will be brought in from outside
# #this method means this age will be as realistic as the other ages
# #see how many 18-24 year olds in households
# table(Master_household_file_Timaru_2013_census_data$AGEBAND)
# 
# #look at master household file and see how many 18-24s not unemployed or in employment
# table(Master_household_file_Timaru_2013_census_data$HRSWORKED)
# #only 391 not elsewhere included in dataframe
# #pull out 18-24 year olds who are not employed into separate data.frame
# Aged18to24NotWorking <- subset(Master_household_file_Timaru_2013_census_data, AGEBAND==5 & 
#                                  (HRSWORKED=="Not Elsewhere Included" | HRSWORKED=="Not Working")) #632 people
# #drop the ones in relationships
# Aged18to24NotWorking <- subset(Aged18to24NotWorking, RELATIONSHIP!="Partnered") #500 people
# #drop the relationship NEC ones
# Aged18to24NotWorking <- subset(Aged18to24NotWorking, RELATIONSHIP!="Not Elsewhere Included") #331 people
# #check number of 18-year-olds in school
# sum(TimaruSchoolMeshblocks$Age.18)
# 
# #Check where I'm at
# Aged18to24MaybeRightApproach <- subset(Master_household_file_Timaru_2013_census_data, 
#                                        AGEBAND==5 & 
#                                          (HRSWORKED=="Not Elsewhere Included" | HRSWORKED=="Not Working") &
#                                          RELATIONSHIP=="Not Elsewhere Included" )