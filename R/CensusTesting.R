
library("dplyr")
library("ggplot2")
library("sn")

#  bring in data - this uses the test file
# HH3P <- readRDS("~/Sync/PhD/PopSim/R/HH3P.Rds")

# prep for households
# import data on the number of children that same sex couples have
# TABLECODE8163 <- read.csv(
#   "~/Sync/PhD/egg project/Timaru census data cleaning/Same sex couples information/Number of children/TABLECODE8163_Data_ecdbd669-529d-4052-96e1-ebf7934e8b0e.csv")
#  countSameSexMales <- nrow(PartneredMales)*("~/Sync/PhD/egg project/Timaru census data cleaning/TABLECODE8160_Data_aa91bc90.8521.4921.9d1f.67334c9ded14[3,3]")
# import same sex couple household information
# TABLECODE8160 <- read.csv(
# "~/Sync/PhD/egg project/Timaru census data cleaning/Same sex couples information/Family type with type of couple/TABLECODE8160_Data_aa91bc90-8521-4921-9d1f-67334c9ded14.csv")
# import same sex couple age information
# TABLECODE8161 <- read.csv(
# "~/Sync/PhD/egg project/Timaru census data cleaning/Same sex couples information/Age group of people in same-sex couples in occupied private dwellings/TABLECODE8161_Data_88aa206a-26ef-4733-87a5-434df88589b7.csv")
# calculate peak same sex age proportions
# SameSexPeakAgeCounts <- TABLECODE8161 %>%
#   filter(Age.group.of.people.in.same.sex.couples %in% c("25-34 years", "35-44 years", "45-54 years")) %>%
#   summarise(Value=sum(Value))
# get proportion of peak same sex ages as a function of totals
# SameSexPeakAgeProp <- SameSexPeakAgeCounts/TABLECODE8161[1,3]

########################################################################################################
########################################################################################################
#test same sex code
# split out the males, and do the prep
# PartneredMales <- HH3P %>%
#   filter(SEX=="Male", RELATIONSHIP=="Partnered")
# ProbSameSexMales <- (TABLECODE8160[3,3]/TABLECODE8160[5,3])*2

# run function
# SameSexMales <- same_sex(PartneredMales, ProbSameSexMales, as.numeric(SameSexPeakAgeProp), 25, 54, 8, 1, "Households")

# split out females
# PartneredFemales <- HH3P %>%
#   filter(SEX=="Female", RELATIONSHIP=="Partnered")
# SameSexFemales <- same_sex(PartneredFemales , .1, TRUE, .6, 25, 54, 8, 50, MyHouseholds)
# ProbSameSexFemales <- (TABLECODE8160[4,3]/TABLECODE8160[5,3])*2
# SameSexFemales <- same_sex(PartneredFemales, ProbSameSexFemales, as.numeric(SameSexPeakAgeProp), 25, 54, 8, (nrow(SameSexMales)/2)+1, "Households")

# Add both dataframes into one same sex coupled data frame
# SameSex <- rbind(SameSexMales, SameSexFemales)

# Subset original data frame so that the same sex couples no longer remain
# SubsetNoSameSex <- HH3P %>%
# filter(!ID %in% SameSex$ID)
# saveRDS(SubsetNoSameSex, "SubsetNoSameSex.rds")

########################################################################################################
########################################################################################################
# test opposite sex couples
#  bring in data - this uses the test file

# Exclude the children
# ExcludingChildren <- SubsetNoSameSex %>%
# filter(AssignedAge > 17)
#
# split out the males
# PartneredMalesOppSex <- ExcludingChildren %>%
#   filter(SEX=="Male", RELATIONSHIP=="Partnered")

# split out females
# PartneredFemalesOppSex <- ExcludingChildren %>%
#   filter(SEX=="Female", RELATIONSHIP=="Partnered")

# OppSexPartners <- opposite_sex(PartneredMalesOppSex, 5, 8, PartneredFemalesOppSex, 5, 8, 2, 6, 2, , pValueToStop=.01, , (nrow(SameSex)/2)+1, "Households")

# Add both dataframes into one same sex coupled data frame

# Subset original data frame so that the same sex couples no longer remain
# SubsetNoPartners <- SubsetNoSameSex %>%
# filter(!ID %in% OppSexPartners$ID)
# saveRDS(SubsetNoPartners, "SubsetNoPartners.rds")

########################################################################################################
########################################################################################################
# look at child age distribution
# these are counts generated from the Stats NZ aggregate cells. Only contains contributions from children aged 0-4, 5-9, 10-14, and 15-17
# ChildDist0through17 <- data.frame(MaternalAge=c(16:49), ChildCount=c(82.58333, 82.58333, 99.29762, 99.29762, 82.04762, 193.7143, 193.7143, 234.8571, 234.8571, 152.8095,
#                                                                      307.1429, 307.1429, 352.1429, 352.1429, 199.3333, 309, 309, 329.1429, 329.1429, 129.8095, 165.8095,
#                                                                      165.8095, 169.6667, 169.6667, 39.85714, 45.85714, 45.85714, 45.85714, 45.85714, 6, 6, 6, 6, 6))
#
# #
# # GetSNDistn <- sn::msn.mple(ChildDist0through17$MaternalAge, ChildDist0through17$ChildCount, penalty = "Qpenalty")
# #
# # GetSNDistn
# #
# # sn::fitted(GetSNDistn)
#
# plot(ChildDist0through17)
#
# library("ggplot2")
# ggplot(data=ChildDist0through17, aes(MaternalAge, ChildCount)) +
#   geom_histogram(stat="identity", breaks = seq(16, 49, by=1),
#                  fill="grey") +
#   geom_smooth()
#
# #smoothed values should be:
# SmoothedValues <- predict(loess(ChildCount ~ MaternalAge, ChildDist0through17, ChildDist0through17$MaternalAge))
# plot(SmoothedValues)
#
# ChildDist0through17 <- cbind(ChildDist0through17, SmoothedValues)
#
# library("dplyr")
# ChildDist0through17 <- ChildDist0through17 %>%
#   mutate(PropofBirths = SmoothedValues/sum(SmoothedValues))
#
# plot(ChildDist0through17$PropofBirths)
#
#
# # use package fitdistrplus to find best distribution
# fitdistrplus::descdist(ChildDist0through17$ChildCount, discrete=FALSE, boot=500)
#
# # use this data to generate skew normal distribution
# # after ripping my hair out with various parameterisations, this works. Note that the mean had to shift from 29 to 25 in order to get this to work. Minimum age is 16, max will round to 55
# SN34 <- sn::rsn(100000, xi=25, omega=6, alpha=2)
# hist(SN34)
# get min age
# if min age < actual min age, add difference to actual min age

# bring in the fertility population file
library(readxl)
FertilityRates <- read_excel("~/Sync/PhD/egg project/Timaru census data cleaning/More census 2013 data/ethnic-fertility-rates-2001-2006-2013.xls", sheet=3, range = "a9:c47")
colnames(FertilityRates) <- c("MaternalAgeAtBirth", "AvAnnBrths2012To2014", "FertlRatePer1000")
FertilityRates <- FertilityRates %>%
  mutate(ProbByMatAge = FertlRatePer1000/sum(FertlRatePer1000))

# look at distribution of probabilities
ggplot(FertilityRates, aes(x=MaternalAgeAtBirth, y=ProbByMatAge)) +
  geom_bar(stat="identity")

# try to find right sn distribution
# SN34 <- sn::rsn(100000, xi=25, omega=6, alpha=2)
# hist(SN34)
SN34 <- sn::rsn(100000, xi=25, omega=-6, alpha=2)
SN34 <- sn::rsn(100000, xi=25, omega=-6, alpha=2)
SN34 <- sn::rsn(100000, xi=31, omega=-6, alpha=2)
SN34 <- sn::rsn(100000, xi=34, omega=-6, alpha=2)

SN34 <- sn::rsn(100000, xi=35, omega=-6, alpha=2)

SN34 <- sn::rsn(100000, xi=35, omega=-5, alpha=3)
SN34 <- sn::rsn(100000, xi=35, omega=-3, alpha=4)

SN34 <- sn::rsn(100000, xi=35, omega=-4, alpha=4)

SN34 <- sn::rsn(100000, xi=35, omega=-4, alpha=3)
SN34 <- sn::rsn(100000, xi=35, omega=-2, alpha=4)
SN34 <- sn::rsn(100000, xi=35, omega=-5, alpha=4)
SN34 <- sn::rsn(100000, xi=35, omega=-4, alpha=4)
SN34 <- sn::rsn(100000, xi=35, omega=-4, alpha=5)
SN34 <- sn::rsn(100000, xi=35, omega=-2, alpha=5)
SN34 <- sn::rsn(100000, xi=35, omega=-5, alpha=8)
SN34 <- sn::rsn(100000, xi=35, omega=-5, alpha=12)
SN34 <- sn::rsn(100000, xi=35, omega=-5, alpha=-4)
SN34 <- sn::rsn(100000, xi=35, omega=-1, alpha=15)
SN34 <- sn::rsn(100000, xi=35, omega=-1, alpha=0)
SN34 <- sn::rsn(100000, xi=35, omega=0, alpha=0)
SN34 <- sn::rsn(100000, xi=35, omega=0, alpha=10)

SN3568 <-rsn(100000, xi=35, omega=-6, alpha=8)
hist(SN3568)

SN3558 <-rsn(100000, xi=35, omega=-5, alpha=8)
hist(SN3558)

SN3258 <-rsn(100000, xi=32, omega=-5, alpha=8)
hist(SN3258)

SN3218 <-rsn(100000, xi=32, omega=-1, alpha=8)
hist(SN3218)

SN32112 <-rsn(100000, xi=32, omega=-1, alpha=12)
hist(SN32112)

SN32125 <-rsn(100000, xi=32, omega=-1, alpha=25)
hist(SN32125)

SN32140 <-rsn(100000, xi=32, omega=-1, alpha=40)
hist(SN32140)

SN32540 <-rsn(100000, xi=32, omega=-5, alpha=40)
hist(SN32540)

SN32340 <-rsn(100000, xi=32, omega=-3, alpha=40)
hist(SN32340)

SN32140 <-rsn(100000, xi=32, omega=-1, alpha=40)
hist(SN32140)

SN32160 <-rsn(100000, xi=32, omega=-1, alpha=60)
hist(SN32160)

SN32601 <-rsn(100000, xi=32, omega=-60, alpha=1)
hist(SN32601)

SN32151 <-rsn(100000, xi=32, omega=-15, alpha=1)
hist(SN32151)

SN32101 <-rsn(100000, xi=32, omega=-10, alpha=-1)
hist(SN32101)

SN30102 <-rsn(100000, xi=30, omega=-10, alpha=-2)
hist(SN30102)

SN30102b <-rsn(100000, xi=30, omega=-10, alpha=2)
hist(SN30102b)

SN34102 <-rsn(100000, xi=34, omega=-10, alpha=2)
hist(SN34102)

SN40102 <-rsn(100000, xi=40, omega=-10, alpha=2)
hist(SN40102)

SN40092 <-rsn(100000, xi=40, omega=-9, alpha=2)
hist(SN40092)

SN40082 <-rsn(100000, xi=40, omega=-8, alpha=2)
hist(SN40082)

SN40081 <-rsn(100000, xi=40, omega=-8, alpha=1)
hist(SN40081)

SN36071 <-rsn(100000, xi=36, omega=-7, alpha=1)
hist(SN36071)

SN36061 <-rsn(100000, xi=36, omega=-6, alpha=1)
hist(SN36061)

SN38061 <-rsn(100000, xi=38, omega=-6, alpha=1)
hist(SN38061)

SN38051 <-rsn(100000, xi=38, omega=-5, alpha=1)
hist(SN38051)

SN38041 <-rsn(100000, xi=38, omega=-4, alpha=1)
hist(SN38041)

SN38042 <-rsn(100000, xi=38, omega=-4, alpha=2)
hist(SN38042)

SN38062 <-rsn(100000, xi=38, omega=-6, alpha=2)
hist(SN38062)

SN38082 <-rsn(100000, xi=38, omega=-8, alpha=2)
hist(SN38082)

SN38092 <-rsn(100000, xi=39, omega=-8, alpha=2)
hist(SN38092)

SN40061 <-rsn(100000, xi=40, omega=-6, alpha=1)
hist(SN40061)

SN38061 <-rsn(100000, xi=38, omega=-6, alpha=1)
hist(SN38061)

SN35051 <-rsn(100000, xi=35, omega=-5, alpha=1)
hist(SN35051)

SN38051 <-rsn(100000, xi=38, omega=-5, alpha=1)
hist(SN38051)
