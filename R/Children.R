# look at child age distribution
# these are counts generated from the Stats NZ aggregate cells. Only contains contributions from children aged 0-4, 5-9, 10-14, and 15-17
ChildDist0through17 <- data.frame(MaternalAge=c(16:49), ChildCount=c(82.58333, 82.58333, 99.29762, 99.29762, 82.04762, 193.7143, 193.7143, 234.8571, 234.8571, 152.8095,
                                                                     307.1429, 307.1429, 352.1429, 352.1429, 199.3333, 309, 309, 329.1429, 329.1429, 129.8095, 165.8095,
                                                                     165.8095, 169.6667, 169.6667, 39.85714, 45.85714, 45.85714, 45.85714, 45.85714, 6, 6, 6, 6, 6))

#
# GetSNDistn <- sn::msn.mple(ChildDist0through17$MaternalAge, ChildDist0through17$ChildCount, penalty = "Qpenalty")
#
# GetSNDistn
#
# sn::fitted(GetSNDistn)

plot(ChildDist0through17)

library("ggplot2")
ggplot(data=ChildDist0through17, aes(MaternalAge, ChildCount)) +
  geom_histogram(stat="identity", breaks = seq(16, 49, by=1),
                 fill="grey") +
  geom_smooth()

#smoothed values should be:
SmoothedValues <- predict(loess(ChildCount ~ MaternalAge, ChildDist0through17, ChildDist0through17$MaternalAge))
plot(SmoothedValues)

ChildDist0through17 <- cbind(ChildDist0through17, SmoothedValues)

library("dplyr")
ChildDist0through17 <- ChildDist0through17 %>%
  mutate(PropofBirths = SmoothedValues/sum(SmoothedValues))

plot(ChildDist0through17$PropofBirths)


# use package fitdistrplus to find best distribution
fitdistrplus::descdist(ChildDist0through17$ChildCount, discrete=FALSE, boot=500)

# use this data to generate skew normal distribution
# after ripping my hair out with various parameterisations, this works. Note that the mean had to shift from 29 to 25 in order to get this to work. Minimum age is 16, max will round to 55
SN34 <- sn::rsn(100000, xi=25, omega=6, alpha=2)
hist(SN34)
# get min age
# if min age < actual min age, add difference to actual min age

parent_child <- function() {


}


library("dplyr")

#  bring in data - this uses the test file
# HH3P <- readRDS("~/Sync/PhD/PopSim/R/HH3P.Rds")
# pull out definite dependent children
# Children <- HH3P %>%
#   filter(AssignedAge < 16)

# get parents/guardians
# Parents1 <- HH3P %>%

