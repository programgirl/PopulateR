#' Create a subset of observations containing only opposite-sex couples
#'
#' This is a wrapper for randomly sampling observations into opposite-sex couples.
#' An even number of observations is output, with an appropriate age-difference distribution between the female and male ages in the couples.
#'
#' @export
#' @param dataframe A data frame containing observations limited to one sex. An age column is required.
#' @param ProbSameSex The probability of any observation being assigned to a same-sex couple.
#' @param UpWeight If TRUE, a subset of ages will be over-sampled.
#' @param UpWeightLowerAge The youngest age for the over-sampling. Required if UpWeight is TRUE.
#' @param UpWeightUpperAge The oldest age for the over-sampling. Required if UpWeight is TRUE.
#' @param AgeVariableIndex The column number of the data frame that contains the ages. Only used if over-sampling is specified. Required if UpWeight is TRUE.
#' @param CoupleIDValue The starting number for generating a variable that identifies the observations in a couple. Must be numeric.
#'
#' @return A data frame of an even number of observations for allocation into same-sex couples.
#'
#' @examples
#'

opposite_sex <- function(dataframe) {


}


# library("dplyr")
# library("tidyr")
# library("ggplot2")
# library("sn")
# library("ggthemes")
# library("stringr")
#
#

# code below is the basis for the function. Needs modification into a function.

# ################################################################################################
# ################################################################################################
# # opposite sex couples
# ################################################################################################
# ################################################################################################
# # match remaining men and women marked as "matched" - there will be some non-matches as more women are recorded as partnered
# # use a skewed normal distribution
# # a normal distribution won't retain high enough peaks at standard age differences between partners
# # will use the sn package to generate a skewed normal distribution - this retains the high peaks at the mean values
# # the mean is set to 2 years difference, with men generally being older than women
# # the method is:
# # 1. get the counts of women by single age
# # 2. construct two lists of these so that one is the age and the other is the corresponding count
# # 3. generate the skewed normal rng for the man, and the matching age is his age plus the rng number (rounded)
# # 4. as the minimum age is 18, the matching age index is matching age - 17
# # 5. check if there is still a non-zero count in the age list
# # 6a. if so, keep the age as the matched age and decrease the count by 1 in the count list
# # 6b. if the matching age has a count of 0 in the table, set the matched age to NA
# # 7. the non-matched ages will be the May-December matches - check this (make sure there aren't too many non-matches)
#
# # remove same-sex households from the 2-person household partnered data
# # this is for women
# # get counts for each single age for the women
# Partnered2PHHDiffSexFemCounts <- Partnered2PHH %>%
#   filter(RELATIONSHIP=="Partnered", SEX=="Female", AssignedAge>=17, !(ID %in% Partnered2PHHSameSexWomen$ID)) %>%
#   group_by(AssignedAge) %>%
#   summarise(AgeCount=n())
#
# Partnered2PHHCounts <- Partnered2PHHDiffSexFemCounts$AgeCount
# Partnered2PHHAges <- Partnered2PHHDiffSexFemCounts$AssignedAge
#
# # remove same-sex households from the 2-person household partnered data
# # this is for men
# Partnered2PHHDiffSexMales <- Partnered2PHH %>%
#   filter(RELATIONSHIP=="Partnered", SEX=="Male", !(ID %in% Partnered2PHHSameSexMen$ID))
#
# # generate the constants
# # the age difference used must be smaller than the maximum age difference possible so that the bins work
# xiUsed <- -2
# omegaUsed <- 6
# alphaUsed <- 2
# # this must enable at least some extreme age differences to be assigned to the Inf categories
# # otherwise the bins will be wrong
# AgeDifferenceUsed <-  (max(Partnered2PHHDiffSexMales$AssignedAge) -
#                          min(Partnered2PHHDiffSexFemCounts$AssignedAge))-5
# NumIterations <- 1000000
#
# # estimate expected minimum and maximum ages from the distribution, and bin these
# set.seed(161018)
# min_bin <- round(qsn(0.000001,xi=xiUsed, omega=omegaUsed, alpha=alphaUsed))-0.5
# max_bin <- round(qsn(0.999999,xi=xiUsed, omega=omegaUsed, alpha=alphaUsed))+0.5
# bins <- c(-Inf, min_bin:max_bin, Inf)
#
# # construct the probabilities for each bin, gives n(bins)-1
# Probabilities <- psn(bins[-1], xi=xiUsed, omega=omegaUsed, alpha=alphaUsed) -
#   psn(bins[-length(bins)], xi=xiUsed, omega=omegaUsed, alpha=alphaUsed)
#
# # assign realistic expected probabilities in the bins outside the bins constructed earlier
# # use minAge and maxAge for this, only need range for included ages
# # Just use midpoint rule. This will underestimate a little probably (concave up)
# logProbLow <- dsn(-AgeDifferenceUsed:(min_bin-0.5), xi=xiUsed, omega=omegaUsed, alpha=alphaUsed, log=TRUE)
# logProbHigh <- dsn((max_bin+0.5):AgeDifferenceUsed, xi=xiUsed, omega=omegaUsed, alpha=alphaUsed, log=TRUE)
#
# logProb <- c(logProbLow, log(Probabilities[-c(1, length(Probabilities))]), logProbHigh)
# logBins    <- c(-Inf, -(AgeDifferenceUsed-.5):(AgeDifferenceUsed-.5), Inf)
#
# # create expected values
# OppPairs2PHH <- data.frame(MaleID = Partnered2PHHDiffSexMales$ID,
#                            MaleAge = Partnered2PHHDiffSexMales$AssignedAge,
#                            FemaleAge=sample(rep(Partnered2PHHAges, Partnered2PHHCounts),
#                                             size=nrow(Partnered2PHHDiffSexMales),
#                                             replace = FALSE))
#
# Expected <- Probabilities * nrow(OppPairs2PHH)
# logE <- logProb + log(nrow(OppPairs2PHH))
#
# # construct starting set of observed values for iteration
# Observed <- hist(OppPairs2PHH$MaleAge - OppPairs2PHH$FemaleAge, breaks = bins, plot=FALSE)$counts
#
# # do chi-squared
# logO <- hist(OppPairs2PHH$MaleAge - OppPairs2PHH$FemaleAge, breaks = logBins, plot=FALSE)$counts
# logK = ifelse(logO == 0, 2*logE, log((logO - exp(logE))^2)) - logE
#
# log_chisq = max(logK) + log(sum(exp(logK - max(logK))))
#
#
# # improve the distribution of the difference in ages
# # maximum number of iterations is set, let user of package choose their own?
# # let user choose their own p-value to stop?
#
# ptm <- proc.time() # for testing
# WrongAllocations <- numeric(NumIterations)
# Critical_log_chisq <- log(qchisq(0.99, df=(length(logE-1)), lower.tail = FALSE))
#
# for (i in 1:NumIterations) {
#
#   # choose two pairs
#   wch1 <- sample(nrow(OppPairs2PHH), 1)
#   wch2 <- sample(nrow(OppPairs2PHH), 1)
#   curr1 <- OppPairs2PHH[wch1,]
#   curr2 <- OppPairs2PHH[wch2,]
#
#   # proposed pairing after a swap
#   prop1 <- swap_female(curr1, curr2)
#   prop2 <- swap_female(curr2, curr1)
#
#   # compute change in Chi-squared value from current pairing to proposed pairing
#   PropOppPairs2PHH <- OppPairs2PHH %>%
#     filter(!(MaleID %in% c(prop1$MaleID, prop2$MaleID))) %>%
#     bind_rows(., prop1,prop2)
#
#   # do chi-squared
#
#   ProplogO <- hist(PropOppPairs2PHH$MaleAge - PropOppPairs2PHH$FemaleAge, breaks = logBins, plot=FALSE)$counts
#   ProplogK = ifelse(ProplogO == 0, 2*logE, log((ProplogO - exp(logE))^2)) - logE
#
#   compare_logK <- function(prop, curr) {
#     # what we want to do is know if sum(exp(prop)) > sum(exp(curr))
#     # but we can't work out exp(prop) or exp(curr) during the process..
#
#     # to do this, we first eliminate those that don't matter
#     w = prop != curr
#     if (sum(w) == 0) {
#       return(0) # no change
#     }
#     prop = prop[w]
#     curr = curr[w]
#
#     # next we find which is the dominant exponent, as changes these are all that will matter
#     # i.e. we write exp(a) + exp(b) = exp(a)[1 + exp(b-a)] where a > b, so that the additional terms are less than 1
#     # and we can exponentiate them safely. We then ignore the base (it's common) and just use extras
#     base <- max(prop, curr)
#     prop = prop - base
#     curr = curr - base
#     sum(exp(prop)) - sum(exp(curr))
#   }
#
#   prop_log_chisq = max(ProplogK) + log(sum(exp(ProplogK - max(ProplogK))))
#
#   if (compare_logK(ProplogK, logK) < 0) { # we cancel out the bits that haven't changed first.
#
#     OppPairs2PHH[wch1,] <- prop1
#     OppPairs2PHH[wch2,] <- prop2
#
#
#     logO <- ProplogO
#     logK <- ProplogK
#     log_chisq <- prop_log_chisq
#
#
#   } else {
#
#   }
#   WrongAllocations[i] <- log_chisq
#
#   if (log_chisq <= Critical_log_chisq) {
#     break
#
#   }
#
# }
#
# proc.time() - ptm
# LookAtIt <- as.data.frame(WrongAllocations) %>%
#   filter(. >0)
# # plot(LookAtIt)
#
#
# # plot the histogram of the age differences in the matches
# AgeDifferencesOutput <- data.frame(OppPairs2PHH$MaleAge-OppPairs2PHH$FemaleAge) %>%
#   rename_at( 1, ~"AgeDifference" ) %>%
#   group_by(AgeDifference) %>%
#   summarise(count=n()) %>%
#   mutate(perc=count/sum(count))
#
# ggplot(AgeDifferencesOutput, aes(x=factor(AgeDifference), y=perc)) +
#   geom_bar(stat="identity") +
#   scale_y_continuous(labels=function(x)paste0(x*100,"%"), limits=c(0,0.1), breaks = seq(0,.1,.02)) +
#   labs(#title="Age difference of matched opposite-sex partners in two-person households",
#     x ="Age difference (years, male age is base)", y = "Percentage of matches") +
#   geom_hline(yintercept=seq(0, .1, .02), col="white", lwd=1) +
#   theme_tufte()
# ggsave("~/Sync/PhD/Thesis dissertation/HH2POppSexMtchdCpls.pdf")
#
#
#
#
# ################################################################################################
# # construct file of matched men and women
# # column bind women that match the FemaleAge
# # use SQL joins so that a one-to-one match is made in each case.
#
# # create the partnered 2PHH female subset
# Partnered2PHHDiffSexFemales <- Partnered2PHH %>%
#   filter(RELATIONSHIP=="Partnered", SEX=="Female", AssignedAge>=17, !(ID %in% Partnered2PHHSameSexWomen$ID))
#
# # extract ages counts for matching the women
# HH2PMatchedWomenAges <- OppPairs2PHH %>%
#   select(FemaleAge) %>%
#   group_by(FemaleAge) %>%
#   mutate(AgeCount = row_number())
#
# # generate same AgeCount second ID variable for the women data
# # the AgeCount is used to ensure that the first woman with a specific age is matched first
# # the second woman with a specific age is matched second
# # and so forth
# HH2PAllWomenToMatch <- Partnered2PHH %>%
#   filter(RELATIONSHIP=="Partnered", SEX=="Female", AssignedAge>16, !(ID %in% Partnered2PHHSameSexWomen$ID)) %>%
#   group_by(AssignedAge) %>%
#   mutate(AgeCount = row_number(), FemaleAge = AssignedAge)
#
# # reduce partnered women to ones with an actual match
# HH2PActuallyMatchedWomen <- left_join(HH2PMatchedWomenAges, HH2PAllWomenToMatch)
#
# # construct same file for the partnered men
# # bring in all data at this point
# HH2PDiffSexMenMatchPrep <- OppPairs2PHH %>%
#   group_by(FemaleAge) %>%
#   mutate(AgeCount = row_number()) %>%
#   left_join(Partnered2PHHDiffSexMales, by=c("MaleID" = "ID"))
#
# # now merge the partnered women to the partnered men
# # by FemaleAge and AgeCount
# # women outnumber the men so left_join on the partnered men
# # also add the household numbers at this point
# OppSex2PHHStartNumber <- max(Partnered2PHHSameSexWomen$Household)+1
# OppSex2PHHMaxNumber <- nrow(HH2PDiffSexMenMatchPrep)-1
#
# HH2PPartneredOppSex <- left_join(HH2PDiffSexMenMatchPrep,HH2PActuallyMatchedWomen,
#                                  by=c("FemaleAge", "AgeCount")) %>%
#   ungroup() %>%
#   mutate(Household = rep(OppSex2PHHStartNumber:(OppSex2PHHStartNumber+OppSex2PHHMaxNumber), each=1))
#
# # convert from wide to long
# # need to rename columns because the join earlier added in suffixes
# HH2POppSexMenTemp <- HH2PPartneredOppSex %>%
#   select(1, 5:12, 22) %>%
#   rename(ID = MaleID) %>%
#   rename_all(list(~gsub("\\.x$", "", .))) %>%
#   select(SEX, AGEBAND, RELATIONSHIP, INHABITANTS, ID, AGEBANDnum, INHABITANTSnum, AssignedAge, FixedHours,
#          Household)
#
# HH2POppSexWomenTemp <- HH2PPartneredOppSex %>%
#   select(13:22) %>%
#   rename_all(list(~gsub("\\.y$", "", .))) %>%
#   select(SEX, AGEBAND, RELATIONSHIP, INHABITANTS, ID, AGEBANDnum, INHABITANTSnum, AssignedAge, FixedHours,
#          Household)
#
# HH2PAllPartnered <- rbind(Partnered2PHHSameSexMen, Partnered2PHHSameSexWomen, HH2POppSexMenTemp,
#                           HH2POppSexWomenTemp)
#
# # delete extra files
# rm(AgeDifferencesOutput, curr1, curr2, HH2PActuallyMatchedWomen, HH2PAllWomenToMatch,
#    HH2PDiffSexMenMatchPrep, HH2PMatchedWomenAges, HH2POppSexMenTemp, HH2POppSexWomenTemp,
#    HH2PPartneredOppSex, LookAtIt, OppPairs2PHH, Partnered2PHH, Partnered2PHHDiffSexFemales,
#    Partnered2PHHDiffSexMales, Partnered2PHHDiffSexFemCounts, Partnered2PHHSameSexMen, Partnered2PHHSameSexWomen,
#    prop1, prop2, PropOppPairs2PHH, AgeDifferenceUsed, alphaUsed, bins, Critical_log_chisq, Expected, i,
#    log_chisq, logBins, logE, logK, logO, logProb, logProbHigh, logProbLow, max_bin, min_bin, NumIterations,
#    Observed, omegaUsed, OppSex2PHHMaxNumber, OppSex2PHHStartNumber, Partnered2PHHAges, Partnered2PHHCounts,
#    Probabilities, prop_log_chisq, ProplogK, ProplogO, ptm, wch1, wch2, WrongAllocations, xiUsed)
