#' Create a subset of observations containing only opposite-sex couples
#'
#' This function creates a data frame of couples, based on a population distribution of age differences. The distribution used is the skew normal.
#' Two data frames are required. The recipient data frame contains the age data, of one sex, to which the distribution will be applied. The
#' donor data frame contains the age data, of the other sex, from which the age counts to match are constructed. If the two data frames are different
#' lengths, the recipient data frame must be the shortest of the two. In this situation, a random subsample of the donor data frame will be used.
#' Both data frames must be restricted to only those ages that will have a couples match performed. No age reasonableness check is made.
#' An even number of observations is output, using the defined age-difference distribution between the female and male ages for the couples.
#'
#' The function performs a reasonableness check for the first five variables. If any other parameters are missing, the usual error messages from the imported
#' functions will be output.
#'
#' If desired, this can be used to construct same-sex couples.
#'
#' @export
#' @param Recipient A data frame containing observations limited to one sex. An age column is required. Only include the ages that are eligible for partner allocation.
#' @param RecipientIDVariable The column number for the recipient ID.
#' @param RecipientAgeVariable The column number for the age variable in the Recipient data frame.
#' @param Donor A data frame containing observations limited to one sex. An age column is required. Only include the ages that will be allocated to partners.
#' @param DonorIDVariable The column number for the donor ID. Must be numeric.
#' @param DonorAgeVariable The column number for the age variable in the Donor data frame.
#' @param xiUsed The xi value for the skew normal distribution.
#' @param OmegaUsed The omega value for the skew normal distribution.
#' @param AlphaUsed The alpha value for the skew normal distribution.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param pValueToStop The primary stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#'
#'
#' @param CoupleIDValue The starting number for generating a variable that identifies the observations in a couple. Must be numeric.
#'
#' @return A data frame of an even number of observations for allocation into same-sex couples.
#'
#' @examples
#'

opposite_sex <- function(Recipient, RecipientIDVariable=NULL, RecipientAgeVariable=NULL, Donor, DonorIDVariable=NULL, DonorAgeVariable=NULL, xiUsed=NULL, OmegaUsed=NULL,
                         AlphaUsed=NULL, UserSeed=NULL, pValueToStop=NULL) {

  # content check
  if (!any(duplicated(Recipient[RecipientIDVariable])) == FALSE) {
    stop("The column number for the ID variable in the recipient data frame must be supplied.")
  }

  if (!is.numeric(RecipientAgeVariable)) {
    stop("Both the Recipient ID and the Recipient age column numbers must be supplied.")
  }

  if (!any(duplicated(Donor[DonorIDVariable])) == FALSE) {
    stop("The column number for the ID variable in the donor data frame must be supplied.")
  }

  # pairing swap used later in the function
  swap_donor <- function(pair1, pair2) {
    swap <- pair1
    swap$DonorID <- pair2$DonorID
    swap$DonorAge <- pair2$DonorAge
    return(swap)
  }

  # get counts for each single age from the donor data frame
  DonorCounts <- Donor %>%
    group_by_at(DonorAgeVariable) %>%
    summarise(AgeCount=n())

  # DonorAges <- as.vector(DonorCounts[1])
  DonorAges <- pull(DonorCounts[1])
  DonorAgeCounts <- pull(DonorCounts[2])

  # set up bins for iterations
  # enable at least some extreme age differences to be assigned to the Inf categories
  # otherwise the bins will be wrong

  MaxAgeDifference <-  (max(Recipient[RecipientAgeVariable]) -
                           min(Donor[DonorAgeVariable]))-5

  # create max number of iterations in case iterations don't converge with a result
  NumIterations <- 1000000

  # estimate expected minimum and maximum ages from the distribution, and bin these

  min_bin <- round(qsn(0.000001,xi=xiUsed, omega=OmegaUsed, alpha=AlphaUsed))-0.5
  max_bin <- round(qsn(0.999999,xi=xiUsed, omega=OmegaUsed, alpha=AlphaUsed))+0.5
  bins <- c(-Inf, min_bin:max_bin, Inf)

  # construct the probabilities for each bin, gives n(bins)-1
  Probabilities <- psn(bins[-1], xi=xiUsed, omega=OmegaUsed, alpha=AlphaUsed) -
    psn(bins[-length(bins)], xi=xiUsed, omega=OmegaUsed, alpha=AlphaUsed)

  # assign realistic expected probabilities in the bins outside the bins constructed earlier
  # use minAge and maxAge for this, only need range for included ages
  # Uses midpoint rule.
  logProbLow <- dsn(-MaxAgeDifference:(min_bin-0.5), xi=xiUsed, omega=OmegaUsed, alpha=AlphaUsed, log=TRUE)
  logProbHigh <- dsn((max_bin+0.5):MaxAgeDifference, xi=xiUsed, omega=OmegaUsed, alpha=AlphaUsed, log=TRUE)

  logProb <- c(logProbLow, log(Probabilities[-c(1, length(Probabilities))]), logProbHigh)
  logBins    <- c(-Inf, -(MaxAgeDifference-.5):(MaxAgeDifference-.5), Inf)



  # create initial age matches
  # this is a random sample so age differences will not follow desired distribution
  # however, if the donor data frame is larger than the recipient data frame
  # this ensures that a random selection of donors has the correct count
   if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }


  CurrentAgeMatch <- data.frame(Recipient[RecipientIDVariable],
                                Recipient[RecipientAgeVariable],
                                DonorAge = sample(rep(DonorAges, DonorAgeCounts),
                                                size=nrow(Recipient),
                                                replace = FALSE))

  # set up for chi-squared test
  ExpectedAgeProbs <- Probabilities * nrow(CurrentAgeMatch)
  logEAgeProbs <- logProb + log(nrow(CurrentAgeMatch))

  # construct starting set of observed age difference values for iteration
  ObservedAgeDifferences <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,3], breaks = bins, plot=FALSE)$counts

  # set up for chi-squared
  log0ObservedAges <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,3], breaks = logBins, plot=FALSE)$counts
  logKObservedAges = ifelse(log0ObservedAges == 0, 2*logEAgeProbs, log((log0ObservedAges - exp(logEAgeProbs))^2)) - logEAgeProbs
  log_chisq = max(logKObservedAges) + log(sum(exp(logKObservedAges - max(logKObservedAges))))

  if (is.null(pValueToStop)) {

    Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))

  } else {

    Critical_log_chisq <- log(qchisq(pValueToStop, df=(length(logEAgeProbs-1)), lower.tail = TRUE))

  }


# iteration for getting couple ages starts here

#  for (i in 1:NumIterations) {

    # randomly choose two pairs
    Pick1 <- sample(nrow(CurrentAgeMatch), 1)
    Pick2 <- sample(nrow(CurrentAgeMatch), 1)
    Current1 <- CurrentAgeMatch[Pick1,]
    Current2 <- CurrentAgeMatch[Pick2,]

    # # proposed pairing after a swap
    PropPair1 <- swap_donor(Current1, Current2)
    PropPair2 <- swap_donor(Current2, Current1)

  # compute change in Chi-squared value from current pairing to proposed pairing
  PropAgeMatch <- CurrentAgeMatch %>%
    filter_at(!(1 %in% c(PropPair1[,1], PropPair2[,1]))) %>%
    bind_rows(., PropPair1,PropPair2)

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
#  }



return(PropAgeMatch)

}


# library("dplyr")
# library("sn")

TestResults <- opposite_sex(OppSexPartneredMales, 5, 8, OppSexPartneredFemales,5, 8, 4,2,2,4, .01)




# library("tidyr")
# library("ggplot2")

# library("ggthemes")
# library("stringr")
#
#

#  bring in data - this uses the test file
# HH3P <- readRDS("~/Sync/PhD/PopSim/R/HH3P.Rds")
# split out the coupled data for testing
# OppSexPartneredMales <- HH3P %>%
#   filter(SEX=="Male", RELATIONSHIP=="Partnered")
# OppSexPartneredFemales <- HH3P %>%
#   filter(SEX=="Female", RELATIONSHIP=="Partnered")



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
