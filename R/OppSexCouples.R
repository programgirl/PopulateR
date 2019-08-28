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
#' @param NumIterations The maximum number of iterations used to construct the coupled data frame. This has a default value of 1000000, and is the stopping rule
#' if the algorithm does not converge.
#'
#'
#' @param CoupleIDValue The starting number for generating a variable that identifies the observations in a couple. Must be numeric.
#'
#' @return A data frame of an even number of observations for allocation into same-sex couples.
#'
#' @examples
#'

opposite_sex <- function(Recipient, RecipientIDVariable=NULL, RecipientAgeVariable=NULL, Donor, DonorIDVariable=NULL, DonorAgeVariable=NULL, xiUsed=NULL, OmegaUsed=NULL,
                         AlphaUsed=NULL, UserSeed=NULL, pValueToStop=NULL, NumIterations=1000000) {

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

  #####################################
  #####################################
  # sub functions are here
  #####################################
  #####################################
  # pairing swap subfunction

  swap_donor <- function(pair1, pair2) {
    swap <- pair1
    swap$DonorID <- pair2$DonorID
    swap$DonorAge <- pair2$DonorAge
    return(swap)
  }

  # #####################################
  # # chi-squared check subfunction
  #####################################

  compare_logK <- function(prop, curr) {
    # what we want to do is know if sum(exp(prop)) > sum(exp(curr))
    # but we can't work out exp(prop) or exp(curr) during the process..

    # to do this, we first eliminate those that don't matter
    w = prop != curr
    if (sum(w) == 0) {
      return(0) # no change
    }
    prop = prop[w]
    curr = curr[w]

    #     # next we find which is the dominant exponent, as changes these are all that will matter
    #     # i.e. we write exp(a) + exp(b) = exp(a)[1 + exp(b-a)] where a > b, so that the additional terms are less than 1
    #     # and we can exponentiate them safely. We then ignore the base (it's common) and just use extras
    base <- max(prop, curr)
    prop = prop - base
    curr = curr - base
    sum(exp(prop)) - sum(exp(curr))
  }

  #####################################
  #####################################
  # sub functions end
  #####################################
  #####################################

  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  # Recipient ID variable
  RecipientIDColName <- sym(names(Recipient[RecipientIDVariable]))

  # Recipient age variable
  RecipientAgeColName <- sym(names(Recipient[RecipientAgeVariable]))

  # Donor age variable
  DonorAgeColName <- sym(names(Donor[DonorAgeVariable]))

  #####################################
  #####################################
  # end column names
  #####################################
  #####################################


  #####################################
  #####################################
  # set up pre-data information for matching
  #####################################
  #####################################

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

  # estimate expected minimum and maximum ages from the distribution, and bin these

  min_bin <- round(sn::qsn(0.000001,xi=xiUsed, omega=OmegaUsed, alpha=AlphaUsed))-0.5
  max_bin <- round(sn::qsn(0.999999,xi=xiUsed, omega=OmegaUsed, alpha=AlphaUsed))+0.5
  bins <- c(-Inf, min_bin:max_bin, Inf)

  # construct the probabilities for each bin, gives n(bins)-1
  Probabilities <- sn::psn(bins[-1], xi=xiUsed, omega=OmegaUsed, alpha=AlphaUsed) -
    sn::psn(bins[-length(bins)], xi=xiUsed, omega=OmegaUsed, alpha=AlphaUsed)

  # assign realistic expected probabilities in the bins outside the bins constructed earlier
  # use minAge and maxAge for this, only need range for included ages
  # Uses midpoint rule.
  logProbLow <- sn::dsn(-MaxAgeDifference:(min_bin-0.5), xi=xiUsed, omega=OmegaUsed, alpha=AlphaUsed, log=TRUE)
  logProbHigh <- sn::dsn((max_bin+0.5):MaxAgeDifference, xi=xiUsed, omega=OmegaUsed, alpha=AlphaUsed, log=TRUE)

  logProb <- c(logProbLow, log(Probabilities[-c(1, length(Probabilities))]), logProbHigh)
  logBins    <- c(-Inf, -(MaxAgeDifference-.5):(MaxAgeDifference-.5), Inf)


  #####################################
  #####################################
  # end set up
  #####################################
  #####################################


  #####################################
  #####################################
  # create initial age matches
  #####################################
  #####################################
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

  #####################################
  #####################################
  # iteration for matching couple ages starts here
  #####################################
  #####################################

 for (i in 1:NumIterations) {

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
    filter(!({{RecipientIDColName}} %in% c(PropPair1[,1], PropPair2[,1]))) %>%
    bind_rows(., PropPair1,PropPair2)

    # do chi-squared
    Proplog0 <- hist(PropAgeMatch[,2] - PropAgeMatch[,3], breaks = logBins, plot=FALSE)$counts
    ProplogK = ifelse(Proplog0 == 0, 2*logEAgeProbs, log((Proplog0 - exp(logEAgeProbs))^2)) - logEAgeProbs

    prop_log_chisq = max(ProplogK) + log(sum(exp(ProplogK - max(ProplogK))))

    if (compare_logK(ProplogK, logKObservedAges) < 0) { # we cancel out the bits that haven't changed first.

      CurrentAgeMatch[Pick1,] <- PropPair1
      CurrentAgeMatch[Pick2,] <- PropPair2


      log0ObservedAges <- Proplog0
      logKObservedAges <- ProplogK
      log_chisq <- prop_log_chisq

    }

    if (log_chisq <= Critical_log_chisq) {
      break

    }

 }

  #####################################
  #####################################
  # iteration for matching couple ages ends here
  #####################################
  #####################################


  #####################################
  #####################################
  # pairing the actual couples starts here
  #####################################
  #####################################
  # return full donor and recipient rows as matched household pairs
  # extract ages counts for matching the donors
  MatchedDonorAges <- CurrentAgeMatch %>%
    select(DonorAge) %>%
    group_by(DonorAge) %>%
    mutate(DonorAgeCount = row_number()) %>%
    ungroup()


  # generate same AgeCount second ID variable for the donor data
  # the AgeCount is used to ensure that the first donor with a specific age is matched first
  # the second donor with a specific age is matched second
  # and so forth
  DonorsToMatch <- Donor %>%
    group_by({{DonorAgeColName}}) %>%
    mutate(DonorAgeCount = row_number()) %>%
    ungroup()

  # reduce pool of potentially partnered donors to only those matched to recipients
  DonorsMatched <- left_join(MatchedDonorAges, rename_at(DonorsToMatch, DonorAgeVariable, ~ names(MatchedDonorAges)[1]), by = c(names(MatchedDonorAges)[1], "DonorAgeCount")) %>%
                               mutate(!!DonorAgeColName := DonorAge)


  # construct same file for the recipients
  # need both donor age and donor age count so that the join between the recipients and the donors works
  # do not need Recipient age as this will be a duplicate column on the merge
  RecipientsMatchPrep <- CurrentAgeMatch %>%
    group_by(DonorAge) %>%
    mutate(DonorAgeCount = row_number()) %>%
    select(-c(2))

  RecipientsReadyToMatch <- left_join(Recipient, RecipientsMatchPrep, by = c(names(Recipient[RecipientIDVariable]), names(RecipientsMatchPrep[1])))

  # now merge the full data of the subset donors to the recipients
  # by donor age and donor age count
  # recipient data frame is the one to which observations must be joined
  # also add the household numbers at this point
  MaxCoupleIDValue <- (nrow(RecipientsReadyToMatch))-1

  FullMatchedDataFrame <- left_join(RecipientsReadyToMatch, DonorsMatched, by=c("DonorAge", "DonorAgeCount")) %>%
    ungroup() #%>%
  #   # mutate({{HouseholdNumVariable}} := rep((CoupleIDValue):(CoupleIDValue+MaxCoupleIDValue),
  #   #                                        each=2))


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

  #####################################
  #####################################
  # pairing the actual couples ends here
  #####################################
  #####################################


  return(FullMatchedDataFrame)

}


# library("dplyr")

# TestResults <- opposite_sex(OppSexPartneredMales, 5, 8, OppSexPartneredFemales,5, 8, 4,2,2,4, .01, 100)

# used different return specifications to get the two data frames below output
# MatchedDonorAges <- opposite_sex(OppSexPartneredMales, 5, 8, OppSexPartneredFemales,5, 8, 4,2,2,4, .01, 100)
# DonorsToMatch <- opposite_sex(OppSexPartneredMales, 5, 8, OppSexPartneredFemales,5, 8, 4,2,2,4, .01, 100)
# DonorsMatched <- opposite_sex(OppSexPartneredMales, 5, 8, OppSexPartneredFemales,5, 8, 4,2,2,4, .01, 100)
# RecipientsMatchPrep <- opposite_sex(OppSexPartneredMales, 5, 8, OppSexPartneredFemales,5, 8, 4,2,2,4, .01, 100)
# RecipientsReadyToMatch <- opposite_sex(OppSexPartneredMales, 5, 8, OppSexPartneredFemales,5, 8, 4,2,2,4, .01, 100)
# FullMatchedDataFrame <- opposite_sex(OppSexPartneredMales, 5, 8, OppSexPartneredFemales,5, 8, 4,2,2,4, .01, 100)
# at this point, check for duplicate Recipient and Donor IDs - should be none
# print(any(duplicated(FullMatchedDataFrame$ID.x)))
# print(any(duplicated(FullMatchedDataFrame$ID.y)))


# note - check


# library("tidyr")
# library("ggplot2")

# library("ggthemes")
# library("stringr")
#
#

#  bring in data - this uses the test file
# HH3P <- readRDS("~/Sync/PhD/PopSim/R/HH3P.Rds")
# split out the coupled data for testing,
# OppSexPartneredMales <- HH3P %>%
#   filter(SEX=="Male", RELATIONSHIP=="Partnered")

# OppSexPartneredFemales <- HH3P %>%
#   filter(SEX=="Female", RELATIONSHIP=="Partnered")



# code below is the basis for the function. Needs modification into a function.


# ################################################################################################
# # construct file of matched men and women
# # column bind women that match the FemaleAge
# # use SQL joins so that a one-to-one match is made in each case.
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
