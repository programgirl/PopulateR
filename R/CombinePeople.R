#' Create a match of people into households
#' This function creates a data frame of household inhabitants, of the required numbered of inhabitants.
#' One data frame, containing the people to match, is required. The function outputs an equivalent data frame of households. If the number of people in the data frame is not divisible by the household size specified, the excess number of people will be removed using a random process.
#' The use of an age distribution for the matching ensures that an age structure is present in the households. The age structure can be reduced by entering a larger standard deviation and/or a less-restrictive p-value.
#' The function performs a reasonableness check for person ID variable, person age, and household number.
#'
#' @export
#' @param Occupants A data frame containing observations limited to the people to be matched into households An age column is required.
#' @param IDVariable The column number for the ID variable.
#' @param AgeVariable The column number for the Age variable.
#' @param HouseholdSize The number of people expected in each household.
#' @param meanUsed The mean of the normal distribution for the distribution of parent ages at the time the child is born. For women, this will commonly be the age at childbirth.
#'  @param sdUsed The standard deviation of the normal distribution for the distribution of parent ages at the time the child is born. For women, this will commonly be the age at childbirth.
#' @param HouseholdIDValue The starting number for generating a variable that identifies the observations in the same household. Must be numeric. If no value is provided, the Household ID starts at 1.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied, and in quotes.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param pValueToStop = The primary stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param NumIterations The maximum number of iterations used to construct the household data frame. This has a default value of 1000000, and is the stopping rule if the algorithm does not converge.

OccupantN <- function(Occupants, IDVariable, AgeVariable, HouseholdSize = NULL, meanUsed, sdUsed, HouseholdIDValue = NULL, HouseholdNumVariable= NULL,
                      UserSeed=NULL, pValueToStop = .01, NumIterations = 1000000)
{

  options(dplyr.summarise.inform=F)

  # content check
  if (!any(duplicated(Occupants[IDVariable])) == FALSE) {
    stop("The column number for the ID variable in the data frame must be supplied.")
  }

  if (!is.numeric(AgeVariable)) {
    stop("Both the people ID and the age column numbers must be supplied.")
  }

  if (is.null(HouseholdSize)) {
    stop("Both the people ID and the age column numbers must be supplied.")
  }

    if (is.null(HouseholdNumVariable)) {
    stop("A name for the household count variable must be supplied.")
  }

  #####################################
  #####################################
  # sub functions are here
  #####################################
  #####################################

  # pairing swap subfunction

   swap_people <- function(pair1, pair2) {
    swap <- pair1
    swap$IDVariable <- pair2$IDVariable
    swap$AgeVariable <- pair2$AgeVariable
    return(swap)
  }


  #####################################
  # chi-squared check subfunction
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

        # next we find which is the dominant exponent, as changes these are all that will matter
        # i.e. we write exp(a) + exp(b) = exp(a)[1 + exp(b-a)] where a > b, so that the additional terms are less than 1
        # and we can exponentiate them safely. We then ignore the base (it's common) and just use extras
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

  # ID variable
  IDColName <- sym(names(Occupants[IDVariable]))

  # Age variable
  AgeColName <- sym(names(Occupants[AgeVariable]))


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

  # ensure that the total number of people to match is divisible by household size

  TestNumber <- nrow(Occupants)

  if (TestNumber %% Occupants != 0) {

    NumExcess <- TestNumber %% Occupants
    Occupants <- Occupants %>%
      slice_sample(n = nrow()-NumExcess, replace = FALSE)

  }

  #
  # DonorCounts <- Donor %>%
  #   group_by_at(DonorAgeVariable) %>%
  #   summarise(AgeCount=n())
  #
  # # DonorAges <- as.vector(DonorCounts[1])
  # DonorAges <- pull(DonorCounts[1])
  # DonorAgeCounts <- pull(DonorCounts[2])
  #
  # # set up bins for iterations
  # # enable at least some extreme age differences to be assigned to the Inf categories
  # # otherwise the bins will be wrong
  #
  # MaxAgeDifference <-  (max(Recipient[RecipientAgeVariable]) -
  #                         min(Donor[DonorAgeVariable]))-5
  #
  # # estimate expected minimum and maximum ages from the distribution, and bin these
  #
  # min_bin <- round(qnorm(0.000001, mean = meanUsed, sd = sdUsed))-0.5
  # max_bin <- round(qnorm(0.999999, mean = meanUsed, sd = sdUsed))+0.5
  # bins <- c(-Inf, min_bin:max_bin, Inf)
  #
  # # construct the probabilities for each bin, gives n(bins)-1
  # Probabilities <- pnorm(bins[-1], mean = meanUsed, sd = sdUsed) -
  #   pnorm(bins[-length(bins)], mean = meanUsed, sd = sdUsed)
  #
  # # assign realistic expected probabilities in the bins outside the bins constructed earlier
  # # use minAge and maxAge for this, only need range for included ages
  # # Uses midpoint rule.
  # logProbLow <- dnorm(-MaxAgeDifference:(min_bin-0.5), mean = meanUsed, sd = sdUsed, log=TRUE)
  # logProbHigh <- dnorm((max_bin+0.5):MaxAgeDifference, mean = meanUsed, sd = sdUsed, log=TRUE)
  #
  # logProb <- c(logProbLow, log(Probabilities[-c(1, length(Probabilities))]), logProbHigh)
  # logBins    <- c(-Inf, -(MaxAgeDifference-.5):(MaxAgeDifference-.5), Inf)
  #
  #
  #
  #
  # #####################################
  # #####################################
  # # end set up
  # #####################################
  # #####################################
  #
  #
  # #####################################
  # #####################################
  # # create initial age matches
  # #####################################
  # #####################################
  # # this is a random sample so age differences will not follow desired distribution
  # # however, if the donor data frame is larger than the recipient data frame
  # # this ensures that a random selection of donors has the correct count
  # if (!is.null(UserSeed)) {
  #   set.seed(UserSeed)
  # }
  #
  #
  # CurrentAgeMatch <- data.frame(Recipient[RecipientIDVariable],
  #                               Recipient[RecipientAgeVariable],
  #                               DonorAge = sample(rep(DonorAges, DonorAgeCounts),
  #                                                 size=nrow(Recipient),
  #                                                 replace = FALSE))
  #
  # # set up for chi-squared test
  # ExpectedAgeProbs <- Probabilities * nrow(CurrentAgeMatch)
  # logEAgeProbs <- logProb + log(nrow(CurrentAgeMatch))
  #
  # # construct starting set of observed age difference values for iteration
  # ObservedAgeDifferences <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,3], breaks = bins, plot=FALSE)$counts
  #
  # # set up for chi-squared
  # log0ObservedAges <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,3], breaks = logBins, plot=FALSE)$counts
  # logKObservedAges = ifelse(log0ObservedAges == 0, 2*logEAgeProbs, log((log0ObservedAges - exp(logEAgeProbs))^2)) - logEAgeProbs
  # log_chisq = max(logKObservedAges) + log(sum(exp(logKObservedAges - max(logKObservedAges))))
  #
  # if (is.null(pValueToStop)) {
  #
  #   Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
  #
  # } else {
  #
  #   Critical_log_chisq <- log(qchisq(pValueToStop, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
  #
  # }
  #
  # #####################################
  # #####################################
  # # iteration for matching couple ages starts here
  # #####################################
  # #####################################
  #
  # for (i in 1:NumIterations) {
  #
  #   # randomly choose two pairs
  #   Pick1 <- sample(nrow(CurrentAgeMatch), 1)
  #   Pick2 <- sample(nrow(CurrentAgeMatch), 1)
  #   Current1 <- CurrentAgeMatch[Pick1,]
  #   Current2 <- CurrentAgeMatch[Pick2,]
  #
  #   # # proposed pairing after a swap
  #   PropPair1 <- swap_donor(Current1, Current2)
  #   PropPair2 <- swap_donor(Current2, Current1)
  #
  #   # compute change in Chi-squared value from current pairing to proposed pairing
  #   PropAgeMatch <- CurrentAgeMatch %>%
  #     filter(!({{RecipientIDColName}} %in% c(PropPair1[,1], PropPair2[,1]))) %>%
  #     bind_rows(., PropPair1,PropPair2)
  #
  #   # do chi-squared
  #   Proplog0 <- hist(PropAgeMatch[,2] - PropAgeMatch[,3], breaks = logBins, plot=FALSE)$counts
  #   ProplogK = ifelse(Proplog0 == 0, 2*logEAgeProbs, log((Proplog0 - exp(logEAgeProbs))^2)) - logEAgeProbs
  #
  #   prop_log_chisq = max(ProplogK) + log(sum(exp(ProplogK - max(ProplogK))))
  #
  #   if (compare_logK(ProplogK, logKObservedAges) < 0) { # we cancel out the bits that haven't changed first.
  #
  #     CurrentAgeMatch[Pick1,] <- PropPair1
  #     CurrentAgeMatch[Pick2,] <- PropPair2
  #
  #
  #     log0ObservedAges <- Proplog0
  #     logKObservedAges <- ProplogK
  #     log_chisq <- prop_log_chisq
  #
  #   }
  #
  #   if (log_chisq <= Critical_log_chisq) {
  #     break
  #
  #   }
  #
  # }
  #
  # #####################################
  # #####################################
  # # iteration for matching couple ages ends here
  # #####################################
  # #####################################
  #
  #
  # #####################################
  # #####################################
  # # pairing the actual couples starts here
  # #####################################
  # #####################################
  # # return full donor and recipient rows as matched household pairs
  # # extract ages counts for matching the donors
  # MatchedDonorAges <- CurrentAgeMatch %>%
  #   dplyr::select(DonorAge) %>%
  #   group_by(DonorAge) %>%
  #   mutate(DonorAgeCount = row_number()) %>%
  #   ungroup()
  #
  #
  # #   # generate same AgeCount second ID variable for the donor data
  # #   # the AgeCount is used to ensure that the first donor with a specific age is matched first
  # #   # the second donor with a specific age is matched second
  # #   # and so forth
  # DonorsToMatch <- Donor %>%
  #   group_by({{DonorAgeColName}}) %>%
  #   mutate(DonorAgeCount = row_number()) %>%
  #   ungroup()
  #
  # # reduce pool of potentially partnered donors to only those matched to recipients
  # DonorsMatched <- left_join(MatchedDonorAges,
  #                            rename_at(DonorsToMatch, DonorAgeVariable, ~ names(MatchedDonorAges)[1]),
  #                            by = c(names(MatchedDonorAges)[1], "DonorAgeCount")) %>%
  #   mutate(!!DonorAgeColName := DonorAge)
  #
  #
  # # construct same file for the recipients
  # # need both donor age and donor age count so that the join between the recipients and the donors works
  # # do not need Recipient age as this will be a duplicate column on the merge
  # RecipientsMatchPrep <- CurrentAgeMatch %>%
  #   group_by(DonorAge) %>%
  #   mutate(DonorAgeCount = row_number()) %>%
  #   dplyr::select(-c(2))
  #
  # #
  # RecipientsReadyToMatch <- left_join(Recipient, RecipientsMatchPrep, by = names(Recipient[RecipientIDVariable]))
  #
  # # now merge the full data of the subset donors to the recipients
  # # by donor age and donor age count
  # # recipient data frame is the one to which observations must be joined
  # # also add the household numbers at this point
  # MaxCoupleIDValue <- (nrow(RecipientsReadyToMatch)-1) + CoupleIDValue
  #
  # FullMatchedDataFrame <- left_join(RecipientsReadyToMatch, DonorsMatched, by=c("DonorAge", "DonorAgeCount")) %>%
  #   dplyr::select(-DonorAge, -DonorAgeCount) %>%
  #   ungroup() %>%
  #   mutate({{HouseholdNumVariable}} := seq(CoupleIDValue, MaxCoupleIDValue))
  #
  # # convert from wide to long, use .x and .y to do the split
  #
  # FirstDataframeSplit <- FullMatchedDataFrame %>%
  #   dplyr::select(ends_with(".x"), {{HouseholdNumVariable}}) %>%
  #   rename_all(list(~gsub("\\.x$", "", .)))
  #
  # SecondDataframeSplit <- FullMatchedDataFrame %>%
  #   dplyr::select(ends_with(".y"), {{HouseholdNumVariable}}) %>%
  #   rename_all(list(~gsub("\\.y$", "", .)))
  #
  #
  # OutputDataframe <- rbind(FirstDataframeSplit, SecondDataframeSplit)
  # #
  # # #####################################
  # # #####################################
  # # # pairing the actual couples ends here
  # # #####################################
  # # #####################################
  #
  # # use for checking number of iterations used, the p-value to stop, and the p-value reached
  # #
  # # print(i)
  # # print(Critical_log_chisq)
  # # print(log_chisq)

  return(Occupants)


}
