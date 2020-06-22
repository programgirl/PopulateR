#' @param ChildDataframe A data frame containing observations limited to the children to be matched An age column is required. All children in this data frame will be matched to a parent/guardian.
#' @param ChildIDVariable The column number for the ID variable in the ChildDataFrame.
#' @param ChildAgeVariable The column number for the Age variable in the ChildDataFrame.
#' @param meanlogUsed The mean of the natural log for the distribution of mother ages at childbirth.
#'  @param sdlogUsed The standard deviation of the natural log for the distribution of mother ages at childbirth.
#' @param MotherDataframe A data frame containing observations limited to mothers.An age column is required. This can contain the entire set of women who can be mothers, as the assignment is made on age at birth, not current age.
#' @param MotherIDVariable The column number for the ID variable in the Recipient data frame.
#' @param MotherAgeVariable The column number for the Age variable in the Mother data frame.
#' @param MinMotherAge The youngest age at which a woman gives birth.
#' @param MaxMotherAge The oldest age at which a woman gives birth.
#' @param PropMothers The proportion of children who are assigned to mothers, the remainder will be assigned to fathers
#' @param MinPropRemain The minimum proportion of people, at each age, who are not parents
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied in quotes.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param pValueToStop = The primary stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param NumIterations The maximum number of iterations used to construct the coupled data frame. This has a default value of 1000000, and is the stopping rule
#' if the algorithm does not converge.

AddChldrnln <- function(ChildDataframe, ChildIDVariable, ChildAgeVariable, meanlogUsed, sdlogUsed, MotherDataframe, MotherIDVariable,
                          MotherAgeVariable, MinMotherAge, MaxMotherAge, PropMothers, MinPropRemain,HouseholdNumVariable, UserSeed=NULL,
                          pValueToStop = .01, NumIterations = 1000000)
  {

  options(dplyr.summarise.inform=F)

    # content check
    if (!any(duplicated(ChildDataframe[ChildIDVariable])) == FALSE) {
      stop("The column number for the ID variable in the child data frame must be supplied.")
    }

    if (!is.numeric(ChildAgeVariable)) {
      stop("Both the child ID and the child age column numbers must be supplied.")
    }

    if (!any(duplicated(MotherDataframe[MotherIDVariable])) == FALSE) {
      stop("The column number for the ID variable in the mother data frame must be supplied.")
    }

    if(is.null(HouseholdNumVariable)) {
      stop("A name for the household count variable must be supplied.")
    }

    #####################################
    #####################################
    # sub functions are here
    #####################################
    #####################################
    # pairing swap subfunction

   # for mothers

    swap_mother <- function(pair1, pair2) {
      swap <- pair1
      swap$MotherID <- pair2$MotherID
      swap$MotherAge <- pair2$MotherAge
      return(swap)
    }

    # for fathers

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

    # Child ID variable
    ChildIDColName <- sym(names(ChildDataframe[ChildIDVariable]))

    # Child age variable
    ChildAgeColName <- sym(names(ChildDataframe[ChildAgeVariable]))

    # Mother age variable
    MotherAgeColName <- sym(names(MotherDataframe[MotherAgeVariable]))

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

    # get counts for each single age from the mother data frame
    MotherCounts <- MotherDataframe %>%
      group_by_at(MotherAgeVariable) %>%
      summarise(AgeCount=n())

    # MotherAges <- as.vector(MotherCounts[1])
    MotherAges <- pull(MotherCounts[1])
    MotherAgeCounts <- pull(MotherCounts[2])

    # set up bins for iterations
    # enable at least some extreme age differences to be assigned to the Inf categories
    # otherwise the bins will be wrong

    # estimate expected minimum and maximum ages from the distribution, and bin these

    min_bin <- round(qlnorm(0.000001, meanlog = meanlogUsed, sdlog = sdlogUsed))-0.5
    max_bin <- round(qlnorm(0.999999, meanlog = meanlogUsed, sdlog = sdlogUsed))+0.5
    bins <- c(-Inf, min_bin:max_bin, Inf)

    # # construct the probabilities for each bin, gives n(bins)-1
    Probabilities <- plnorm(bins[-1], meanlog = meanlogUsed, sdlog = sdlogUsed) -
      plnorm(bins[-length(bins)], meanlog = meanlogUsed, sdlog = sdlogUsed)


    # assign realistic expected probabilities in the bins outside the bins constructed earlier
    # use minAge and maxAge for this, only need range for included ages
    # Uses midpoint rule.
    logProbLow <- dlnorm(MinMotherAge:(min_bin-0.5), meanlog = meanlogUsed, sdlog = sdlogUsed, log=TRUE)
    logProbHigh <- dlnorm((max_bin+0.5):MaxMotherAge, meanlog = meanlogUsed, sdlog = sdlogUsed, log=TRUE)



    logProb <- c(logProbLow, log(Probabilities[-c(1, length(Probabilities))]), logProbHigh)
    logBins    <- c(-Inf, -(MinMotherAge-.5):(MaxMotherAge-.5), Inf)


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
    if (!is.null(UserSeed)) {
      set.seed(UserSeed)
    }
    #
    # generate the random subset of children to match to mothers

    ChildToMother <- ChildDataframe %>%
      sample_n(round(PropMothers*nrow(ChildDataframe),0))



    CurrentAgeMatch <- data.frame(ChildToMother[ChildIDVariable],
                                  ChildToMother[ChildAgeVariable],
                                  MotherAge = sample(rep(MotherAges, MotherAgeCounts),
                                                    size=nrow(ChildToMother),
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
    # iteration for matching child to mother ages starts here
    #####################################
    #####################################
  ChiSquareValuesCreated <- data.frame()

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
        filter(!({{ChildDataframe}} %in% c(PropPair1[,1], PropPair2[,1]))) %>%
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

      log_chisq

      ChiSquareValuesCreated <- c(ChiSquareValuesCreated, log_chisq)

      if (log_chisq <= Critical_log_chisq) {
        break

      }

    }

    return(ChiSquareValuesCreated)
  #   #
  #   #####################################
  #   #####################################
  #   # iteration for matching mother-child ages ends here
  #   #####################################
  #   #####################################
  #
  #
  #   #####################################
  #   #####################################
  #   # pairing the actual mother-child dyads starts here
  #   #####################################
  #   #####################################
  #   # return full donor and recipient rows as matched household pairs
  #   # extract ages counts for matching the donors
  #   MatchedMotherAges <- CurrentAgeMatch %>%
  #     select(MotherAge) %>%
  #     group_by(MotherAge) %>%
  #     mutate(MotherAgeCount = row_number()) %>%
  #     ungroup()
  #
  #
  #   # generate same AgeCount second ID variable for the donor data
  #   # the AgeCount is used to ensure that the first donor with a specific age is matched first
  #   # the second donor with a specific age is matched second
  #   # and so forth
  #   MothersToMatch <- Mother %>%
  #     group_by({{MotherAgeColName}}) %>%
  #     mutate(MotherAgeCount = row_number()) %>%
  #     ungroup()
  #
  #   # reduce pool of potentially partnered donors to only those matched to recipients
  #   MothersMatched <- left_join(MatchedMotherAges, rename_at(MothersToMatch, MotherAgeVariable, ~ names(MatchedMotherAges)[1]), by = names(MatchedMotherAges)[1]) %>%
  #     mutate(!!MotherAgeColName := MotherAge)
  #
  #
  #
  #   # construct same file for the children
  #   # need both mother age and mother age count so that the join between the children and the mothers works
  #   # do not need child age as this will be a duplicate column on the merge
  #   ChildrenMatchPrep <- CurrentAgeMatch %>%
  #     group_by(ChildAgeVariable) %>%
  #     mutate(ChildAgeCount = row_number()) %>%
  #     select(-c(2))


    # RecipientsReadyToMatch <- left_join(Recipient, RecipientsMatchPrep, by = c(names(Recipient[RecipientIDVariable]), names(RecipientsMatchPrep[1])))
#
#     RecipientsReadyToMatch <- left_join(Recipient, RecipientsMatchPrep, by = c(names(Recipient[RecipientIDVariable])))

    #
    #
    #
    #
    #
    #
    # # now merge the full data of the subset donors to the recipients
    # # by donor age and donor age count
    # # recipient data frame is the one to which observations must be joined
    # # also add the household numbers at this point
    # MaxCoupleIDValue <- (nrow(RecipientsReadyToMatch)-1) + CoupleIDValue
    #
    # FullMatchedDataFrame <- left_join(RecipientsReadyToMatch, DonorsMatched, by=c("DonorAge", "DonorAgeCount")) %>%
    #   select(-DonorAge, -DonorAgeCount) %>%
    #   ungroup() %>%
    #   mutate({{HouseholdNumVariable}} := seq(CoupleIDValue, MaxCoupleIDValue))
    #
    # # convert from wide to long, use .x and .y to do the split
    #
    # FirstDataframeSplit <- FullMatchedDataFrame %>%
    #   select(ends_with(".x"), {{HouseholdNumVariable}}) %>%
    #   rename_all(list(~gsub("\\.x$", "", .)))
    #
    # SecondDataframeSplit <- FullMatchedDataFrame %>%
    #   select(ends_with(".y"), {{HouseholdNumVariable}}) %>%
    #   rename_all(list(~gsub("\\.y$", "", .)))
    #
    #
    # OutputDataframe <- rbind(FirstDataframeSplit, SecondDataframeSplit)
    #
    # #####################################
    # #####################################
    # # pairing the actual couples ends here
    # #####################################
    # #####################################
    # print(i)
    #
    # return(OutputDataframe)

  }
