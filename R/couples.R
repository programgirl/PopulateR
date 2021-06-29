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
#' @param RecipientIDCol The column number for the ID variable in the Recipient data frame.
#' @param RecipientAgeCol The column number for the age variable in the Recipient data frame.
#' @param Donor A data frame containing observations limited to one sex. An age column is required. Only include the ages that will be allocated to partners.
#' @param DonorIDCol The column number for the donor ID. Must be numeric.
#' @param DonorAgeCol The column number for the age variable in the Donor data frame.
#' @param meanUsed The mean value for the  normal distribution.
#' @param sdUsed The standard deviation value for the normal distribution.
#' @param IDStartValue The starting number for generating a variable that identifies the observations in a couple. Must be numeric.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied in quotes.
#' @param pValueToStop The primary stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' #' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param NumIterations The maximum number of iterations used to construct the coupled data frame. This has a default value of 1000000, and is the stopping rule
#' if the algorithm does not converge.

#'
#' @return A data frame of an even number of observations that have been allocated into opposite-sex couples.
#'
#' @examples
#' Recipients <- data.frame(cbind(PersonID = c(1:1000),
#'                               PersonAge = c(round(runif(200, min=18, max=23),0), round(runif(300, min=24, max=50),0), round(runif(500, min=51, max=90),0))))
#'
#' Donors <- data.frame(cbind(PersonID = c(2001:4000),
#'                               PersonAge = c(round(runif(400, min=18, max=23),0), round(runif(500, min=24, max=50),0), round(runif(1100, min=51, max=90),0))))
#'
#' ExampleOutput <- OppositeSex(Recipients, RecipientIDCol=1, RecipientAgeCol=2, Donors, DonorIDCol=1, DonorAgeCol=2, meanUsed=2, sdUsed=4, IDStartValue = 10001, HouseholdNumVariable="TheHouseholds", UserSeed=NULL, pValueToStop=.001, NumIterations=1000)


couples <- function(Recipient, RecipientIDCol=NULL, RecipientAgeCol=NULL, Donor, DonorIDCol=NULL,
                    DonorAgeCol=NULL, DirectXi=NULL, DirectOmega=NULL, AlphaUsed=0, IDStartValue = NULL,
                    HouseholdNumVariable=NULL, UserSeed=NULL, pValueToStop=NULL, NumIterations=1000000) {

  # content check
  if (!any(duplicated(Recipient[RecipientIDCol])) == FALSE) {
    stop("The column number for the ID variable in the recipient data frame must be supplied.")
  }

  if (!is.numeric(RecipientAgeCol)) {
    stop("Both the Recipient ID and the Recipient age column numbers must be supplied.")
  }

  if (!any(duplicated(Donor[DonorIDCol])) == FALSE) {
    stop("The column number for the ID variable in the donor data frame must be supplied.")
  }

  if(is.null(HouseholdNumVariable)) {
    stop("A name for the household count variable must be supplied.")
  }

  if(is.null(DirectOmega) | DirectOmega < 0) {
    stop("The mean age difference must be greater than zero.")
  }

  #####################################
  #####################################
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
  RecipientIDColName <- sym(names(Recipient[RecipientIDCol]))
  # Recipient age variable
  RecipientAgeColName <- sym(names(Recipient[RecipientAgeCol]))

  # Donor ID variable
  DonorIDColName <- sym(names(Donor[DonorIDCol]))
  # Donor age variable
  DonorAgeColName <- sym(names(Donor[DonorAgeCol]))

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
    group_by_at(DonorAgeCol) %>%
    summarise(AgeCount=n())

  # DonorAges <- as.vector(DonorCounts[1])
  DonorAges <- pull(DonorCounts[1])
  DonorAgeCounts <- pull(DonorCounts[2])

  # set up bins for iterations
  # enable at least some extreme age differences to be assigned to the Inf categories
  # otherwise the bins will be wrong

  MaxAgeDifference <-  (max(Recipient[RecipientAgeCol]) -
                          min(Donor[DonorAgeCol]))-5

  cat("Starting the bins", "\n")

  # estimate expected minimum and maximum ages from the distribution, and bin these
  min_bin <- round(sn::qsn(0.000001,xi=DirectXi, omega=DirectOmega, alpha=AlphaUsed))-0.5
  max_bin <- round(sn::qsn(0.999999,xi=DirectXi, omega=DirectOmega, alpha=AlphaUsed))+0.5

  cat("Error when trying to bin", "\n")

  bins <- c(-9999, min_bin:max_bin, 9999)

  cat("Error after making the bins", "\n")

  # construct the probabilities for each bin, gives n(bins)-1
  Probabilities <- sn::psn(bins[-1], xi=DirectXi, omega=DirectOmega, alpha=AlphaUsed) -
    sn::psn(bins[-length(bins)], xi=DirectXi, omega=DirectOmega, alpha=AlphaUsed)

  cat("Error after making the probabilities", "\n")

  # assign realistic expected probabilities in the bins outside the bins constructed earlier
  # use minAge and maxAge for this, only need range for included ages
  # Uses midpoint rule.
  logProbLow <- sn::dsn(-MaxAgeDifference:(min_bin-0.5), xi=DirectXi, omega=DirectOmega, alpha=AlphaUsed, log=TRUE)
  logProbHigh <- sn::dsn((max_bin+0.5):MaxAgeDifference, xi=DirectXi, omega=DirectOmega, alpha=AlphaUsed, log=TRUE)

  logProb <- c(logProbLow, log(Probabilities[-c(1, length(Probabilities))]), logProbHigh)
  logBins    <- c(-Inf, -(MaxAgeDifference-.5):(MaxAgeDifference-.5), Inf)

  cat("Error after making the logProb and logBins", "\n")

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


  CurrentAgeMatch <- data.frame(Recipient[RecipientIDCol],
                                Recipient[RecipientAgeCol],
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
    dplyr::select(DonorAge) %>%
    group_by(DonorAge) %>%
    mutate(DonorAgeCount = row_number()) %>%
    ungroup()


  #   # generate same AgeCount second ID variable for the donor data
  #   # the AgeCount is used to ensure that the first donor with a specific age is matched first
  #   # the second donor with a specific age is matched second
  #   # and so forth
  DonorsToMatch <- Donor %>%
    group_by({{DonorAgeColName}}) %>%
    mutate(DonorAgeCount = row_number()) %>%
    ungroup()

  # reduce pool of potentially partnered donors to only those matched to recipients
  DonorsMatched <- left_join(MatchedDonorAges,
                             rename_at(DonorsToMatch, DonorAgeCol, ~ names(MatchedDonorAges)[1]),
                             by = c(names(MatchedDonorAges)[1], "DonorAgeCount")) %>%
    mutate(!!DonorAgeColName := DonorAge)


  # construct same file for the recipients
  # need both donor age and donor age count so that the join between the recipients and the donors works
  # do not need Recipient age as this will be a duplicate column on the merge
  RecipientsMatchPrep <- CurrentAgeMatch %>%
    group_by(DonorAge) %>%
    mutate(DonorAgeCount = row_number()) %>%
    dplyr::select(-c(2))

  #
  RecipientsReadyToMatch <- left_join(Recipient, RecipientsMatchPrep, by = names(Recipient[RecipientIDCol]))

  # now merge the full data of the subset donors to the recipients
  # by donor age and donor age count
  # recipient data frame is the one to which observations must be joined
  # also add the household numbers at this point
  MaxIDStartValue <- (nrow(RecipientsReadyToMatch)-1) + IDStartValue

  FullMatchedDataFrame <- left_join(RecipientsReadyToMatch, DonorsMatched, by=c("DonorAge", "DonorAgeCount")) %>%
    dplyr::select(-DonorAge, -DonorAgeCount) %>%
    ungroup() %>%
    mutate({{HouseholdNumVariable}} := seq(IDStartValue, MaxIDStartValue))

  # convert from wide to long, use .x and .y to do the split

  FirstDataframeSplit <- FullMatchedDataFrame %>%
    dplyr::select(ends_with(".x"), {{HouseholdNumVariable}}) %>%
    rename_all(list(~gsub("\\.x$", "", .)))

  SecondDataframeSplit <- FullMatchedDataFrame %>%
    dplyr::select(ends_with(".y"), {{HouseholdNumVariable}}) %>%
    rename_all(list(~gsub("\\.y$", "", .)))


  OutputDataframe <- rbind(FirstDataframeSplit, SecondDataframeSplit)


  # #####################################
  # #####################################
  # # pairing the actual couples ends here
  # #####################################
  # #####################################

  # use for checking number of iterations used, the p-value to stop, and the p-value reached
  #
  print(i)
  # print(Critical_log_chisq)
  # print(log_chisq)

  if(nrow(Donor) > nrow(Recipient)) {

    cat("The donor dataframe contained more observations than the recipient dataframe.", "\n")
    cat("A merged dataframe has been returned as a list.", "\n")
    cat("The individual dataframes are $Matched and $Unmatched.", "\n")

    MatchedIDs <- OutputDataframe %>%
      pull({{DonorIDColName}})

    UnmatchedDataframe <- Donor %>%
      filter(!({{DonorIDColName}} %in% MatchedIDs)) #%>%
     # mutate({{HouseholdNumVariable}} = NA)

    MergedList <- list()

    MergedList$Matched <- OutputDataframe
    MergedList$Unmatched <- UnmatchedDataframe

    return(MergedList)

    # closes test for non-matches and returns the merged data frame if there are non-matches
  } else {

    # if donor has the same number of rows as recipient, returns everything as the dataframe specified on the input.
    return(OutputDataframe)

     }


}
