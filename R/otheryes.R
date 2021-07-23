#' Create a match of people into households
#' This function creates a data frame of household inhabitants, with the specified number of inhabitants.
#' One data frame, containing the people to match, is required. The use of an age distribution for the matching ensures that an age structure is present in the households. A less correlated age structure can be produced by entering a larger standard deviation.
#' The output data frame of matches will only contain households of the required size. If the number of rows in the people data frame is not divisible by household size, the overcount will be output to a separate data frame.
#'
#' @export
#' @param existing A data frame containing the people already in households.
#' @param exsidcol The column number for the ID variable, for people in the existing data frame.
#' @param exsagecol The column number for the Age variable, for people in the existing data frame.
#' @param additions A data frame containing the people to be added to the existing households.
#' @param addidcol The column number for the ID variable, for people to be added to the existing households.
#' @param addagecol The column number for the Age variable, for people in the existing data frame.
#' @param numppl The number of people to be added to the household.
#' @param sdused The standard deviation of the normal distribution for the distribution of ages in a household.
#' @param hhidstart The starting number for generating the household identifier value for showing unique households. Must be numeric.
#' @param hhidvar The column name for the household variable. This must be supplied, and in quotes.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param ptostop = The primary stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param numiters The maximum number of iterations used to construct the household data frame. This has a default value of 1000000, and is the stopping rule if the algorithm does not converge.
#'
#' @return A list of three data frames $Matched contains the data frame of households containing matched people. All households will be of the specified size. $Existing, if populated, contains the excess people in the existing data frame, who could not be allocated additional people. $Additions, if populated, contains the excess people in the additions data frame who could not be allocated to an existing household.

otheryes <- function(existing, exsidcol, exsagecol, hhidcol = NULL, additions, addidcol, addagecol,
                     numppl = NULL, sdused, userseed=NULL, ptostop = .01, numiters = 1000000)
{

  options(dplyr.summarise.inform=F)

  # content check
  if (is.null(numppl)) {
    stop("The household size must be supplied.")
  }

  if (!any(duplicated(existing[hhidcol])) == FALSE) {
    stop("The column number for the household ID variable in the 'existing' data frame must be supplied, and the household number must be unique to each person")
  }

  if (!any(duplicated(existing[exsidcol])) == FALSE) {
    stop("The column number for the ID variable in the parent data frame must be supplied, and the ID must be unique to each parent.")
  }

  if (!any(duplicated(additions[addidcol])) == FALSE) {
    stop("The column number for the ID variable in the 'additions' data frame must be supplied, and the ID must be unique to each person")
  }


  #######################
  # set up user seed

  if (!is.null(userseed)) {
    set.seed(userseed)
  }


  #####################################
  #####################################
  # sub functions are here
  #####################################
  #####################################

  # pairing swap subfunction

  swap_household_matches <- function(pair1, pair2) {
    swap <- pair1
    swap$MatchedID <- pair2$MatchedID
    swap$MatchedAge <- pair2$MatchedAge
    return(swap)
  }

  # set up bins and variables for matching
  # set up bins for iterations
  # enable at least some extreme age differences to be assigned to the Inf categories
  # otherwise the bins will be wrong

  MaxAgeDifference <-  (max(existing[exsagecol]) -
                          min(existing[exsagecol]))-5

  # estimate expected minimum and maximum ages from the distribution, and bin these

  min_bin <- round(qnorm(0.000001, mean = 0, sd = sdused))-0.5
  max_bin <- round(qnorm(0.999999, mean = 0, sd = sdused))+0.5
  bins <- c(-Inf, min_bin:max_bin, Inf)

  # construct the probabilities for each bin, gives n(bins)-1
  Probabilities <- pnorm(bins[-1], mean = 0, sd = sdused) -
    pnorm(bins[-length(bins)], mean = 0, sd = sdused)

  # assign realistic expected probabilities in the bins outside the bins constructed earlier
  # use minAge and maxAge for this, only need range for included ages
  # Uses midpoint rule.
  logProbLow <- dnorm(-MaxAgeDifference:(min_bin-0.5), mean = 0, sd = sdused, log=TRUE)
  logProbHigh <- dnorm((max_bin+0.5):MaxAgeDifference, mean = 0, sd = sdused, log=TRUE)

  logProb <- c(logProbLow, log(Probabilities[-c(1, length(Probabilities))]), logProbHigh)
  logBins    <- c(-Inf, -(MaxAgeDifference-.5):(MaxAgeDifference-.5), Inf)

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

  ####################################
  ####################################
  # sub functions end
  ####################################
  ####################################

  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################


  existingRenamed <- existing %>%
    rename(existID = !! exsidcol, existAge = !! exsagecol,
           HouseholdID = !! hhidcol)


  additionsRenamed <- additions %>%
    rename(addID = !! addidcol, addAge = !! addagecol)

  # variable names for existing data frame
  existsIDColName <- sym(names(existing[exsidcol]))

  existsAgeColName <- sym(names(existing[exsagecol]))

  existsHouseholdColName <- sym(names(existing[hhidcol]))

  # variable names for the household additions
  addsidcolName <- sym(names(additions[addidcol]))

  addsagecolName <- sym(names(additions[addagecol]))

  # need column count for turning wide dataframe into long, uses the existing data frame
  NumberColsexistingPlusTwo <- as.numeric(ncol(existing))+2

  #####################################
  #####################################
  # end column names
  #####################################
  #####################################

  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  #####################################
  #####################################

  # work out data frame compatibility
  # the additions data frame may be too small or too large based on the size of the
  # existing count and the number of people to add

  # get the length of the existing data frame

  NumExisting <- nrow(existingRenamed)

  NumNeeded <- NumExisting * numppl

  NumProvided <- nrow(additionsRenamed)

  # cat("Number existing is", NumExisting, "number needed is", NumNeeded, "number provided is", NumProvided, "\n")

  if((NumNeeded > NumProvided) == TRUE) {

    cat("The additions data frame should contain", NumNeeded, "people but only contains", NumProvided, "\n")

    NumCanUse <- floor(NumProvided / numppl)

    cat(NumCanUse, "will be randomly sampled from the", NumExisting, "people already in households", "\n")

    existingRenamed <- existingRenamed %>%
      slice_sample(n = NumCanUse)
  }

  if((NumProvided > NumNeeded) == TRUE) {

    cat("The additions data frame should contain", NumNeeded, "people and contains", NumProvided, "\n")

    cat(NumNeeded, "will be randomly sampled from the", NumProvided, "people to add to households", "\n")

    additionsRenamed <- additionsRenamed %>%
      slice_sample(n = NumNeeded)

  }


  #####################################
  # matching
  #####################################


  BaseSize <- (nrow(existingRenamed))

  Remainingadditions <- additionsRenamed

  # cat("Remainingexisting is", nrow(Remainingexisting), "rows", "\n")

  for (i in 1: numppl) {

    # if(BaseSize < 1) {
    #   stop("Sample size is less than 1", "\n")
    # }

    MatchingSample <- Remainingadditions %>%
      slice_sample(n = BaseSize)

    #    cat("MatchingSample size is", nrow(MatchingSample), "\n")

    Remainingadditions <- Remainingadditions %>%
      filter(!(addID %in% MatchingSample$addID))

    # cat("workingsexdataframe is", nrow(WorkingSexDataFrame), "rows", "\n")

    # get age differences

    CurrentAgeMatch <- existingRenamed %>%
      select(existAge, existID)

    MatchedAgeExtract <- MatchingSample %>%
      select(addAge, addID)

    CurrentAgeMatch <- cbind(CurrentAgeMatch, MatchedAgeExtract)


    # cat("Current age match is", nrow(CurrentAgeMatch), "Matched age extract is",
    #     nrow(MatchedAgeExtract), "combined age match is", nrow(CurrentAgeMatch), "\n")

    ExpectedAgeProbs <- Probabilities * nrow(CurrentAgeMatch)
    logEAgeProbs <- logProb + log(nrow(CurrentAgeMatch))

    ObservedAgeDifferences <- hist(CurrentAgeMatch[,1] - CurrentAgeMatch[,3],
                                   breaks = bins, plot=FALSE)$counts


    # set up for chi-squared
    log0ObservedAges <- hist(CurrentAgeMatch[,1] - CurrentAgeMatch[,3],
                             breaks = logBins, plot=FALSE)$counts
    logKObservedAges = ifelse(log0ObservedAges == 0, 2*logEAgeProbs,
                              log((log0ObservedAges - exp(logEAgeProbs))^2)) - logEAgeProbs
    log_chisq = max(logKObservedAges) + log(sum(exp(logKObservedAges - max(logKObservedAges))))

    # print(log_chisq)


    if (is.null(ptostop)) {

      Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))

    } else {

      Critical_log_chisq <- log(qchisq(ptostop, df=(length(logEAgeProbs-1)), lower.tail = TRUE))

      # closes p-value stopping rule
    }

    # print(Critical_log_chisq)

    #####################################
    #####################################
    # iteration for matching  ages starts here
    #####################################
    #####################################

    # cat("Gets to matching age iterations", "\n")


    for (j in 1:numiters) {

      # print(i)

      # randomly choose two pairs
      Pick1 <- sample(nrow(CurrentAgeMatch), 1)
      Pick2 <- sample(nrow(CurrentAgeMatch), 1)
      Current1 <- CurrentAgeMatch[Pick1,]
      Current2 <- CurrentAgeMatch[Pick2,]

      # # proposed pairing after a swap
      PropPair1 <- swap_household_matches(Current1, Current2)
      PropPair2 <- swap_household_matches(Current2, Current1)

      # compute change in Chi-squared value from current pairing to proposed pairing
      PropAgeMatch <- CurrentAgeMatch %>%
        filter(!(existID %in% c(PropPair1[,2], PropPair2[,2]))) %>%
        bind_rows(., PropPair1,PropPair2)

      # cat("PropAgeMatch has", nrow(PropAgeMatch), "rows", "\n")

      # do chi-squared
      Proplog0 <- hist(PropAgeMatch[,1] - PropAgeMatch[,3], breaks = logBins, plot=FALSE)$counts
      ProplogK = ifelse(Proplog0 == 0, 2*logEAgeProbs,
                        log((Proplog0 - exp(logEAgeProbs))^2)) - logEAgeProbs

      prop_log_chisq = max(ProplogK) + log(sum(exp(ProplogK - max(ProplogK))))

      if (compare_logK(ProplogK, logKObservedAges) < 0) {

        # cat("Loop entered", prop_log_chisq, "\n")

        CurrentAgeMatch[Pick1,] <- PropPair1
        CurrentAgeMatch[Pick2,] <- PropPair2


        log0ObservedAges <- Proplog0
        logKObservedAges <- ProplogK
        log_chisq <- prop_log_chisq

        # closes pair swqp

      }

      # cat("log chi-square is", log_chisq, "\n")

      if (log_chisq <= Critical_log_chisq) {
        break

        cat("Break due to critical p-value reached", "\n")
      }

      # closes iterations through the age matching
    }

    # cat("Gets out of loop", "\n")

    if(exists("TheMatched")) {

      InterimDataFrame <- existingRenamed %>%
        left_join(CurrentAgeMatch, by=c("existID", "existAge")) %>%
        left_join(MatchingSample, by = "addID") %>%
        select(HouseholdID, all_of(NumberColsexistingPlusTwo:ncol(.)))

      TheMatched <- bind_rows(TheMatched, InterimDataFrame)

    } else {

      TheMatched <- existingRenamed %>%
        left_join(CurrentAgeMatch, by=c("existID", "existAge")) %>%
        left_join(MatchingSample, by = "addID") %>%
        select(HouseholdID, all_of(NumberColsexistingPlusTwo:ncol(.)))

    }

    # closes the loop through the number of sets of existing to match,
    # e.g. 1 set for two-person households, 2 sets for three-person households
  }
  # correct the names of the variables in the interim and base data frames
  # row bind these and output

  TheMatched <- TheMatched %>%
    rename_all(list(~gsub("\\.y$", "", .))) %>%
    mutate({{existsIDColName}} := addID,
           {{existsAgeColName}} := addAge,
           {{existsHouseholdColName}} := HouseholdID) %>%
    select(-c(addID, addAge))

  TheOriginal <- existingRenamed %>%
    mutate({{existsIDColName}} := existID,
           {{existsAgeColName}} := existAge,
           {{existsHouseholdColName}} := HouseholdID) %>%
    select(-c(existID, existAge))

  OutputDataframe <- bind_rows(TheOriginal, TheMatched)

  cat("The individual dataframes are $Matched, $Existing, and $Additions", "\n")

  MatchedIDs <- OutputDataframe %>%
    pull({{existsIDColName}})

  UnmatchedExisting <- existing %>%
    filter(!({{existsIDColName}} %in% MatchedIDs))

  UnmatchedAdditions <- additions %>%
    filter(!({{addsidcolName}} %in% MatchedIDs))

  MergedList <- list()

  MergedList$Matched <- OutputDataframe
  MergedList$Existing <- UnmatchedExisting
  MergedList$Additions <- UnmatchedAdditions

  return(MergedList)

}

