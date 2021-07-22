#' Create a match of people into households
#' This function creates a data frame of household inhabitants, with the specified number of inhabitants.
#' One data frame, containing the people to match, is required. The use of an age distribution for the matching ensures that an age structure is present in the households. A less correlated age structure can be produced by entering a larger standard deviation.
#' The output data frame of matches will only contain households of the required size. If the number of rows in the people data frame is not divisible by household size, the overcount will be output to a separate data frame.
#'
#' @export
#' @param existing A data frame containing the people already in households.
#' @param exsidcol The column number for the ID variable, for people in the existing data frame.
#' @param exsagecol The column number for the Age variable, for people in the existing data frame.
#' @param numppl The household size to be constructed.
#' @param sdused The standard deviation of the normal distribution for the distribution of ages in a household.
#' @param hhidstart The starting number for generating the household identifier value for showing unique households. Must be numeric.
#' @param hhidvar The column name for the household variable. This must be supplied, and in quotes.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param ptostop = The primary stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param numiters The maximum number of iterations used to construct the household data frame. This has a default value of 1000000, and is the stopping rule if the algorithm does not converge.
#'
#' @return A list of two data frames $Matched contains the data frame of households containing matched people. All households will be of the specified size. $Unmatched, if populated, contains the people that were not allocated to households. If the number of rows in the people data frame is divisible by the household size required, $Unmatched will be an empty data frame.

otheryes <- function(existing, exsidcol, exsagecol, additions, addidcol, addagecol, numppl = NULL,
                     sdused, hhidcol = NULL, userseed=NULL, ptostop = .01, numiters = 1000000
)
{

  options(dplyr.summarise.inform=F)

  # content check
  if (!any(duplicated(existing[exsidcol])) == FALSE) {
    stop("The column number for the ID variable in the data frame must be supplied.")
  }

  if (!is.numeric(exsagecol)) {
    stop("Both the existing ID and the age column numbers must be supplied.")
  }

  if (is.null(numppl)) {
    stop("The household size must be supplied.")
  }

  if (is.null(hhidvar)) {
    stop("A name for the household count variable must be supplied.")
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

  # ID variable
  IDColName <- sym(names(existing[exsidcol]))

  # Age variable
  AgeColName <- sym(names(existing[exsagecol]))

  # need column count for turning wide dataframe into long
  NumberColsexistingPlusOne <- as.numeric(ncol(existing))+1

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

  existingRenamed <- existing %>%
    rename(RenamedID = !! exsidcol, RenamedAge = !! exsagecol)

  #####################################
  #####################################

  # fix dataset to be a factor of household size

  ModuloDF <- nrow(existingRenamed) %% numppl


  # test if each Sex is divisible by household size
  if(ModuloDF > 0) {

    SampleSizeUsed <- nrow(existingRenamed) - ModuloDF

    existingRenamed <- existingRenamed %>%
      slice_sample(n = SampleSizeUsed)

    # closes if(ModuloDF > 0)
  }

  #####################################
  # matching
  #####################################


  BaseSize <- nrow(existingRenamed)/numppl

  Baseexisting <- existingRenamed %>%
    slice_sample(n = BaseSize) %>%
    mutate({{hhidvar}} := seq(hhidstart, (hhidstart + BaseSize - 1)))

  Remainingexisting <- existingRenamed %>%
    filter(!(RenamedID %in% c(Baseexisting$RenamedID)))


  # cat("Remainingexisting is", nrow(Remainingexisting), "rows", "\n")

  while(!(is.na(Remainingexisting$RenamedAge[1])) == TRUE) {

    if(BaseSize < 1) {
      stop("Sample size is less than 1", "\n")
    }

    MatchingSample <- Remainingexisting %>%
      slice_sample(n = BaseSize)

    #    cat("MatchingSample size is", nrow(MatchingSample), "\n")

    Remainingexisting <- Remainingexisting %>%
      filter(!(RenamedID %in% MatchingSample$RenamedID))

    # cat("workingsexdataframe is", nrow(WorkingSexDataFrame), "rows", "\n")

    # get age differences

    CurrentAgeMatch <- Baseexisting %>%
      select(RenamedAge,RenamedID)

    MatchedAgeExtract <- MatchingSample %>%
      select(RenamedAge, RenamedID) %>%
      rename(MatchedAge = RenamedAge,
             MatchedID = RenamedID)

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


    for (i in 1:numiters) {

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
        filter(!(RenamedID %in% c(PropPair1[,2], PropPair2[,2]))) %>%
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

      InterimDataFrame <- Baseexisting %>%
        left_join(CurrentAgeMatch, by=c("RenamedID", "RenamedAge")) %>%
        left_join(MatchingSample, by= c("MatchedID" = "RenamedID")) %>%
        select(all_of(NumberColsexistingPlusOne:ncol(.)))

      TheMatched <- bind_rows(TheMatched, InterimDataFrame)

    } else {

      TheMatched <- Baseexisting %>%
        left_join(CurrentAgeMatch, by=c("RenamedID", "RenamedAge")) %>%
        left_join(MatchingSample, by= c("MatchedID" = "RenamedID")) %>%
        select(all_of(NumberColsexistingPlusOne:ncol(.)))


    }

    # closes the loop through the number of sets of existing to match,
    # e.g. 1 set for two-person households, 2 sets for three-person households
  }
  #
  #   # need to ensure that the base for BOTH sexes exists, not just one
  if(exists("AppendedBase")){

    AppendedBase <- bind_rows(AppendedBase, Baseexisting)

  } else {

    AppendedBase <- Baseexisting

  }
  #
  #
  #   # closes loop for matching existing if sex IS NOT correlated
  # }


  # correct the names of the variables in the interim and base data frames
  # row bind these and output

  TheMatched <- TheMatched %>%
    rename_all(list(~gsub("\\.y$", "", .))) %>%
    select(-RenamedAge) %>%
    mutate({{IDColName}} := MatchedID,
           {{AgeColName}} := MatchedAge) %>%
    select(-c(MatchedID, MatchedAge))

  TheBase <- AppendedBase %>%
    mutate({{IDColName}} := RenamedID,
           {{AgeColName}} := RenamedAge) %>%
    select(-c(RenamedID, RenamedAge))

  OutputDataframe <- bind_rows(TheBase, TheMatched)

  cat("The individual dataframes are $Matched and $Unmatched.", "\n")

  MatchedIDs <- OutputDataframe %>%
    pull({{IDColName}})

  UnmatchedDataframe <- existing %>%
    filter(!({{IDColName}} %in% MatchedIDs))


  MergedList <- list()

  MergedList$Matched <- OutputDataframe
  MergedList$Unmatched <- UnmatchedDataframe

  return(MergedList)

}

