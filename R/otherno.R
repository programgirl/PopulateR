#' Create a match of people into households
#' This function creates a data frame of household inhabitants, of the required numbered of inhabitants.
#' One data frame, containing the people to match, is required. The function outputs an equivalent data frame of households. If the number of people in the data frame is not divisible by the household size specified, the excess number of people will be removed using a random process.
#' If the people in the same household should be the same sex, then the value for ssrate should be set to "yes".
#' The use of an age distribution for the matching ensures that an age structure is present in the households. A less correlated age structure can be produced by entering a larger standard deviation.
#'
#' default value of 0 reflects populations with co-sex households. completely sex-split households take the value 1. binary variable.
#' The function performs a reasonableness check for person ID variable, person age, and household number.
#'
#' @export
#' @param people A data frame containing observations limited to the people to be matched into households An age column is required.
#' @param pplidcol The column number for the ID variable.
#' @param pplagecol The column number for the Age variable.
#' @param pplsxcol The column number for the Sex variable.
#' @param numppl The number of people expected in each household.
#' @param ssrate Must be either 0 or 1. The default value of 0 is random generation.
#' @param sdused The standard deviation of the normal distribution for the distribution of parent ages at the time the child is born. For women, this will commonly be the age at childbirth.
#' @param IDStartValue The starting number for generating a variable that identifies the observations in the same household. Must be numeric. If no value is provided, the Household ID starts at 1.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied, and in quotes.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param pValueToStop = The primary stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param NumIterations The maximum number of iterations used to construct the household data frame. This has a default value of 1000000, and is the stopping rule if the algorithm does not converge.

otherno <- function(people, pplidcol, pplagecol, pplsxcol, numppl = NULL,
                          ssrate = 0, sdused, IDStartValue = 1,
                          HouseholdNumVariable= NULL, UserSeed=NULL, pValueToStop = .01,
                          NumIterations = 1000000)
{

  options(dplyr.summarise.inform=F)

  # content check
  if (!any(duplicated(people[pplidcol])) == FALSE) {
    stop("The column number for the ID variable in the data frame must be supplied.")
  }

  if (!is.numeric(pplagecol)) {
    stop("Both the people ID and the age column numbers must be supplied.")
  }

  if (is.null(numppl)) {
    stop("The household size must be supplied.")
  }

  if (!(ssrate == 0) | !(ssrate == 1)) {
    stop("The rate for same-sex cohabitation must either be completely random (0) or completely sex-segregated (1)")
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

  MaxAgeDifference <-  (max(people[peopleAgeCol]) -
                          min(people[peopleAgeCol]))-5

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
  IDColName <- sym(names(people[pplidcol]))

  # Age variable
  AgeColName <- sym(names(people[peopleAgeCol]))

  # Sex variable
  SexColName <- sym(names(people[peoplexCol]))

  # need column count for turning wide dataframe into long
  NumberColspeoplePlusOne <- as.numeric(ncol(people))+1

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

  peopleRenamed <- people %>%
    rename(RenamedID = !! pplidcol, RenamedAge = !! peopleAgeCol,
           RenamedSex = !! peoplexCol) %>%
    mutate(RenamedSex = as.character(RenamedSex))

  #####################################
  #####################################


  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  #####################################
  #####################################
  # work on sex is correlated first
  #####################################
  #####################################

  if(ssrate == "yes") {

    NumberSexes <- peopleRenamed %>%
      group_by(RenamedSex) %>%
      summarise(NumberEachSex = n()) %>%
      mutate(CountModulo = NumberEachSex %% numppl)

    # test if each Sex is divisible by household size
    if(!(sum(NumberSexes$CountModulo)) == 0) {

      cat("At least one sex has a count indivisible by", numppl,
          "and one household will contain different sexes", "\n")

      # closes modulo check for console print
    }


    #####################################
    # if sex is correlated
    # matching is within-subset first
    #####################################

    for(i in 1:nrow(NumberSexes)) {

      SexInUse <- as.character(NumberSexes[i,1])

      # cat("Sex in use is", SexInUse, "\n")

      WorkingSexDataFrame <- peopleRenamed %>%
        filter(RenamedSex == SexInUse)

      if(!(nrow(WorkingSexDataFrame) %% numppl == 0)) {

        if(exists("ExtraPeople")) {

          NewAddition <-  WorkingSexDataFrame %>%
            slice_sample(n = nrow(WorkingSexDataFrame) %% numppl)

          ExtraPeople <- bind_rows(ExtraPeople, NewAddition)

          WorkingSexDataFrame <- WorkingSexDataFrame %>%
            filter(!(RenamedID %in% NewAddition$RenamedID))

        } else {

          ExtraPeople <- WorkingSexDataFrame %>%
            slice_sample(n = nrow(WorkingSexDataFrame) %% numppl)

          WorkingSexDataFrame <- WorkingSexDataFrame %>%
            filter(!(RenamedID %in% ExtraPeople$RenamedID))


          # closes loop for extracting people who cannot be matched to the same sex
        }

        # closes loop for extracting extra people into a separate data frame to be
        # dealt with later
      }

      #    cat("Working data frame is", nrow(WorkingSexDataFrame), "rows", "\n")

      SampleSizeToUse <- nrow(WorkingSexDataFrame)/numppl

      #    cat("Sample size is", SampleSizeToUse, "and ID start value is", IDStartValue, "\n")

      BaseSample <- WorkingSexDataFrame %>%
        slice_sample(n = SampleSizeToUse) %>%
        mutate({{HouseholdNumVariable}} := seq(IDStartValue, (IDStartValue + SampleSizeToUse - 1)))

      #   cat("Base sample size is", nrow(BaseSample), "\n")

      IDStartValue = IDStartValue + SampleSizeToUse - 1

      WorkingSexDataFrame <- WorkingSexDataFrame %>%
        filter(!(RenamedID %in% BaseSample$RenamedID))

      #   cat("workingsexdataframe is", nrow(WorkingSexDataFrame), "rows", "\n")

      while(!(is.na(WorkingSexDataFrame$RenamedAge[1])) == TRUE) {

        if(SampleSizeToUse < 1) {
          stop("Sample size is less than 1", "\n")
        }

        MatchingSample <- WorkingSexDataFrame %>%
          slice_sample(n = SampleSizeToUse)

        #    cat("MatchingSample size is", nrow(MatchingSample), "\n")

        WorkingSexDataFrame <- WorkingSexDataFrame %>%
          filter(!(RenamedID %in% MatchingSample$RenamedID))

        # cat("workingsexdataframe is", nrow(WorkingSexDataFrame), "rows", "\n")

        # get age differences

        CurrentAgeMatch <- BaseSample %>%
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

        if (is.null(pValueToStop)) {

          Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))

        } else {

          Critical_log_chisq <- log(qchisq(pValueToStop, df=(length(logEAgeProbs-1)), lower.tail = TRUE))

          # closes p-value stopping rule
        }

        #####################################
        #####################################
        # iteration for matching  ages starts here
        #####################################
        #####################################

        for (i in 1:NumIterations) {

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

            #        cat("Loop entered", prop_log_chisq, "\n")

            CurrentAgeMatch[Pick1,] <- PropPair1
            CurrentAgeMatch[Pick2,] <- PropPair2


            log0ObservedAges <- Proplog0
            logKObservedAges <- ProplogK
            log_chisq <- prop_log_chisq

            # closes pair swqp

          }

          #          cat("log chi-square is", log_chisq, "\n")

          if (log_chisq <= Critical_log_chisq) {
            break

          }

          # closes iterations through the age matching
        }


        if(exists("TheMatched")) {

          InterimDataFrame <- BaseSample %>%
            left_join(CurrentAgeMatch, by=c("RenamedID", "RenamedAge")) %>%
            left_join(MatchingSample, by= c("MatchedID" = "RenamedID")) %>%
            select(all_of(NumberColspeoplePlusOne:ncol(.)))

          TheMatched <- bind_rows(TheMatched, InterimDataFrame)

        } else {

          TheMatched <- BaseSample %>%
            left_join(CurrentAgeMatch, by=c("RenamedID", "RenamedAge")) %>%
            left_join(MatchingSample, by= c("MatchedID" = "RenamedID")) %>%
            select(all_of(NumberColspeoplePlusOne:ncol(.)))


        }

        # closes the loop through the number of sets of people to match,
        # e.g. 1 set for two-person households, 2 sets for three-person households
      }

      # need to ensure that the base for BOTH sexes exists, not just one
      if(exists("AppendedBase")){

        AppendedBase <- bind_rows(AppendedBase, BaseSample)

      } else {

        AppendedBase <- BaseSample

      }


      # closes for loop through the data frame for each sex
    }


    # closes the if loop for matching people if sex IS correlated
    #corresponding else for sex not correlated is below

  }  else {

    WorkingSexDataFrame <- peopleRenamed

    SampleSizeToUse <- nrow(WorkingSexDataFrame)/numppl

    #    cat("Sample size is", SampleSizeToUse, "and ID start value is", IDStartValue, "\n")

    BaseSample <- WorkingSexDataFrame %>%
      slice_sample(n = SampleSizeToUse) %>%
      mutate({{HouseholdNumVariable}} := seq(IDStartValue, (IDStartValue + SampleSizeToUse - 1)))

    #   cat("Base sample size is", nrow(BaseSample), "\n")

    IDStartValue = IDStartValue + SampleSizeToUse - 1

    WorkingSexDataFrame <- WorkingSexDataFrame %>%
      filter(!(RenamedID %in% BaseSample$RenamedID))

    #   cat("workingsexdataframe is", nrow(WorkingSexDataFrame), "rows", "\n")

    while(!(is.na(WorkingSexDataFrame$RenamedAge[1])) == TRUE) {

      if(SampleSizeToUse < 1) {
        stop("Sample size is less than 1", "\n")
      }

      MatchingSample <- WorkingSexDataFrame %>%
        slice_sample(n = SampleSizeToUse)

      #    cat("MatchingSample size is", nrow(MatchingSample), "\n")

      WorkingSexDataFrame <- WorkingSexDataFrame %>%
        filter(!(RenamedID %in% MatchingSample$RenamedID))

      # cat("workingsexdataframe is", nrow(WorkingSexDataFrame), "rows", "\n")

      # get age differences

      CurrentAgeMatch <- BaseSample %>%
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

      if (is.null(pValueToStop)) {

        Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))

      } else {

        Critical_log_chisq <- log(qchisq(pValueToStop, df=(length(logEAgeProbs-1)), lower.tail = TRUE))

        # closes p-value stopping rule
      }

      #####################################
      #####################################
      # iteration for matching  ages starts here
      #####################################
      #####################################

      for (i in 1:NumIterations) {

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

          #        cat("Loop entered", prop_log_chisq, "\n")

          CurrentAgeMatch[Pick1,] <- PropPair1
          CurrentAgeMatch[Pick2,] <- PropPair2


          log0ObservedAges <- Proplog0
          logKObservedAges <- ProplogK
          log_chisq <- prop_log_chisq

          # closes pair swqp

        }

        #          cat("log chi-square is", log_chisq, "\n")

        if (log_chisq <= Critical_log_chisq) {
          break

        }

        # closes iterations through the age matching
      }


      if(exists("TheMatched")) {

        InterimDataFrame <- BaseSample %>%
          left_join(CurrentAgeMatch, by=c("RenamedID", "RenamedAge")) %>%
          left_join(MatchingSample, by= c("MatchedID" = "RenamedID")) %>%
          select(all_of(NumberColspeoplePlusOne:ncol(.)))

        TheMatched <- bind_rows(TheMatched, InterimDataFrame)

      } else {

        TheMatched <- BaseSample %>%
          left_join(CurrentAgeMatch, by=c("RenamedID", "RenamedAge")) %>%
          left_join(MatchingSample, by= c("MatchedID" = "RenamedID")) %>%
          select(all_of(NumberColspeoplePlusOne:ncol(.)))


      }

      # closes the loop through the number of sets of people to match,
      # e.g. 1 set for two-person households, 2 sets for three-person households
    }

    # need to ensure that the base for BOTH sexes exists, not just one
    if(exists("AppendedBase")){

      AppendedBase <- bind_rows(AppendedBase, BaseSample)

    } else {

      AppendedBase <- BaseSample

    }


    # closes loop for matching people if sex IS NOT correlated
  }


  # correct the names of the variables in the interim and base data frames
  # row bind these and output

  TheMatched <- TheMatched %>%
    rename_all(list(~gsub("\\.y$", "", .))) %>%
    select(-RenamedAge) %>%
    mutate({{IDColName}} := MatchedID,
           {{AgeColName}} := MatchedAge,
           {{SexColName}} := RenamedSex) %>%
    select(-c(MatchedID, MatchedAge, RenamedSex))

  TheBase <- AppendedBase %>%
    mutate({{IDColName}} := RenamedID,
           {{AgeColName}} := RenamedAge,
           {{SexColName}} := RenamedSex) %>%
    select(-c(RenamedID, RenamedAge, RenamedSex))


  OutputDataFrame <- bind_rows(TheBase, TheMatched)


  return(OutputDataFrame)

}

