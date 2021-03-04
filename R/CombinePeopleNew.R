#' Create a match of people into households
#' This function creates a data frame of household inhabitants, of the required numbered of inhabitants.
#' One data frame, containing the people to match, is required. The function outputs an equivalent data frame of households. If the number of people in the data frame is not divisible by the household size specified, the excess number of people will be removed using a random process.
#' If the people in the same household should be the same sex, then the value for CorrelateSx should be set to "yes".
#' The use of an age distribution for the matching ensures that an age structure is present in the households. A less correlated age structure can be produced by entering a larger standard deviation.
#' The function performs a reasonableness check for person ID variable, person age, and household number.
#'
#' @export
#' @param Occupants A data frame containing observations limited to the people to be matched into households An age column is required.
#' @param OccupantIDCol The column number for the ID variable.
#' @param OccupantAgeCol The column number for the Age variable.
#' @param OccupantSxCol The column number for the Sex variable.
#' @param HouseholdSize The number of people expected in each household.
#' @param CorrelateSx Whether the people in the household should be the same sex. Default is no.
#' @param MeanUsed The mean of the normal distribution for the distribution of parent ages at the time the child is born. For women, this will commonly be the age at childbirth.
#'  @param SDUsed The standard deviation of the normal distribution for the distribution of parent ages at the time the child is born. For women, this will commonly be the age at childbirth.
#' @param IDStartValue The starting number for generating a variable that identifies the observations in the same household. Must be numeric. If no value is provided, the Household ID starts at 1.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied, and in quotes.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param pValueToStop = The primary stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param NumIterations The maximum number of iterations used to construct the household data frame. This has a default value of 1000000, and is the stopping rule if the algorithm does not converge.

CombinePeople <- function(Occupants, OccupantIDCol, OccupantAgeCol, OccupantSxCol, HouseholdSize = NULL,
                          CorrelateSx = "no", MeanUsed, SDUsed, IDStartValue = 1,
                          HouseholdNumVariable= NULL, UserSeed=NULL, pValueToStop = .01,
                          NumIterations = 1000000)
  {

  options(dplyr.summarise.inform=F)

  # content check
  if (!any(duplicated(Occupants[OccupantIDCol])) == FALSE) {
    stop("The column number for the ID variable in the data frame must be supplied.")
  }

  if (!is.numeric(OccupantAgeCol)) {
    stop("Both the people ID and the age column numbers must be supplied.")
  }

  if (is.null(HouseholdSize)) {
    stop("The household size must be supplied.")
  }

    if (is.null(HouseholdNumVariable)) {
    stop("A name for the household count variable must be supplied.")
    }

  cat("number rows is", nrow(Occupants), "and household size is", HouseholdSize, "\n")
  if((nrow(Occupants) %% HouseholdSize) > 0) {
    stop("The number of people to be assigned to households is not divisible by household size.")
      }



  #####################################
  #####################################
  # sub functions are here
  #####################################
  #####################################

  # pairing swap subfunction

  swap_people <- function(pair1, pair2) {
    swap <- pair1
    swap$DonorID <- pair2$DonorID
    swap$DonorAge <- pair2$DonorAge
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
  IDColName <- sym(names(Occupants[OccupantIDCol]))

  # Age variable
  AgeColName <- sym(names(Occupants[OccupantAgeCol]))

  # Sex variable
  SexColName <- sym(names(Occupants[OccupantSxCol]))

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

  OccupantsRenamed <- Occupants %>%
    rename(RenamedID = !! OccupantIDCol, RenamedAge = !! OccupantAgeCol,
           RenamedSex = !! OccupantSxCol) %>%
    mutate(RenamedSex = as.character(RenamedSex))

  #####################################
  #####################################

  #####################################
  #####################################
  # set up pre-data information for matching
  #####################################
  #####################################



  #####################################
  #####################################
  # work on sex is correlated first
  #####################################
  #####################################

  if(CorrelateSx == "yes") {

    NumberSexes <- OccupantsRenamed %>%
      group_by(RenamedSex) %>%
      summarise(NumberEachSex = n()) %>%
      mutate(CountModulo = NumberEachSex %% HouseholdSize)

    # test if each Sex is divisible by household size
    if(!(sum(NumberSexes$CountModulo)) == 0) {

      cat("At least one sex has a count indivisible by", HouseholdSize,
          "and one household will contain different sexes", "\n")

      # closes modulo check for console print
    }

    #####################################
    # if sex is correlated
    # matching is within-subset first
    #####################################

    for(i in 1:nrow(NumberSexes)) {

      SexInUse <- as.character(NumberSexes[i,1])

      cat("Sex in use is", SexInUse, "\n")

      WorkingSexDataFrame <- OccupantsRenamed %>%
        filter(RenamedSex == SexInUse)

      if(!(nrow(WorkingSexDataFrame) %% HouseholdSize == 0)) {

           if(exists("ExtraPeople")) {

          NewAddition <-  WorkingSexDataFrame %>%
            slice_sample(n = nrow(WorkingSexDataFrame) %% HouseholdSize)

          ExtraPeople <- bind_rows(ExtraPeople, NewAddition)

          WorkingSexDataFrame <- WorkingSexDataFrame %>%
            filter(!(RenamedID %in% NewAddition$RenamedID))

        } else {

        ExtraPeople <- WorkingSexDataFrame %>%
          slice_sample(n = nrow(WorkingSexDataFrame) %% HouseholdSize)

        WorkingSexDataFrame <- WorkingSexDataFrame %>%
          filter(!(RenamedID %in% ExtraPeople$RenamedID))

        # closes loop for extracting people who cannot be matched to the same sex
        }

        # closes loop for extracting extra people into a separate data frame to be
        # dealt with later
      }

      cat("Working data frame is", nrow(WorkingSexDataFrame), "rows", "\n")

          while(!(is.na(WorkingSexDataFrame$RenamedAge[1])) == TRUE) {

            SampleSizeToUse <- nrow(WorkingSexDataFrame)/HouseholdSize

    #        cat("Sample size is", SampleSizeToUse, "\n")

            if(SampleSizeToUse < 1) {
              stop("Sample size is less than 1", "\n")
            }

            ExtractBaseSample <- WorkingSexDataFrame %>%
              slice_sample(n = SampleSizeToUse)

            WorkingSexDataFrame <- WorkingSexDataFrame %>%
              filter(!(RenamedID %in% ExtractBaseSample$RenamedID))

           ExtractMatchingSample <- WorkingSexDataFrame %>%
             slice_sample(n = SampleSizeToUse)

           WorkingSexDataFrame <- WorkingSexDataFrame %>%
             filter(!(RenamedID %in% ExtractMatchingSample$RenamedID))


            # cat("workingsexdataframe is", nrow(WorkingSexDataFrame), "rows", "\n")


      #
      #   WorkingPerson <- WorkingSexDataFrame %>%
      #     slice_sample(n = 1)
      #
      #   cat("Sex in use is", SexInUse, "and ID is", WorkingPerson$RenamedID, "\n")
      #
      #   WorkingSexDataFrame <- WorkingSexDataFrame %>%
      #     filter(!(RenamedID %in% WorkingPerson$RenamedID))
      #
      #
      #   MatchPerson <-
      #
      #
      #
        # closes while look through the data frame matching the sex
      }




      # closes the for loop through the summary data frame that has 1 row per sex
    }


    # TODO work through the extra people data frame
    # there will be exactly the number of people required in the household in there
    # so simply put them into the same household

    # closes the loop for matching people if sex is correlated
  }


  #
 #  #####################################
 #  # split the dataframe into the required number of subsets
 #  #####################################
 #
 #  # create base data frame, which all the others will match to
 #
 #  MaxIDStartValue <- ((nrow(Occupants)/HouseholdSize)-1) + IDStartValue
 #
 #
 #  BaseDataFrame <- Occupants %>%
 #    slice_sample(weight_by := {{OccupantAgeCol}}, n=nrow(Occupants)/HouseholdSize, replace = FALSE) %>%
 #    mutate({{HouseholdNumVariable}} := seq(IDStartValue, MaxIDStartValue))
 #
 #   IDList <- BaseDataFrame[,OccupantIDCol]
 #   BaseDataFrameIDList <- BaseDataFrame[,OccupantIDCol]
 #
 #  # set up bins for iterations
 #  # enable at least some extreme age differences to be assigned to the Inf categories
 #  # otherwise the bins will be wrong
 #
 #  MaxAgeDifference <-  (max(Occupants[OccupantAgeCol]) -
 #                          min(Occupants[OccupantAgeCol]))-5
 #
 #  # estimate expected minimum and maximum ages from the distribution, and bin these
 #
 #  min_bin <- round(qnorm(0.000001, mean = MeanUsed, sd = SDUsed))-0.5
 #  max_bin <- round(qnorm(0.999999, mean = MeanUsed, sd = SDUsed))+0.5
 #  bins <- c(-Inf, min_bin:max_bin, Inf)
 #
 #  # construct the probabilities for each bin, gives n(bins)-1
 #  Probabilities <- pnorm(bins[-1], mean = MeanUsed, sd = SDUsed) -
 #    pnorm(bins[-length(bins)], mean = MeanUsed, sd = SDUsed)
 #
 #  # assign realistic expected probabilities in the bins outside the bins constructed earlier
 #  # use minAge and maxAge for this, only need range for included ages
 #  # Uses midpoint rule.
 #  logProbLow <- dnorm(-MaxAgeDifference:(min_bin-0.5), mean = MeanUsed, sd = SDUsed, log=TRUE)
 #  logProbHigh <- dnorm((max_bin+0.5):MaxAgeDifference, mean = MeanUsed, sd = SDUsed, log=TRUE)
 #
 #  logProb <- c(logProbLow, log(Probabilities[-c(1, length(Probabilities))]), logProbHigh)
 #  logBins    <- c(-Inf, -(MaxAgeDifference-.5):(MaxAgeDifference-.5), Inf)
 #
 #  #####################################
 #  #####################################
 #  # end set up
 #  #####################################
 #  #####################################
 #  #
 #  #
 #  #
 #  # # # waaaaaay too damn complicated below.
 #  # # # just do one at at time
 #  # # # inside the loop for the number of iterations
 #  # # # slice one
 #  # # # do the matching
 #  # # # construct the ID for the first set
 #  # # # merge the two sets of IDs into a vector
 #  # # # check vector for IDs already used then loop
 #  # # # removes need to construct multipel data frames.
 #  # #
 #  #
 #  #
 #  for (j in 2:HouseholdSize) {
 #
 #  h <- 1
 #
 #  #    if (h == HouseholdSize) {
 #    if (j == HouseholdSize) {
 #
 #        AvailablePeople <- Occupants %>%
 #        anti_join(IDList)
 #
 #      } else {
 #
 #        # sample from the Occupant data frame
 #        AvailablePeople <- Occupants %>%
 #          anti_join(IDList) %>%
 #          slice_sample(n = ((nrow(.))/(HouseholdSize - h)), replace = FALSE)
 #
 #        # print(nrow(AvailablePeople)/(HouseholdSize - h))
 #
 #        h <- h + 1
 #
 #        # add used IDs to the ID list
 #        NewIDList <- AvailablePeople[,OccupantIDCol]
 #        IDList <- rbind(IDList, NewIDList)
 #
 #       print("Two-person households should not have entered")
 #
 #      }
 #
 #    DonorCounts <- AvailablePeople %>%
 #      group_by_at(OccupantAgeCol) %>%
 #      summarise(AgeCount=n())
 #
 #    DonorAges <- pull(DonorCounts[1])
 #    DonorAgeCounts <- pull(DonorCounts[2])
 #
 #    CurrentAgeMatch <- data.frame(BaseDataFrame[OccupantIDCol],
 #                                  BaseDataFrame[OccupantAgeCol],
 #                                  DonorAge = sample(rep(DonorAges, DonorAgeCounts),
 #                                                    size=nrow(BaseDataFrame),
 #                                                    replace = FALSE))
 #
 #    # # set up for chi-squared test
 #    ExpectedAgeProbs <- Probabilities * nrow(CurrentAgeMatch)
 #    logEAgeProbs <- logProb + log(nrow(CurrentAgeMatch))
 #
 #    # construct starting set of observed age difference values for iteration
 #    ObservedAgeDifferences <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,3], breaks = bins, plot=FALSE)$counts
 #
 #    # set up for chi-squared
 #    log0ObservedAges <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,3], breaks = logBins, plot=FALSE)$counts
 #    logKObservedAges = ifelse(log0ObservedAges == 0, 2*logEAgeProbs, log((log0ObservedAges - exp(logEAgeProbs))^2)) - logEAgeProbs
 #    log_chisq = max(logKObservedAges) + log(sum(exp(logKObservedAges - max(logKObservedAges))))
 #
 #    if (is.null(pValueToStop)) {
 #
 #      Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
 #
 #    } else {
 #
 #      Critical_log_chisq <- log(qchisq(pValueToStop, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
 #
 #    }
 #
 #    #####################################
 #    #####################################
 #    # iteration for matching ages starts here
 #    #####################################
 #    #####################################
 #
 #    for (i in 1:NumIterations) {
 #
 #      # randomly choose two pairs
 #      Pick1 <- sample(nrow(CurrentAgeMatch), 1)
 #      Pick2 <- sample(nrow(CurrentAgeMatch), 1)
 #      Current1 <- CurrentAgeMatch[Pick1,]
 #      Current2 <- CurrentAgeMatch[Pick2,]
 #
 #      # # proposed pairing after a swap
 #      PropPair1 <- swap_people(Current1, Current2)
 #      PropPair2 <- swap_people(Current2, Current1)
 #
 #      # compute change in Chi-squared value from current pairing to proposed pairing
 #      PropAgeMatch <- CurrentAgeMatch %>%
 #        #filter(!(BaseDataFrameIDList[,1] %in% c(PropPair1[,1], PropPair2[,1]))) %>%
 #        #filter(!(BaseDataFrameIDList %in% c(PropPair1[,1], PropPair2[,1]))) %>%
 #        filter(!({{IDColName}} %in% c(PropPair1[,1], PropPair2[,1]))) %>%
 #        bind_rows(., PropPair1,PropPair2)
 #
 #      # do chi-squared
 #      Proplog0 <- hist(PropAgeMatch[,2] - PropAgeMatch[,3], breaks = logBins, plot=FALSE)$counts
 #      ProplogK = ifelse(Proplog0 == 0, 2*logEAgeProbs, log((Proplog0 - exp(logEAgeProbs))^2)) - logEAgeProbs
 #
 #      prop_log_chisq = max(ProplogK) + log(sum(exp(ProplogK - max(ProplogK))))
 #
 #      if (compare_logK(ProplogK, logKObservedAges) < 0) { # we cancel out the bits that haven't changed first.
 #
 #        CurrentAgeMatch[Pick1,] <- PropPair1
 #        CurrentAgeMatch[Pick2,] <- PropPair2
 #
 #        log0ObservedAges <- Proplog0
 #        logKObservedAges <- ProplogK
 #        log_chisq <- prop_log_chisq
 #
 #
 #      }
 #
 #      if (log_chisq <= Critical_log_chisq) {
 #        break
 #
 #      }
 #      #####################################
 #      #####################################
 #      # iteration for matching ages ends here
 #      #####################################
 #      #####################################
 #
 #
 #    }
 #
 #    #####################################
 #    #####################################
 #    # pairing the actual occupants starts here
 #    #####################################
 #    #####################################
 #
 #    # return matched household pairs
 #    # extract ages counts for matching the donors
 #    MatchedDonorAges <- CurrentAgeMatch %>%
 #      dplyr::select(DonorAge) %>%
 #      group_by(DonorAge) %>%
 #      mutate(DonorAgeCount = row_number()) %>%
 #      ungroup()
 #
 #      # generate same AgeCount second ID variable for the donor data
 #      # the AgeCount is used to ensure that the first donor with a specific age is matched first
 #      # the second donor with a specific age is matched second
 #      # and so forth
 #    DonorsToMatch <- AvailablePeople %>%
 #      group_by_at(OccupantAgeCol) %>%
 #      mutate(DonorAgeCount = row_number()) %>%
 #      ungroup()
 #
 #    # reduce pool of potentially partnered donors to only those matched
 #    DonorsMatched <- left_join(MatchedDonorAges,
 #                               rename_at(DonorsToMatch, OccupantAgeCol, ~ names(MatchedDonorAges)[1]),
 #                               by = c(names(MatchedDonorAges)[1], "DonorAgeCount")) %>%
 #      mutate(!!AgeColName := DonorAge)
 #
 #    # need both donor age and donor age count so that the join between the base and the donors works
 #    # do not need base dataframe age as this will be a duplicate column on the merge
 #    BaseDataFrameMatchPrep <- CurrentAgeMatch %>%
 #      group_by(DonorAge) %>%
 #      mutate(DonorAgeCount = row_number()) %>%
 #      dplyr::select(-c(2))
 #
 #
 #    PreppedBaseDataFrame <- left_join(BaseDataFrame, BaseDataFrameMatchPrep, by = names(Occupants[OccupantIDCol]))
 #
 #    # now merge the full data of the subset people to the base data frame
 #    # by donor age and donor age count
 #    # this merge must happen each iteration through the data frame
 #
 #    FullMatchedDataFrame <- left_join(PreppedBaseDataFrame, DonorsMatched, by=c("DonorAge", "DonorAgeCount")) %>%
 #      dplyr::select(-DonorAge, -DonorAgeCount) %>%
 #      ungroup()
 #
 #    if (exists("UpdatingDataFrame")) {
 #      SecondDataframeSplit <- FullMatchedDataFrame %>%
 #        dplyr::select(ends_with(".y"), {{HouseholdNumVariable}}) %>%
 #        rename_all(list(~gsub("\\.y$", "", .)))
 #
 #        OutputDataFrame <- rbind(OutputDataFrame, SecondDataframeSplit)
 #
 #      print("UpdatingDataFrame loop incorrectly entered for 2-person households")
 #
 #    } else {
 #      UpdatingDataFrame <- FullMatchedDataFrame
 #
 #      FirstDataframeSplit <- UpdatingDataFrame %>%
 #        dplyr::select(ends_with(".x"), {{HouseholdNumVariable}}) %>%
 #        rename_all(list(~gsub("\\.x$", "", .)))
 #
 #      SecondDataframeSplit <- UpdatingDataFrame %>%
 #        dplyr::select(ends_with(".y"), {{HouseholdNumVariable}}) %>%
 #        rename_all(list(~gsub("\\.y$", "", .)))
 #
 #      OutputDataFrame <- rbind(FirstDataframeSplit, SecondDataframeSplit)
 #
 #      print("Loop should only be entered on first pass")
 #
 #    }
 #
 #    # convert from wide to long, use .x and .y to do the split
 #
 #    # FirstDataframeSplit <- UpdatingDataFrame %>%
 #    #   dplyr::select(ends_with(".x"), {{HouseholdNumVariable}}) %>%
 #    #   rename_all(list(~gsub("\\.x$", "", .)))
 #    #
 #    # SecondDataframeSplit <- UpdatingDataFrame %>%
 #    #   dplyr::select(ends_with(".y"), {{HouseholdNumVariable}}) %>%
 #    #   rename_all(list(~gsub("\\.y$", "", .)))
 #    #
 #    # if (exists("OutputDataframe")) {
 #    #
 #    #   TemporaryBind <- rbind(FirstDataframeSplit, SecondDataframeSplit)
 #    #   OutputDataframe <- rbind(OutputDataframe, TemporaryBind)
 #    #
 #    #   print("OutputDataframe loop incorrectly entered for 2-person households")
 #    #   print(nrow(OutputDataframe))
 #    #
 #    #
 #    # } else {
 #    #
 #    #   print("OutputDataframe loop correctly entered for 2-person households")
 #    #
 #    #   OutputDataframe <- rbind(FirstDataframeSplit, SecondDataframeSplit)
 #    #
 #    # }
 #
 #    # test for existing output data frame, because the second SecondDataframeSplit will be appended if this is the case
 #
 #
 #
 #
 #   }
 #
 #  #####################################
 #  #####################################
 #  # Output data frame with rbinds finished here
 #  #####################################
 #  #####################################
 #
 #
 #  # use for checking number of iterations used, the p-value to stop, and the p-value reached
 #  # shift to iteration where required
 #  #
 #  # print(i)
 #  print(Critical_log_chisq)
 #  print(log_chisq)
 #
 # # return(OutputDataFrame)

 return(ExtractMatchingSample)


}
