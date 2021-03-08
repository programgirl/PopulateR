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
    swap$MatchedAge <- pair2$MatchedAge
    return(swap)
  }

   # set up bins and variables for matching
   # set up bins for iterations
   # enable at least some extreme age differences to be assigned to the Inf categories
   # otherwise the bins will be wrong

   MaxAgeDifference <-  (max(Occupants[OccupantAgeCol]) -
                           min(Occupants[OccupantAgeCol]))-5

   # estimate expected minimum and maximum ages from the distribution, and bin these

   min_bin <- round(qnorm(0.000001, mean = MeanUsed, sd = SDUsed))-0.5
   max_bin <- round(qnorm(0.999999, mean = MeanUsed, sd = SDUsed))+0.5
   bins <- c(-Inf, min_bin:max_bin, Inf)

   # construct the probabilities for each bin, gives n(bins)-1
   Probabilities <- pnorm(bins[-1], mean = MeanUsed, sd = SDUsed) -
     pnorm(bins[-length(bins)], mean = MeanUsed, sd = SDUsed)

   # assign realistic expected probabilities in the bins outside the bins constructed earlier
   # use minAge and maxAge for this, only need range for included ages
   # Uses midpoint rule.
   logProbLow <- dnorm(-MaxAgeDifference:(min_bin-0.5), mean = MeanUsed, sd = SDUsed, log=TRUE)
   logProbHigh <- dnorm((max_bin+0.5):MaxAgeDifference, mean = MeanUsed, sd = SDUsed, log=TRUE)

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


  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

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

      SampleSizeToUse <- nrow(WorkingSexDataFrame)/HouseholdSize
      cat("Sample size is", SampleSizeToUse, "\n")

      BaseSample <- WorkingSexDataFrame %>%
        slice_sample(n = SampleSizeToUse)  #%>%
        # select_all(list(~ paste0("baseDF.", .)))

      WorkingSexDataFrame <- WorkingSexDataFrame %>%
        filter(!(RenamedID %in% BaseSample$RenamedID))

          while(!(is.na(WorkingSexDataFrame$RenamedAge[1])) == TRUE) {

            if(SampleSizeToUse < 1) {
              stop("Sample size is less than 1", "\n")
            }

           MatchingSample <- WorkingSexDataFrame %>%
             slice_sample(n = SampleSizeToUse)

           WorkingSexDataFrame <- WorkingSexDataFrame %>%
             filter(!(RenamedID %in% MatchingSample$RenamedID))

          # cat("workingsexdataframe is", nrow(WorkingSexDataFrame), "rows", "\n")

           # get age differences

           CurrentAgeMatch <- BaseSample %>%
             select(RenamedAge,RenamedID)

           MatchedAgeExtract <- MatchingSample %>%
             select(RenamedAge) %>%
             rename(MatchedAge = RenamedAge)

           CurrentAgeMatch <- cbind(CurrentAgeMatch, MatchedAgeExtract)

           cat("Current age match is", nrow(CurrentAgeMatch), "Matched age extract is",
               nrow(MatchedAgeExtract), "combined age match is", nrow(CurrentAgeMatch), "\n")

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
             PropPair1 <- swap_people(Current1, Current2)
             PropPair2 <- swap_people(Current2, Current1)

             # compute change in Chi-squared value from current pairing to proposed pairing
             PropAgeMatch <- CurrentAgeMatch %>%
               filter(!(RenamedID %in% c(PropPair1[,2], PropPair2[,2]))) %>%
               bind_rows(., PropPair1,PropPair2)

            # cat("PropAgeMatch has", nrow(PropAgeMatch), "rows", "\n")

             # do chi-squared
             Proplog0 <- hist(PropAgeMatch[,1] - PropAgeMatch[,3], breaks = logBins, plot=FALSE)$counts
             ProplogK = ifelse(Proplog0 == 0, 2*logEAgeProbs, log((Proplog0 - exp(logEAgeProbs))^2)) - logEAgeProbs

             prop_log_chisq = max(ProplogK) + log(sum(exp(ProplogK - max(ProplogK))))

             if (compare_logK(ProplogK, logKObservedAges) < 0) {

              # cat("Loop entered", prop_log_chisq, "\n")

               CurrentAgeMatch[Pick1,] <- PropPair1
               CurrentAgeMatch[Pick2,] <- PropPair2


               log0ObservedAges <- Proplog0
               logKObservedAges <- ProplogK
               log_chisq <- prop_log_chisq

             }

              # cat("log chi-square is", log_chisq, "\n")

             if (log_chisq <= Critical_log_chisq) {
               break

             }

             # closes iterations through the age matching
           }

           if(exists("BaseDataFrame") == TRUE) {

             cat("Entering second pass ID start value is", IDStartValue, "\n")

             InterimDataFrame <- left_join(BaseSample %>% group_by(RenamedAge) %>%
                                             mutate(Counter = row_number()),
                                           MatchingSample %>% group_by(RenamedAge) %>%
                                             mutate(Counter = row_number()),
                                           by = c("RenamedAge", "Counter")) %>%
               dplyr::select(-Counter) %>%
               ungroup() %>%
               mutate({{HouseholdNumVariable}} := seq(IDStartValue, (IDStartValue+nrow(BaseSample)-1)))

             IDStartValue = IDStartValue+(nrow(InterimDataFrame)-1)

             cat("Second pass ID end value is", IDStartValue, "\n")

             BaseDataFrame <- bind_rows(BaseDataFrame, InterimDataFrame)

           } else {

             cat("Entering first pass ID start value is", IDStartValue, "\n")

           BaseDataFrame <- left_join(BaseSample %>% group_by(RenamedAge) %>%
                                        mutate(Counter = row_number()),
                                      MatchingSample %>% group_by(RenamedAge) %>%
                                        mutate(Counter = row_number()),
                                      by = c("RenamedAge", "Counter")) %>%
             dplyr::select(-Counter) %>%
             ungroup() %>%
             mutate({{HouseholdNumVariable}} := seq(IDStartValue, (IDStartValue+nrow(BaseSample)-1)))

           IDStartValue = IDStartValue+nrow(BaseSample)

           cat("Ending first pass ID start value is", IDStartValue, "\n")

           }

           # loops through the number of samples required, e.g. if there are three people in a
           # household then the loop needs to complete twice.
          }


           # closes while loop through the data frame for each sex
    }

       # convert from wide to long, use .x and .y to do the split
      #
      # FirstDataframeSplit <- FullMatchedDataFrame %>%
      #   dplyr::select(ends_with(".x"), {{HouseholdNumVariable}}) %>%
      #   rename_all(list(~gsub("\\.x$", "", .)))
      #
      # SecondDataframeSplit <- FullMatchedDataFrame %>%
      #   dplyr::select(ends_with(".y"), {{HouseholdNumVariable}}) %>%
      #   rename_all(list(~gsub("\\.y$", "", .)))

      # TODO work through the extra people data frame
      # there will be exactly the number of people required in the household in there
      # so simply put them into the same household

      # closes the if loop for matching people if sex is correlated
    }

   return(BaseDataFrame)

  }

           #
           # # construct the child ages remaining into a vector
           #
           # BaseDataFrame <- BaseDataFrame %>%
           #   select(-c(AgeDifference, age_index, Counter))
           #
           # # remove the matched children ids from the available children in ChildrenRenamed
           #
           # ChildrenRenamed <- ChildrenRenamed %>%
           #   filter(!(ChildID %in%  BaseDataFrame$ChildID))
           #
           # #  add in the extra children to the twins, where there are more than 2 children in the household
           #
           # # create counts of children remaining so that the rest of the children are brought into the dataframe
           # # also, the ChildAge variable has to start from 2 and not 3, as only one child has been matched in the BaseDataFrame
           #
           # ChildrenCounts <- ChildrenRenamed %>%
           #   group_by(ChildAge) %>%
           #   summarise(AgeCount=n()) %>%
           #   tidyr::complete(ChildAge = seq(min(ChildAge), max(ChildAge)),
           #                   fill = list(AgeCount = 0))
           #
           # minChildIndexAge <- as.integer(ChildrenCounts[1,1])
           # maxChildIndexAge <- as.integer(ChildrenCounts[nrow(ChildrenCounts),1])
           #
           # ChildrenAgeCountVector <- ChildrenCounts$AgeCount
           #
           # # match the remaining children
           #
           # # create the column names
           # for (x in 2:NumChildren) {
           #
           #   BaseDataFrame <- BaseDataFrame %>%
           #     tibble::add_column(!! paste0("ChildAge", x) := 1000)
           #
           #   # closes column name loop
           # }
           #
           # # it being a tibble seemed to be the problem for the looping below.
           #
           # BaseDataFrame <- as.data.frame(BaseDataFrame)
           #
           # # there are problems with the younger parents not having children available
           # # BaseDataFrame <- BaseDataFrame %>%
           # #  arrange(ParentAge)
           #
           # # now iterate through the other children
           # # nested loop must be columns within rows
           #
           # for (x in 1:nrow(BaseDataFrame)) {
           #
           #   # for (x in 1:152) {
           #
           #   AgesUsed <- as.numeric(BaseDataFrame$ChildAge[x])
           #
           #   for (y in (NumberColsChildren + 4):ncol(BaseDataFrame)) {
           #
           #     Counter <- 0
           #
           #     NewChildAge <- sample(minChildAge:maxChildAge, 1, replace = FALSE, prob = c(ChildrenAgeCountVector))
           #     BaseDataFrame[x,y] <- NewChildAge
           #     AgeDifference <- BaseDataFrame$ParentAge[x]- BaseDataFrame[x,y]
           #     age_index <- NewChildAge + 1
           #
           #     # cat("age_index = ", age_index, "length of ChildrenAgeCountVector[age_index] = ", length(ChildrenAgeCountVector[age_index]), "\n")
           #
           #     while (BaseDataFrame[x,y] %in% (AgesUsed) || AgeDifference < MinParentAge || AgeDifference > MaxParentAge || ChildrenAgeCountVector[age_index] == 0) {
           #
           #       # cat("Entered loop", "age_index = ", age_index, "length of ChildrenAgeCountVector[age_index] = ", length(ChildrenAgeCountVector[age_index]), "\n")
           #
           #       NewChildAge <- sample(minChildAge:maxChildAge, 1, replace = FALSE, prob = c(ChildrenAgeCountVector))
           #       BaseDataFrame[x,y] <- NewChildAge
           #       AgeDifference <- BaseDataFrame$ParentAge[x]- BaseDataFrame[x,y]
           #       age_index <- NewChildAge + 1
           #
           #       Counter <- Counter + 1
           #
           #       if (Counter == 1000 ) {
           #
           #         if (ChildrenAgeCountVector[age_index] == 0) {
           #           cat("No children were available at the ages tested", "The problem household is", BaseDataFrame$HouseholdID[x],"\n")
           #         }
           #
           #         if (AgeDifference < MinParentAge || AgeDifference > MaxParentAge) {
           #
           #           # cat("No credible available parent ages were located", "The problem household is", BaseDataFrame$HouseholdID[x],"\n")
           #
           #           if (exists("ParentTooYoung")) {
           #             ParentTooYoung <- c(ParentTooYoung, BaseDataFrame$HouseholdID[x])
           #
           #           } else {
           #             ParentTooYoung <- as.vector(BaseDataFrame$HouseholdID[x])
           #           }
           #         }
           #
           #         if (BaseDataFrame[x,y] %in% (AgesUsed)) {
           #           # cat("Twins were constructed even though the twin families were previously allocated", "The problem household is", BaseDataFrame$HouseholdID[x],"\n")
           #
           #
           #           if (exists("ShouldNotBeTwins")) {
           #             ShouldNotBeTwins <- c(ShouldNotBeTwins, BaseDataFrame$HouseholdID[x])
           #
           #           } else {
           #             ShouldNotBeTwins <- as.vector(BaseDataFrame$HouseholdID[x])
           #           }
           #
           #         }
           #
           #         break
           #       }
           #
           #       # closes while test
           #     }
           #
           #     # cat("Row is ", x, "Current age is ", BaseDataFrame[x,y], "Ages used are ", AgesUsed, "Parent age is ", BaseDataFrame$ParentAge[x], "\n")
           #
           #     ChildrenAgeCountVector[age_index] = ChildrenAgeCountVector[age_index] - 1
           #     AgesUsed <- cbind(AgesUsed, BaseDataFrame[x,y])
           #
           #     # closes for column loop
           #   }
           #
           #   # closes for numchildren loop
           # }
           #
           #
           # NotTwins <- BaseDataFrame %>%
           #   ungroup() %>%
           #   select(all_of(1:NumberColsChildren), NumberColsChildren+3)
           #
           # NoTwinsDataFrame <- NoTwinsDataFrame %>%
           #   filter(!(ChildID %in%  NotTwins$ChildID))
           #
           # ParentOfNotTwins <- BaseDataFrame %>%
           #   ungroup() %>%
           #   select(all_of((NumberColsChildren+2) : (NumberColsChildren+3)))
           #
           # ParentOfNotTwins <- left_join(ParentOfNotTwins, ParentsRenamed, by = c("ParentID", "HouseholdID"))
           #
           # # extract remaining children and rbind these to each other
           # # will eventually be rbind'ed to the twins and parent data
           #
           # for (z in 2:NumChildren) {
           #
           #
           #   #   cat("z is ", z, " and child age column is", BaseDataFrame[(NumberColsChildren + z + 1),])
           #
           #   OtherNotTwins <- BaseDataFrame %>%
           #     ungroup() %>%
           #     select(all_of(c((NumberColsChildren+3), (NumberColsChildren + z + 2)))) %>%
           #     rename(ChildAge = paste0("ChildAge", z))
           #
           #   OtherNotTwins <- left_join(OtherNotTwins %>% group_by(ChildAge) %>% mutate(Counter = row_number()),
           #                              NoTwinsDataFrame %>% group_by(ChildAge) %>% mutate(Counter = row_number()),
           #                              by = c("ChildAge", "Counter")) %>%
           #     select(-Counter)
           #
           #   NotTwins <- bind_rows(NotTwins, OtherNotTwins)
           #
           #   NoTwinsDataFrame <- NoTwinsDataFrame %>%
           #     filter(!(ChildID %in%  OtherNotTwins$ChildID))
           #
           #   #closes extra child addition loop
           # }
           #
           # #####################################
           # #####################################
           # # join all the data frames together
           # #####################################
           # #####################################
           #
           #
           # if (exists("TwinsFinal")) {
           #   ChildrenFinal <- rbind(TwinsFinal, NotTwins)
           #
           # } else {
           #   ChildrenFinal <- NotTwins
           # }
           #
           # if (exists("ParentOfTwins")) {
           #   ParentsFinal <- rbind(ParentOfTwins, ParentOfNotTwins)
           #
           # } else {
           #   ParentsFinal <- ParentOfNotTwins
           # }
           #
           # ChildrenFinal <- ChildrenFinal %>%
           #   rename(PersonID = ChildID, Age = ChildAge)
           #
           # ParentsFinal <- ParentsFinal %>%
           #   rename(PersonID = ParentID, Age = ParentAge)
           #
           # InterimDataframe <- rbind(ParentsFinal, ChildrenFinal)








    #   if(exists("OutputDataFrame")) {
    #
    #     OutputDataFrame <- bind_rows(OutputDataFrame, NewAddition)
    #
    #     WorkingSexDataFrame <- WorkingSexDataFrame %>%
    #       filter(!(RenamedID %in% NewAddition$RenamedID))
    #
    #   } else {
    #
    #     ExtraPeople <- WorkingSexDataFrame %>%
    #       slice_sample(n = nrow(WorkingSexDataFrame) %% HouseholdSize)
    #
    #     WorkingSexDataFrame <- WorkingSexDataFrame %>%
    #       filter(!(RenamedID %in% ExtraPeople$RenamedID))
    #
    #
    #
    #   # closes the for loop through the summary data frame that has 1 row per sex
    # }



