#' Create a subset of observations containing only children matched to parents/guardians
#' This function creates a data frame of child-parent/guardian pairs, based on a population distribution of age differences. The distribution used in this function is the log normal.
#' Two data frames are required. The Children data frame contains the age data, to which the Parent (Guardian) data will be applied.
#' The minimum and maximum ages of parents can be specified, to minimise the result that there are parents who were too young (e.g. 11 years) or too old (e.g. 70 years) at becoming a parent. The presence of too young and too old parents is tested at the start of the process and an attempt is made to remove initial pairings outside this range. Thus,pre-cleaning the Parent data frame is not required, although pairs outside this age range may be output, due to the distribution used. This solution guarantees no out-of-scope matches.
#' The minimum proportion prevents the outcome where most/all people of a particular age, eg. the entire set of 25-year-olds, are parents. The default value is NULL, which assumes that all people of any age can be parents. The defalt value is 0, enabling a pre-cleaned data frame of parents to be used.
#' An even number of observations is output, which is one child-parent pair.
#'
#' The function performs a reasonableness check for child ID, child age, parent ID variable, and household number.
#'
#' @export
#' @param Children A data frame containing observations limited to the children to be matched An age column is required. All children in this data frame will be matched to a parent/guardian.
#' @param ChildIDVariable The column number for the ID variable in the Children data frame.
#' @param ChildAgeVariable The column number for the Age variable in the Children data frame.
#' @param meanlogUsed The mean of the natural log for the distribution of parent ages at the time the child is born. For women, this will commonly be the age at childbirth.
#'  @param sdlogUsed The standard deviation of the natural log for the distribution of parent ages at the time the child is born. For women, this will commonly be the age at childbirth.
#' @param Parents A data frame containing observations limited to parents. An age column is required. This can contain the entire set of people who can be parents, as the assignment is made on age at becoming a parent, not current age. This file can contain the people who can be guardians, as well as parents. This data frame must contain at least the same number of observations as the Children data frame.
#' @param ParentIDVariable The column number for the ID variable in the Parent data frame.
#' @param ParentAgeVariable The column number for the Age variable in the Parent data frame.
#' @param MinParentAge The youngest age at which a person becomes a parent. The default value is 0, which permits the user to set no lower bound on age. This variable must contain a value.
#' @param MaxParentAge The oldest age at which a person becomes a parent. The default value is 55. This variable can still be set if nrow(Children) == nrow(Parents).
#' @param MinPropRemain The minimum proportion of people, at each age, who are not parents. The default is zero, which may result in all people at a specific age being allocated as parents. This will leave age gaps for any future work, and may not be desirable. If nrow(Children) == nrow(Parents), assigning any value other than 0 will result in an error.
#' @param DyadIDValue The starting number for generating a variable that identifies the observations in a child-parent dyad. Must be numeric. If no value is provided, the Dyad ID starts at 1.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied, and in quotes.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param pValueToStop = The primary stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param NumIterations The maximum number of iterations used to construct the coupled data frame. This has a default value of 1000000, and is the stopping rule if the algorithm does not converge.

AddChildTruncLn <- function(Children, ChildIDVariable, ChildAgeVariable, Parents, ParentIDVariable, ParentAgeVariable,
                       meanlogUsed, sdlogUsed, MinParentAge = 0, MaxParentAge = 55, MinPropRemain = 0,
                       DyadIDValue = NULL, HouseholdNumVariable= NULL, UserSeed=NULL, pValueToStop = .01,
                       NumIterations = 1000000)
{

  options(dplyr.summarise.inform=F)

  # content check
  if (!any(duplicated(Children[ChildIDVariable])) == FALSE) {
    stop("The column number for the ID variable in the child data frame must be supplied.")
  }

  if (!is.numeric(ChildAgeVariable)) {
    stop("Both the child ID and the child age column numbers must be supplied.")
  }

  if (!any(duplicated(Parents[ParentIDVariable])) == FALSE) {
    stop("The column number for the ID variable in the parent data frame must be supplied.")
  }

  if (is.null(HouseholdNumVariable)) {
    stop("A name for the household count variable must be supplied.")
  }

  #####################################
  #####################################
  # sub functions are here
  #####################################
  #####################################


  # fix extreme age differences created at the start
  # these may be interfering with pairing convergence

  fixExtremes <- function(InitialAgeMatch) {

    i <- 0

    # while(NumToSwap > 0 & i < 51){

     RowsToSwap <- InitialAgeMatch %>%
      mutate(row_no = row_number()) %>%
      filter(row_number() > max(row_number()) - NumToSwap | row_number() <= NumToSwap) %>%
      mutate(final_age1 = ParentAge) %>%
      dplyr::select(final_age1, row_no)


    RowsToSwap$row_no <- sort(RowsToSwap$row_no, decreasing = T)

    fixedDF <- InitialAgeMatch %>%
      left_join(RowsToSwap, by = "row_no") %>%
      mutate(final_age1 = ifelse(is.na(final_age1), ParentAge, final_age1)) %>%
      dplyr::select(-ParentAge, -row_no, -AgeDiff) %>%
      rename(ParentAge = final_age1)

    InitialAgeMatch <- fixedDF %>%
      mutate(AgeDiff = ParentAge - .[[2]]) %>%
      arrange(AgeDiff) %>%
      mutate(row_no = row_number())

    # then test if any diffs are outside the required age range

    # TooYoung <- InitialAgeMatch %>%
    #   filter(AgeDiff < MinParentAge) %>%
    #   summarise(Count = n()) %>%
    #   pull(Count)
    #
    # TooOld <- InitialAgeMatch %>%
    #   filter(AgeDiff > MaxParentAge) %>%
    #   summarise(Count = n()) %>%
    #   pull(Count)
    #
    # NumToSwap <- min(max(TooYoung, TooOld), floor(nrow(InitialAgeMatch)/2))
    #
    # i = i + 1

    return(fixedDF)

  # }
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

  # Child ID variable
  ChildIDColName <- sym(names(Children[ChildIDVariable]))

  # Child age variable
  ChildAgeColName <- sym(names(Children[ChildAgeVariable]))

  # Parent age variable
  ParentAgeColName <- sym(names(Parents[ParentAgeVariable]))

  minChildAge <- min(Children[ChildAgeVariable])

  maxChildAge <- max(Children[ChildAgeVariable])

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

  # restrict parents data to the maximum and minimum parents ages for matching
  Parents <- Parents %>%
    filter(between({{ParentAgeColName}}, MinParentAge+minChildAge, MaxParentAge+maxChildAge))

  # get counts for each single age from the parent data frame
  ParentCounts <- Parents %>%
    group_by_at(ParentAgeVariable) %>%
    summarise(AgeCount=n())

  # restrict available counts if MinPropRemain has a value

  if (!(is.null(MinPropRemain))) {

    ParentCounts <- ParentCounts %>%
      mutate(AgeCount = floor(AgeCount*(1-MinPropRemain)))
  }

  # remove rows from the ParentCounts data frame that are 0, as these relate to counts < 2
  # this doesn't affect the total counts as 0 doesn't add anything to the sum()

  ParentCounts <- ParentCounts %>%
    filter(AgeCount != 0)

  # reduce working parent data frame to remaining ages
  # then sample 1-MinPropRemain from each age

  Parents <- Parents %>%
    left_join(ParentCounts, by = names(Parents[ParentAgeVariable])) %>%
    group_by({{ParentAgeColName}}) %>%
    sample_n(AgeCount[1])


  ParentAges <- pull(ParentCounts[1])
  ParentAgeCounts <- pull(ParentCounts[2])

  # set up bins for iterations

     min_bin <- MinParentAge - .5
     max_bin <- MaxParentAge + .5

     bins <- c(min_bin:max_bin)


  # # construct the probabilities for each bin, gives n(bins)-1
  Probabilities <- ptrunc(bins[-1], "lnorm", a = MinParentAge, b = MaxParentAge,
                          meanlog = meanlogUsed, sdlog = sdlogUsed) -
    ptrunc(bins[-length(bins)], "lnorm", a = MinParentAge, b = MaxParentAge,
           meanlog = meanlogUsed, sdlog = sdlogUsed)

  logProb <- c(log(Probabilities[-c(1, length(Probabilities))]))
  logBins <- bins


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


  CurrentAgeMatch <- data.frame(Children[ChildIDVariable],
                                Children[ChildAgeVariable],
                                ParentAge = sample(rep(ParentAges, ParentAgeCounts),
                                                   size=nrow(Children),
                                                   replace = FALSE))

  # fix any extreme age difference values in CurrentAgeMatch by swapping minimum and maximum extremes
  # extremes are differences outside the MinParentAge and MaxParentAge values
  # the minimum and maximum age differences MUST fit inside the probability bins
  # otherwise the entire function falls over with an error relating to the ages being outside the
  # histogram

  CurrentAgeMatch <- CurrentAgeMatch %>%
    mutate(AgeDiff = ParentAge - .[[2]]) %>%
    arrange(AgeDiff) %>%
    mutate(row_no = row_number())

  # then test if any diffs are outside the required age range

  TooYoung <- CurrentAgeMatch %>%
    filter(AgeDiff < MinParentAge) %>%
    summarise(Count = n()) %>%
    pull(Count)

  TooOld <- CurrentAgeMatch %>%
    filter(AgeDiff > MaxParentAge) %>%
    summarise(Count = n()) %>%
    pull(Count)

  NumToSwap <- min(max(TooYoung, TooOld), floor(nrow(CurrentAgeMatch)/2))


  if(NumToSwap > 0) {

    CurrentAgeMatch <- fixExtremes(CurrentAgeMatch)

    return(CurrentAgeMatch)

    }

  print("it got to output")





  # # set up for chi-squared test
  # ExpectedAgeProbs <- Probabilities * nrow(CurrentAgeMatch)
  # logEAgeProbs <- logProb + log(nrow(CurrentAgeMatch))
  #
  # # construct starting set of observed age difference values for iteration
  # ObservedAgeDifferences <- hist(CurrentAgeMatch[,3] - CurrentAgeMatch[,2], breaks = bins, plot=FALSE)$counts
  #
  #
  # # set up for chi-squared
  # log0ObservedAges <- hist(CurrentAgeMatch[,3] - CurrentAgeMatch[,2], breaks = logBins, plot=FALSE)$counts
  # logKObservedAges = ifelse(log0ObservedAges == 0, 2*logEAgeProbs,
  #                           log((log0ObservedAges - exp(logEAgeProbs))^2)) - logEAgeProbs
  # log_chisq = max(logKObservedAges) + log(sum(exp(logKObservedAges - max(logKObservedAges))))
  #
  #
  #
  # if(is.null(pValueToStop)) {
  #
  #   Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
  #
  # } else {
  #
  #   Critical_log_chisq <- log(qchisq(pValueToStop, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
  #
  # }
  #
  #
  # #     #####################################
  # #     #####################################
  # #     # iteration for matching child to parent ages starts here
  # #     #####################################
  # #     #####################################
  # # #  ChiSquareValuesCreated <- data.frame()
  # #
  # for (i in 1:NumIterations) {
  #
  #   # randomly choose two pairs
  #   Pick1 <- sample(nrow(CurrentAgeMatch), 1)
  #   Pick2 <- sample(nrow(CurrentAgeMatch), 1)
  #   Current1 <- CurrentAgeMatch[Pick1,]
  #   Current2 <- CurrentAgeMatch[Pick2,]
  #
  #   # # proposed pairing after a swap
  #   PropPair1 <- swap_parent(Current1, Current2)
  #   PropPair2 <- swap_parent(Current2, Current1)
  #
  #   # compute change in Chi-squared value from current pairing to proposed pairing
  #   PropAgeMatch <- CurrentAgeMatch %>%
  #     filter(!({{ChildIDColName}} %in% c(PropPair1[,1], PropPair2[,1]))) %>%
  #     bind_rows(., PropPair1,PropPair2)
  #
  #   # do chi-squared
  #   Proplog0 <- hist(PropAgeMatch[,3] - PropAgeMatch[,2], breaks = logBins, plot=FALSE)$counts
  #   ProplogK = ifelse(Proplog0 == 0, 2*logEAgeProbs, log((Proplog0 - exp(logEAgeProbs))^2)) - logEAgeProbs
  #
  #   prop_log_chisq = max(ProplogK) + log(sum(exp(ProplogK - max(ProplogK))))
  #
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
  #
  #
  #   }
  #
  #
  #
  #   #   ChiSquareValuesCreated <- c(ChiSquareValuesCreated, log_chisq)
  #
  #   if (log_chisq <= Critical_log_chisq) {
  #     break
  #
  #   }
  #
  #
  # }
  #
  #
  #
  # #####################################
  # #####################################
  # # iteration for matching parent-child ages ends here
  # #####################################
  # #####################################
  #
  #
  # #####################################
  # #####################################
  # # pairing the actual parent-child dyads starts here
  # #####################################
  # #####################################
  # # return full donor and recipient rows as matched household pairs
  # # extract ages counts for matching the donors
  # MatchedParentAges <- CurrentAgeMatch %>%
  #   dplyr::select(ParentAge) %>%
  #   group_by(ParentAge) %>%
  #   mutate(ParentAgeCount = row_number()) %>%
  #   ungroup()
  #
  #
  # # generate same AgeCount second ID variable for the donor data
  # # the AgeCount is used to ensure that the first donor with a specific age is matched first
  # # the second donor with a specific age is matched second
  # #     # and so forth
  # ParentsToMatch <- Parents %>%
  #   group_by({{ParentAgeColName}}) %>%
  #   mutate(ParentAgeCount = row_number()) %>%
  #   ungroup()
  #
  # # reduce pool of potentially partnered donors to only those matched to recipients
  # ParentsMatched <- left_join(MatchedParentAges,
  #                             rename_at(ParentsToMatch, ParentAgeVariable, ~ names(MatchedParentAges)[1],
  #                                       ParentsToMatch, ParentAgeVariable, ~ names(MatchedParentAges)[2]),
  #                             by = c(names(MatchedParentAges)[1], "ParentAgeCount")) %>%
  #   mutate(!!ParentAgeColName := ParentAge)
  #
  #
  # # construct same file for the children
  # # need both parent age and parent age count so that the join between the children and the parents works
  # # do not need child age as this will be a duplicate column on the merge
  # ChildrenMatchPrep <- CurrentAgeMatch %>%
  #   group_by(ParentAge) %>%
  #   mutate(ParentAgeCount = row_number()) %>%
  #   dplyr::select(-c(2)) %>%
  #   ungroup()
  #
  # ChildrenReadyToMatch <- left_join(Children, ChildrenMatchPrep, by = c(names(Children[ChildIDVariable])))
  #
  #
  #
  # # now merge the full data of the subset donors to the recipients
  # # by parent age and parent age count
  # # children data frame is the one to which observations must be joined
  # # also add the household numbers at this point
  # MaxDyadIDValue <- (nrow(ChildrenReadyToMatch)-1) + DyadIDValue
  #
  # FullMatchedDataFrame <- left_join(ChildrenReadyToMatch, ParentsMatched, by=c("ParentAge", "ParentAgeCount")) %>%
  #   dplyr::select(-ParentAge, -ParentAgeCount) %>%
  #   ungroup() %>%
  #   mutate({{HouseholdNumVariable}} := seq(DyadIDValue, MaxDyadIDValue))
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
  #
  # #####################################
  # #####################################
  # # pairing the parents to children ends here
  # #####################################
  # #####################################
  # print(i)
  # print(Critical_log_chisq)
  # print(log_chisq)
  #
  # return(OutputDataframe)

}

