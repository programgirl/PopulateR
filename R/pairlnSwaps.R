#' Construct pairs of people into couples with a household identifier.
#' This function creates a data frame of couples, based on a distribution of age differences. The function uses a log-normal distribution.
#' Two data frames are required. One person from each data frame will be matched, based on the age difference distribution specified. If the data frames are different sizes, the smalldf data frame must be the smaller of the two.In this situation, a random subsample of the largedf data frame will be used.
#' Both data frames must be restricted to only those people that will have a couples match performed.
#' @export
#' @param smalldf A data frame containing one set of observations to be paired.
#' @param smlidcol The ID variable in the smalldf data frame.
#' @param smlagecol The age variable in the smalldf data frame.
#' @param largedf A data frame containing the second set of observations to be paired.
#' @param lrgidcol The ID variable in the largedf data frame.
#' @param lrgagecol The age variable in the largedf data frame.
#' @param meanvalue The log mean value to use. If this value is positive, the larger data frame holds the oldest ages. If this value is negative, the smaller data frame holds the oldest ages.
#' @param sdvalue the log standard deviation to use.
#' @param IDStartValue The starting number for generating the household identifier value that identifies a couple. Must be numeric.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied in quotes.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param ptostop The critical p-value stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param numiters The maximum number of iterations used to construct the coupled data frame. The default value is 1000000, and is the stopping rule if the algorithm does not converge.

#' @return A list of two data frames $Matched contains the data frame of pairs. $Unmatched contains the unmatched observations from largedf. If there are no unmatched people, $Unmatched will be an empty data frame.
#'
#' @examples
#' library(dplyr)
#'
#' set.seed(1)
#' PartneredFemales <- Township %>%
#'   filter(Sex == "Female", Relationship == "Partnered")
#' PartneredMales <- Township %>%
#'   filter(Sex == "Male", Relationship == "Partnered")
#'
#' # partners females and males, using a normal distribution, with the females
#' being younger by a mean of -2 and a standard deviation of 3
#' OppSexCouples <- couples(PartneredFemales, smlidcol=3, smlagecol=4,
#'                          PartneredMales, lrgidcol=3, lrgagecol=4, meanvalue = -2,
#'                          sdvalue = 3, IDStartValue = 100, HouseholdNumVariable="HouseholdID",
#'                          userseed = 4, ptostop=.01,  numiters=1000000)


pairlnSwaps <- function(smalldf, smlid, smlage, largedf, lrgid, lrgage, meanvalue=NULL, sdvalue=NULL, 
                     IDStartValue, HouseholdNumVariable, userseed=NULL, ptostop=NULL, numiters=1000000) {
  
  
  
  
  
  #####################################
  # check for missing input information
  #####################################
  
  # tests for first data frame
  if (!smlid %in% names(smalldf)) {
    stop("The ID variable in the first data frame does not exist.")
  }
  
  if (!smlage %in% names(smalldf)) {
    stop("The age variable in the first data frame does not exist.")
  }
  
  
  # tests for second data frame
  if (!lrgid %in% names(largedf)) {
    stop("The ID variable in the second data frame does not exist.")
  }
  
  if (!lrgage %in% names(largedf)) {
    stop("The age variable in the second data frame does not exist.")
  }
  
  
  if(is.null(HouseholdNumVariable)) {
    stop("A name for the household count variable must be supplied.")
  }
  
  if(is.null(sdvalue) | sdvalue < 0) {
    stop("The mean age difference must be greater than zero.")
  }
  
  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################
  
  # smalldf ID variable
  smlidcolName <- sym(names(smalldf[smlid]))
  # smalldf age variable
  smlagecolName <- sym(names(smalldf[smlage]))
  
  # largedf ID variable
  lrgidcolName <- sym(names(largedf[lrgid]))
  # largedf age variable
  lrgagecolName <- sym(names(largedf[lrgage]))
  
  #####################################
  #####################################
  # end column names
  #####################################
  #####################################
  
  # more testing
  
  if (!any(duplicated(smalldf[[smlidcolName]])) == FALSE) {
    stop("The ID variable in the first data frame has duplicated values.")
  }
  
  if (!any(duplicated(largedf[[lrgidcolName]])) == FALSE) {
    stop("The ID variable in the second data frame has duplicated values.")
  }
  
  if (!is.numeric(smalldf[[smlagecolName]])) {
    stop("The age variable in the first data frame is not numeric.")
  }
  
  if (!is.numeric(largedf[[lrgagecolName]])) {
    stop("The age variable in the first second frame is not numeric.")
  }
  
  #####################################
  #####################################
  #####################################
  #####################################
  # pairing swap subfunction
  
  swap_largedf <- function(pair1, pair2) {
    swap <- pair1
    swap$largedfID <- pair2$largedfID
    swap$largedfAge <- pair2$largedfAge
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
  # set up pre-data information for matching
  #####################################
  #####################################
  
  # get counts for each single age from the largedf data frame
  largedfCounts <- largedf %>%
    group_by(across(!!lrgagecolName)) %>%
    summarise(AgeCount=n())
  
  
  # largedfAges <- as.vector(largedfCounts[1])
  largedfAges <- pull(largedfCounts[1])
  largedfAgeCounts <- pull(largedfCounts[2])
  
  # make sure the function is working with a positive meanlog
  if(meanvalue < 0) {
    posMeanValue <- abs(meanvalue)
  } else {
    posMeanValue <- meanvalue
    
    # closes  if(meanvalue < 0) {
  }
  
  # set up bins for iterations
  # enable at least some extreme age differences to be assigned to the Inf categories
  # otherwise the bins will be wrong
  
  # this bit needs to be changed depending on the sign of the mean
  

  # TODO depends on whether meanlog is positive or negative
  if(meanvalue < 0) {
     MaxAgeDifference <-  (max(smalldf[[smlagecolName]]) -
                            min(largedf[[lrgagecolName]]))-5
  } else {
    MaxAgeDifference <-  (max(largedf[[lrgagecolName]]) -
                            min(smalldf[[smlagecolName]]))-5
    # closes if(meanvalue < 0) {
  }
  
  # cat("Starting the bins", "\n")
  
  # estimate expected minimum and maximum ages from the distribution, and bin these
    # use this new meanvalue in the estimates
    # which means estimates work for both smalldf is older or largedf is older
    min_bin <- round(qlnorm(0.000001, meanlog=posMeanValue, sdlog=sdvalue))-0.5
    max_bin <- round(qlnorm(0.999999, meanlog=posMeanValue, sdlog=sdvalue))+0.5
    
    # cat("Error when trying to bin", "\n")
    
    bins <- c(-9999, min_bin:max_bin, 9999)
    
    # cat("Error after making the bins", "\n")
    
    # construct the probabilities for each bin, gives n(bins)-1
    # again, use absolute mean value
    Probabilities <- plnorm(bins[-1], meanlog=posMeanValue, sdlog=sdvalue) -
      plnorm(bins[-length(bins)], meanlog=posMeanValue, sdlog=sdvalue)
    
    # cat("Error after making the probabilities", "\n")
    
    # assign realistic expected probabilities in the bins outside the bins constructed earlier
    # use minAge and maxAge for this, only need range for included ages
    # Uses midpoint rule.
    # absolute mean value
    logProbLow <- dlnorm(-MaxAgeDifference:(min_bin-0.5), meanlog=posMeanValue, sdlog=sdvalue, log=TRUE)
    logProbHigh <- dlnorm((max_bin+0.5):MaxAgeDifference, meanlog=posMeanValue, sdlog=sdvalue, log=TRUE)
    
    logProb <- c(logProbLow, log(Probabilities[-c(1, length(Probabilities))]), logProbHigh)
    logBins    <- c(-Inf, -(MaxAgeDifference-.5):(MaxAgeDifference-.5), Inf)
    
  cat("min_bin is", min_bin, "max_bin is", max_bin, "bins are", "\n")
  print(bins)
  cat("logProbLow is", "\n")
  print(logProbLow)
  cat("Probabilities are", "\n")
  print(Probabilities)
  cat("LogProbHigh is", "\n")
  print(logProbHigh)
  cat("logProb is", "\n")
  print(logProb)
  cat("logBins is", "\n")
  print(logBins)
  return()
  
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
  # however, if the largedf data frame is larger than the smalldf data frame
  # this ensures that a random selection of largedfs has the correct count
  if (!is.null(userseed)) {
    set.seed(userseed)
  }
  
  
  CurrentAgeMatch <- smalldf %>%
    select(!!smlidcolName, !!smlagecolName) %>%
    mutate(largedfAge = sample(rep(largedfAges, largedfAgeCounts),
                               size=nrow(smalldf),
                               replace = FALSE))
  

  # set up for chi-squared test
  ExpectedAgeProbs <- Probabilities * nrow(CurrentAgeMatch)
  logEAgeProbs <- logProb + log(nrow(CurrentAgeMatch))
  
  # construct starting set of observed age difference values for iteration
  # change depending on sign of meanlog
  if(meanvalue < 0) {
    ObservedAgeDifferences <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,3], breaks = bins, plot=FALSE)$counts
    log0ObservedAges <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,3], breaks = logBins, plot=FALSE)$counts
    
  } else {
    ObservedAgeDifferences <- hist(CurrentAgeMatch[,3] - CurrentAgeMatch[,2], breaks = bins, plot=FALSE)$counts
    log0ObservedAges <- hist(CurrentAgeMatch[,3] - CurrentAgeMatch[,2], breaks = logBins, plot=FALSE)$counts
    
    # closes if(meanvalue < 0) {
  }

  # set up for chi-squared
  logKObservedAges = ifelse(log0ObservedAges == 0, 2*logEAgeProbs, log((log0ObservedAges - exp(logEAgeProbs))^2)) -
    logEAgeProbs
  log_chisq = max(logKObservedAges) + log(sum(exp(logKObservedAges - max(logKObservedAges))))
  
  if (is.null(ptostop)) {
    
    Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
    
  } else {
    
    Critical_log_chisq <- log(qchisq(ptostop, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
    
  }
  
  #####################################
  #####################################
  # iteration for matching couple ages starts here
  #####################################
  #####################################
  
  for (i in 1:numiters) {
    
    # print(i)
    
    # randomly choose two pairs
    Pick1 <- sample(nrow(CurrentAgeMatch), 1)
    Pick2 <- sample(nrow(CurrentAgeMatch), 1)
    Current1 <- CurrentAgeMatch[Pick1,]
    Current2 <- CurrentAgeMatch[Pick2,]
    
    # # proposed pairing after a swap
    PropPair1 <- swap_largedf(Current1, Current2)
    PropPair2 <- swap_largedf(Current2, Current1)
    
    # compute change in Chi-squared value from current pairing to proposed pairing
    PropAgeMatch <- CurrentAgeMatch %>%
      filter(!(smalldf[[smlidcolName]] %in% c(PropPair1[,1], PropPair2[,1]))) %>%
      bind_rows(., PropPair1,PropPair2)
    
    # do chi-squared
    # change depending on meanlog sign
    if(meanvalue < 0) {
      Proplog0 <- hist(PropAgeMatch[,2] - PropAgeMatch[,3], breaks = logBins, plot=FALSE)$counts
      
    } else {
      Proplog0 <- hist(PropAgeMatch[,3] - PropAgeMatch[,2], breaks = logBins, plot=FALSE)$counts
      
      # closes if(meanvalue < 0) {
    }
    
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
  # return full largedf and smalldf rows as matched household pairs
  # extract ages counts for matching the largedfs
  MatchedlargedfAges <- CurrentAgeMatch %>%
    dplyr::select(largedfAge) %>%
    group_by(largedfAge) %>%
    mutate(largedfAgeCount = row_number()) %>%
    ungroup()
  
  
    # generate same AgeCount second ID variable for the largedf data
    # the AgeCount is used to ensure that the first largedf with a specific age is matched first
    # the second largedf with a specific age is matched second
    # and so forth
  largedfsToMatch <- largedf %>%
    group_by(!!lrgagecolName) %>%
    mutate(largedfAgeCount = row_number()) %>%
    ungroup()
  
  
  largedfsToMatch <- largedfsToMatch %>%
    rename(largedfAge = {{lrgagecolName}})
  
  largedfsMatched <- left_join(MatchedlargedfAges, largedfsToMatch, by = c("largedfAge", "largedfAgeCount")) %>%
    mutate(!!lrgagecolName := largedfAge)
  
  
  
  
  # construct same file for the smalldfs
  # need both largedf age and largedf age count so that the join between the smalldfs and the largedfs works
  # do not need smalldf age as this will be a duplicate column on the merge
  
  smalldfsMatchPrep <- CurrentAgeMatch %>%
    group_by(largedfAge) %>%
    mutate(largedfAgeCount = row_number()) %>%
    dplyr::select(-c(2))
  
  
  smalldfsReadyToMatch <- suppressMessages(left_join(smalldf, smalldfsMatchPrep))
  
  
  # now merge the full data of the subset largedfs to the smalldfs
  # by largedf age and largedf age count
  # smalldf data frame is the one to which observations must be joined
  # also add the household numbers at this point
  
  
  MaxIDStartValue <- (nrow(smalldfsReadyToMatch)-1) + IDStartValue
  
  
  
  FullMatchedDataFrame <- left_join(smalldfsReadyToMatch, largedfsMatched, by=c("largedfAge", "largedfAgeCount")) %>%
    dplyr::select(-largedfAge, -largedfAgeCount) %>%
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
  cat("The number of iterations used was", i, "\n")
  # print(Critical_log_chisq)
  # print(log_chisq)
  
  # if(nrow(largedf) > nrow(smalldf)) {
  #
  #   cat("The second dataframe contained more observations than the first dataframe.", "\n")
  #   cat("A merged dataframe has been returned as a list.", "\n")
  cat("The individual dataframes are $Matched and $Unmatched.", "\n")
  
  MatchedIDs <- OutputDataframe %>%
    pull({{lrgidcolName}})
  
  UnmatchedDataframe <- largedf %>%
    filter(!({{lrgidcolName}} %in% MatchedIDs)) #%>%
  # mutate({{HouseholdNumVariable}} = NA)
  
  MergedList <- list()
  
  MergedList$Matched <- OutputDataframe
  MergedList$Unmatched <- UnmatchedDataframe
  
  return(MergedList)
  
}
