#' Construct pairs of people into couples with a household identifier.
#' This function creates a data frame of couples, based on a distribution of age differences. The function will use either a skew normal or normal distribution, depending on whether a skew ("alphaused") parameter is provided. The default value for the skew is 0, and using the default will cause a normal distribution to be used.
#' Two data frames are required. One person from each data frame will be matched, based on the age difference distribution specified. If the data frames are different sizes, the smalldf data frame must be the smaller of the two.In this situation, a random subsample of the largedf data frame will be used.
#' Both data frames must be restricted to only those people that will have a couples match performed.
#' @export
#' @param smalldf A data frame containing one set of observations to be paired.
#' @param smlidcol The column number for the ID variable in the smalldf data frame.
#' @param smlagecol The column number for the age variable in the smalldf data frame.
#' @param largedf A data frame containing the second set of observations to be paired.
#' @param lrgidcol The column number for the ID variable in the largedf data frame.
#' @param lrgagecol The column number for the age variable in the largedf data frame.
#' @param directxi If a skew-normal distribution is used, this is the location value. If the default alphaused value of 0 is used, this defaults to the mean value for the normal distribution.
#' @param directomega If a skew-normal distribution is used, this is the scale value. If the default alphaused value of 0 is used, this defaults to the standard deviation value for the normal distribution.
#' @param alphaused The skew. If a normal distribution is to be used, this can be omitted as the default value is 0 (no skew).
#' @param hhidstart The starting number for generating the household identifier value that identifies a couple. Must be numeric.
#' @param hhidvar The column name for the household variable. This must be supplied in quotes.
#' @param ptostop The critical p-value stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param numiters The maximum number of iterations used to construct the coupled data frame. The default value is 1000000, and is the stopping rule if the algorithm does not converge.

#' @return A list of two data frames $Matched contains the data frame of pairs. $Unmatched contains the unmatched observations from largedf. If there are no unmatched people, $Unmatched will be an empty data frame.
#'
#' @examples
# library(dplyr)
#
# set.seed(1)
# PartneredFemales <- Township %>%
#   filter(Sex == "Female", Relationship == "Partnered")
# PartneredMales <- Township %>%
#   filter(Sex == "Male", Relationship == "Partnered")
#
# partners females and males, using a normal distribution, with the females being younger by a mean of -2 and a standard deviation of 3
# OppSexCouples <- couples(PartneredFemales, smlidcol=3, smlagecol=4,
#                            PartneredMales, lrgidcol=3, lrgagecol=4, directxi = -2,
#                            directomega = 3, hhidstart = 100, hhidvar="HouseholdID",
#                            UserSeed = 4, ptostop=.01,  numiters=1000000)


couples <- function(smalldf, smlidcol=NULL, smlagecol=NULL, largedf, lrgidcol=NULL,
                    lrgagecol=NULL, directxi=NULL, directomega=NULL, alphaused=0, hhidstart = NULL,
                    hhidvar=NULL, UserSeed=NULL, ptostop=NULL, numiters=1000000) {

  # content check
  if (!any(duplicated(smalldf[smlidcol])) == FALSE) {
    stop("The column number for the ID variable in the smalldf data frame must be supplied.")
  }

  if (!is.numeric(smlagecol)) {
    stop("Both the smalldf ID and the smalldf age column numbers must be supplied.")
  }

  if (!any(duplicated(largedf[lrgidcol])) == FALSE) {
    stop("The column number for the ID variable in the largedf data frame must be supplied.")
  }

  if(is.null(hhidvar)) {
    stop("A name for the household count variable must be supplied.")
  }

  if(is.null(directomega) | directomega < 0) {
    stop("The mean age difference must be greater than zero.")
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
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  # smalldf ID variable
  smlidcolName <- sym(names(smalldf[smlidcol]))
  # smalldf age variable
  smlagecolName <- sym(names(smalldf[smlagecol]))

  # largedf ID variable
  lrgidcolName <- sym(names(largedf[lrgidcol]))
  # largedf age variable
  lrgagecolName <- sym(names(largedf[lrgagecol]))

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

  # get counts for each single age from the largedf data frame
  largedfCounts <- largedf %>%
    group_by_at(lrgagecol) %>%
    summarise(AgeCount=n())

  # largedfAges <- as.vector(largedfCounts[1])
  largedfAges <- pull(largedfCounts[1])
  largedfAgeCounts <- pull(largedfCounts[2])

  # set up bins for iterations
  # enable at least some extreme age differences to be assigned to the Inf categories
  # otherwise the bins will be wrong

  MaxAgeDifference <-  (max(smalldf[smlagecol]) -
                          min(largedf[lrgagecol]))-5

  # cat("Starting the bins", "\n")

  # estimate expected minimum and maximum ages from the distribution, and bin these
  min_bin <- round(sn::qsn(0.000001,xi=directxi, omega=directomega, alpha=alphaused))-0.5
  max_bin <- round(sn::qsn(0.999999,xi=directxi, omega=directomega, alpha=alphaused))+0.5

  # cat("Error when trying to bin", "\n")

  bins <- c(-9999, min_bin:max_bin, 9999)

  # cat("Error after making the bins", "\n")

  # construct the probabilities for each bin, gives n(bins)-1
  Probabilities <- sn::psn(bins[-1], xi=directxi, omega=directomega, alpha=alphaused) -
    sn::psn(bins[-length(bins)], xi=directxi, omega=directomega, alpha=alphaused)

  # cat("Error after making the probabilities", "\n")

  # assign realistic expected probabilities in the bins outside the bins constructed earlier
  # use minAge and maxAge for this, only need range for included ages
  # Uses midpoint rule.
  logProbLow <- sn::dsn(-MaxAgeDifference:(min_bin-0.5), xi=directxi, omega=directomega, alpha=alphaused, log=TRUE)
  logProbHigh <- sn::dsn((max_bin+0.5):MaxAgeDifference, xi=directxi, omega=directomega, alpha=alphaused, log=TRUE)

  logProb <- c(logProbLow, log(Probabilities[-c(1, length(Probabilities))]), logProbHigh)
  logBins    <- c(-Inf, -(MaxAgeDifference-.5):(MaxAgeDifference-.5), Inf)

  # cat("Error after making the logProb and logBins", "\n")

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
  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }


  CurrentAgeMatch <- data.frame(smalldf[smlidcol],
                                smalldf[smlagecol],
                                largedfAge = sample(rep(largedfAges, largedfAgeCounts),
                                                  size=nrow(smalldf),
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
      filter(!({{smlidcolName}} %in% c(PropPair1[,1], PropPair2[,1]))) %>%
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
  # return full largedf and smalldf rows as matched household pairs
  # extract ages counts for matching the largedfs
  MatchedlargedfAges <- CurrentAgeMatch %>%
    dplyr::select(largedfAge) %>%
    group_by(largedfAge) %>%
    mutate(largedfAgeCount = row_number()) %>%
    ungroup()


  #   # generate same AgeCount second ID variable for the largedf data
  #   # the AgeCount is used to ensure that the first largedf with a specific age is matched first
  #   # the second largedf with a specific age is matched second
  #   # and so forth
  largedfsToMatch <- largedf %>%
    group_by({{lrgagecolName}}) %>%
    mutate(largedfAgeCount = row_number()) %>%
    ungroup()

  # reduce pool of potentially partnered largedfs to only those matched to smalldfs
  largedfsMatched <- left_join(MatchedlargedfAges,
                             rename_at(largedfsToMatch, lrgagecol, ~ names(MatchedlargedfAges)[1]),
                             by = c(names(MatchedlargedfAges)[1], "largedfAgeCount")) %>%
    mutate(!!lrgagecolName := largedfAge)


  # construct same file for the smalldfs
  # need both largedf age and largedf age count so that the join between the smalldfs and the largedfs works
  # do not need smalldf age as this will be a duplicate column on the merge
  smalldfsMatchPrep <- CurrentAgeMatch %>%
    group_by(largedfAge) %>%
    mutate(largedfAgeCount = row_number()) %>%
    dplyr::select(-c(2))

  #
  smalldfsReadyToMatch <- left_join(smalldf, smalldfsMatchPrep, by = names(smalldf[smlidcol]))

  # now merge the full data of the subset largedfs to the smalldfs
  # by largedf age and largedf age count
  # smalldf data frame is the one to which observations must be joined
  # also add the household numbers at this point
  Maxhhidstart <- (nrow(smalldfsReadyToMatch)-1) + hhidstart

  FullMatchedDataFrame <- left_join(smalldfsReadyToMatch, largedfsMatched, by=c("largedfAge", "largedfAgeCount")) %>%
    dplyr::select(-largedfAge, -largedfAgeCount) %>%
    ungroup() %>%
    mutate({{hhidvar}} := seq(hhidstart, Maxhhidstart))

  # convert from wide to long, use .x and .y to do the split

  FirstDataframeSplit <- FullMatchedDataFrame %>%
    dplyr::select(ends_with(".x"), {{hhidvar}}) %>%
    rename_all(list(~gsub("\\.x$", "", .)))

  SecondDataframeSplit <- FullMatchedDataFrame %>%
    dplyr::select(ends_with(".y"), {{hhidvar}}) %>%
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

  # if(nrow(largedf) > nrow(smalldf)) {
  #
  #   cat("The second dataframe contained more observations than the first dataframe.", "\n")
  #   cat("A merged dataframe has been returned as a list.", "\n")
    cat("The individual dataframes are $Matched and $Unmatched.", "\n")

    MatchedIDs <- OutputDataframe %>%
      pull({{lrgidcolName}})

    UnmatchedDataframe <- largedf %>%
      filter(!({{lrgidcolName}} %in% MatchedIDs)) #%>%
     # mutate({{hhidvar}} = NA)

    MergedList <- list()

    MergedList$Matched <- OutputDataframe
    MergedList$Unmatched <- UnmatchedDataframe

    return(MergedList)

}
