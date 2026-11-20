#' @importFrom dplyr filter group_by mutate pull rename select summarise ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data as_string sym !!
# '
#' Pairs people from youngdf to people in olderdf, creating pairs in households.
#'
#' This function creates a data frame of paired people, based on a log-normal distribution of age differences.
#' Two data frames are required. One person from each data frame will be matched, based on the age difference distribution specified. If the minimum age difference is more than 0 years, use the locationP parameter to specify the minimum difference.
#'
#' Both data frames must be restricted to only those people that can be paired.
#' @export
#' @param youngerdf The data frame containing the youngest people to be paired.
#' @param yngrlid The variable containing the unique ID for each person, in the youngerdf data frame.
#' @param yngrage The age variable, in the youngerdf data frame.
#' @param olderdf A data frame containing the oldest people to be paired.
#' @param oldrid The variable containing the unique ID for each person, in the olderdf data frame.
#' @param oldrage The age variable, in the olderdf data frame.
#' @param lnmean The mean value for the log-normal distribution.
#' @param lnsd The standard deviation value for the log-normal distribution.
#' @param locationP The minimum age difference permitted. The default value is 0.
#' @param HHStartNum The starting value for HHNumVar. Must be numeric.
#' @param HHNumVar The name for the household variable.
#' @param userseed If specified, this will set the seed to the number provided. If not, the normal set.seed() function will be used.
#' @param ptostop The critical p-value stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param numiters The maximum number of iterations used to construct the output data frame ($Matched) containing the couples. The default value is 1000000, and is the stopping rule if the algorithm does not converge.

#' @return A list of two data frames. $Matched contains the data frame of pairs. $Unmatched contains the unmatched observations from the data frame containing the most observations. If there are no unmatched people, $Unmatched will be an empty data frame.
#'
#' @examples
#' library(dplyr)
#'
#' PossibleMothers <- Township %>%
#'   filter(Sex == "Female", Relationship == "Partnered")
#' PossibleChildren <- Township %>%
#'   filter(Age < 16)
#'
#' # minimum age difference between mother and child is 20 years
#' # mean age difference is 28 years (8 = 28 - 20)
#' # standard deviation is 6 years
#'
#' thelnsd <- sqrt(log(1 + (6 / 8)^2))
#' thelnmean <- log(8) - thelnsd^2 / 2

#' Pairs <- pairln(PossibleChildren, yngrid = "ID", yngrage = "Age", PossibleMothers, oldrid = "ID",
#'                            oldrage = "Age", lnmean = thelnmean, lnsd = thelnsd, locationP = 20, HHStartNum = 1,
#'                            HHNumVar = "HouseholdID", userseed = 4, ptostop = 0.05)
#' MothersWithChildren <- Pairs$Matched


pairln <- function(youngerdf, yngrid, yngrage, olderdf, oldrid, oldrage, lnmean=NULL, lnsd=NULL, locationP=0,
                     HHStartNum, HHNumVar, userseed=NULL, ptostop=NULL, numiters=1000000) {





  #####################################
  # check for missing input information
  #####################################

  # tests for first data frame
  if (!yngrid %in% names(youngerdf)) {
    stop("The ID variable in the first data frame does not exist.")
  }

  if (!yngrage %in% names(youngerdf)) {
    stop("The age variable in the first data frame does not exist.")
  }


  # tests for second data frame
  if (!oldrid %in% names(olderdf)) {
    stop("The ID variable in the second data frame does not exist.")
  }

  if (!oldrage %in% names(olderdf)) {
    stop("The age variable in the second data frame does not exist.")
  }


  if(is.null(HHNumVar)) {
    stop("A name for the household count variable must be supplied.")
  }


  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  # youngerdf ID variable
  yngridcolName <- sym(names(youngerdf[yngrid]))
  # youngerdf age variable
  yngragecolName <- sym(names(youngerdf[yngrage]))

  # olderdf ID variable
  oldridcolName <- sym(names(olderdf[oldrid]))
  # olderdf age variable
  oldragecolName <- sym(names(olderdf[oldrage]))

  #####################################
  #####################################
  # end column names
  #####################################
  #####################################

  # more testing

  if (!any(duplicated(youngerdf[[yngridcolName]])) == FALSE) {
    stop("The ID variable in the first data frame has duplicated values.")
  }

  if (!any(duplicated(olderdf[[oldridcolName]])) == FALSE) {
    stop("The ID variable in the second data frame has duplicated values.")
  }

  if (!is.numeric(youngerdf[[yngragecolName]])) {
    stop("The age variable in the first data frame is not numeric.")
  }

  if (!is.numeric(olderdf[[oldragecolName]])) {
    stop("The age variable in the first second frame is not numeric.")
  }

  #####################################
  #####################################
  #####################################
  #####################################
  # pairing swap subfunction

  swap_pairs <- function(pair1, pair2) {
    swap <- pair1

    if(Largest == "Older") {
      swap$olderdfAge <- pair2$olderdfAge
    } else {
      swap$youngerdfAge <- pair2$youngerdfAge
    }

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

 # cat("subfunctions ended \n")

  #####################################
  #####################################
  # set up pre-data information for matching
  #####################################
  #####################################

  # set up bins for iterations
  # enable at least some extreme age differences to be assigned to the Inf categories
  # otherwise the bins will be wrong

      MaxAgeDifference <-  (max(olderdf[[oldragecolName]]) -
                            min(youngerdf[[yngragecolName]]))-5



  # cat("Starting the bins", "\n")

  # estimate expected minimum and maximum ages from the distribution, and bin these

    min_bin <- locationP-0.5
    max_bin <- qlnorm(1-(1/100000), meanlog=lnmean, sdlog=lnsd)+locationP + 0.5

    cat("minbin is ", min_bin, " and max bin is ", max_bin, "\n")

    # cat("Error when trying to bin", "\n")

    bins <- c(min_bin:max_bin)

    # cat("Error after making the bins", "\n")

    # construct the probabilities for each bin, gives n(bins)-1
    Probabilities <- plnorm(bins[-1] - locationP, meanlog=lnmean, sdlog=lnsd) -
      plnorm(bins[-length(bins)] - locationP, meanlog=lnmean, sdlog=lnsd)

    # cat("Error after making the probabilities", "\n")

    # assign realistic expected probabilities in the bins outside the bins constructed earlier
    # use minAge and maxAge for this, only need range for included ages
    # Uses midpoint rule.

    logProb <- c(log(Probabilities))
    logBins <- c(min_bin:max_bin)





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
  # however, if the olderdf data frame is larger than the youngerdf data frame
  # this ensures that a random selection of olderdfs has the correct count
  if (!is.null(userseed)) {
    set.seed(userseed)
  }

    # sample from data frame with largest number of people

    if(nrow(youngerdf) > nrow(olderdf)) {

      Largest <- "Younger"

      # get counts for each single age from the youngerdf data frame
      youngerdfCounts <- youngerdf %>%
        group_by(across(!!yngragecolName)) %>%
        summarise(AgeCount=n())

      youngerdfAges <- pull(youngerdfCounts[1])
      youngerdfAgeCounts <- pull(youngerdfCounts[2])

      CurrentAgeMatch <- olderdf %>%
        select(!!oldridcolName, !!oldragecolName) %>%
        mutate(youngerdfAge = sample(rep(youngerdfAges, youngerdfAgeCounts),
                                     size=nrow(olderdf),
                                     replace = FALSE)) %>%
        rename(olderdfAge = !!oldragecolName)

      return(CurrentAgeMatch)


    } else {

      Largest <- "Older"

      # get counts for each single age from the olderdf data frame
      olderdfCounts <- olderdf %>%
        group_by(across(!!oldragecolName)) %>%
        summarise(AgeCount=n())

      olderdfAges <- pull(olderdfCounts[1])
      olderdfAgeCounts <- pull(olderdfCounts[2])

      CurrentAgeMatch <- youngerdf %>%
        select(!!yngridcolName, !!yngragecolName) %>%
        mutate(olderdfAge = sample(rep(olderdfAges, olderdfAgeCounts),
                                   size=nrow(youngerdf),
                                   replace = FALSE)) %>%
        rename(youngerdfAge = !!yngragecolName)


    }


  # set up for chi-squared test
  ExpectedAgeProbs <- Probabilities * nrow(CurrentAgeMatch)
  logEAgeProbs <- logProb + log(nrow(CurrentAgeMatch))

  # construct starting set of observed age difference values for iteration
  ObservedAgeDifferences <- hist(CurrentAgeMatch$olderdfage - CurrentAgeMatch$youngerdfage, breaks = bins,
                                 plot=FALSE)$counts

  # set up for chi-squared
  log0ObservedAges <- hist(CurrentAgeMatch$olderdfage - CurrentAgeMatch$youngerdfage, breaks = logBins,
                           plot=FALSE)$counts
  logKObservedAges = ifelse(log0ObservedAges == 0, 2*logEAgeProbs,
                            log((log0ObservedAges - exp(logEAgeProbs))^2)) - logEAgeProbs
  log_chisq = max(logKObservedAges) + log(sum(exp(logKObservedAges - max(logKObservedAges))))

  # print(log_chisq)

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
    PropPair1 <- swap_pairs(Current1, Current2)
    PropPair2 <- swap_pairs(Current2, Current1)

    print(Current1)
    print(Current2)
    print(PropPair1)
    print(PropPair2)

    # compute change in Chi-squared value from current pairing to proposed pairing

    if(Largest == "Younger") {


      PropAgeMatch <- CurrentAgeMatch %>%
        filter(!(!!yngridcolName %in%
                   c(PropPair1[[rlang::as_string(yngridcolName)]],
                     PropPair2[[rlang::as_string(yngridcolName)]]))) %>%
        bind_rows(PropPair1, PropPair2)

    } else {

      PropAgeMatch <- CurrentAgeMatch %>%
        filter(!(!!oldridcolName %in%
                   c(PropPair1[[rlang::as_string(oldridcolName)]],
                     PropPair2[[rlang::as_string(oldridcolName)]]))) %>%
        bind_rows(PropPair1, PropPair2)

      }

    # do chi-squared
    Proplog0 <- hist(PropAgeMatch$olderdfage - PropAgeMatch$youngerdfage, breaks = logBins, plot=FALSE)$counts
    ProplogK = ifelse(Proplog0 == 0, 2*logEAgeProbs, log((Proplog0 - exp(logEAgeProbs))^2)) - logEAgeProbs

    prop_log_chisq = max(ProplogK) + log(sum(exp(ProplogK - max(ProplogK))))

    cat("Current:", log_chisq, "Proposed:", prop_log_chisq, "\n")

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

  cat("The critical chi-squared value is", round(Critical_log_chisq,6), "\n")
  cat(i, "iterations were used, and the final chi-squared value was", round(log_chisq,6), "\n")

  return()

  #####################################
  #####################################
  # iteration for matching ages ends here
  #####################################
  #####################################


  #####################################
  #####################################
  # pairing  starts here
  #####################################
  #####################################
  # return full olderdf and youngerdf rows as matched household pairs
  # extract ages counts for matching the olderdfs
  MatchedolderdfAges <- CurrentAgeMatch %>%
    dplyr::select(olderdfAge) %>%
    group_by(olderdfAge) %>%
    mutate(olderdfAgeCount = row_number()) %>%
    ungroup()


  #   # generate same AgeCount second ID variable for the olderdf data
  #   # the AgeCount is used to ensure that the first olderdf with a specific age is matched first
  #   # the second olderdf with a specific age is matched second
  #   # and so forth
  olderdfsToMatch <- olderdf %>%
    group_by(!!oldragecolName) %>%
    mutate(olderdfAgeCount = row_number()) %>%
    ungroup()


  # reduce pool of potentially partnered olderdfs to only those matched to youngerdfs

  # do the rename first and then join
  olderdfsToMatch <- olderdfsToMatch %>%
    rename(olderdfAge = {{oldragecolName}})

  olderdfsMatched <- left_join(MatchedolderdfAges, olderdfsToMatch, by = c("olderdfAge", "olderdfAgeCount")) %>%
    mutate(!!oldragecolName := olderdfAge)




  # construct same file for the youngerdfs
  # need both olderdf age and olderdf age count so that the join between the youngerdfs and the olderdfs works
  # do not need youngerdf age as this will be a duplicate column on the merge

  youngerdfsMatchPrep <- CurrentAgeMatch %>%
    group_by(olderdfAge) %>%
    mutate(olderdfAgeCount = row_number()) %>%
    dplyr::select(-c(2))


  youngerdfsReadyToMatch <- suppressMessages(left_join(youngerdf, youngerdfsMatchPrep))


  # now merge the full data of the subset olderdfs to the youngerdfs
  # by olderdf age and olderdf age count
  # youngerdf data frame is the one to which observations must be joined
  # also add the household numbers at this point


  MaxHHStartNum <- (nrow(youngerdfsReadyToMatch)-1) + HHStartNum



  FullMatchedDataFrame <- left_join(youngerdfsReadyToMatch, olderdfsMatched, by=c("olderdfAge", "olderdfAgeCount")) %>%
    dplyr::select(-olderdfAge, -olderdfAgeCount) %>%
    ungroup() %>%
    mutate({{HHNumVar}} := seq(HHStartNum, MaxHHStartNum))

  # convert from wide to long, use .x and .y to do the split

  FirstDataframeSplit <- FullMatchedDataFrame %>%
    dplyr::select(ends_with(".x"), {{HHNumVar}}) %>%
    rename_with(list(~gsub("\\.x$", "", .)))

  SecondDataframeSplit <- FullMatchedDataFrame %>%
    dplyr::select(ends_with(".y"), {{HHNumVar}}) %>%
    rename_with(list(~gsub("\\.y$", "", .)))

  OutputDataframe <- rbind(FirstDataframeSplit, SecondDataframeSplit)


  # #####################################
  # #####################################
  # # pairing the actual couples ends here
  # #####################################
  # #####################################

  # use for checking number of iterations used, the p-value to stop, and the p-value reached
  #
  # cat("The number of iterations used was", i, "\n")
  # print(Critical_log_chisq)
  # print(log_chisq)

  # if(nrow(olderdf) > nrow(youngerdf)) {
  #
  #   cat("The second dataframe contained more observations than the first dataframe.", "\n")
  #   cat("A merged dataframe has been returned as a list.", "\n")
  cat("The individual dataframes are $Matched and $Unmatched.", "\n")

  MatchedIDs <- OutputDataframe %>%
    pull({{oldridcolName}})

  UnmatchedDataframe <- olderdf %>%
    filter(!({{oldridcolName}} %in% MatchedIDs)) #%>%
  # mutate({{HHNumVar}} = NA)

  MergedList <- list()

  MergedList$Matched <- OutputDataframe
  MergedList$Unmatched <- UnmatchedDataframe

  return(MergedList)

}
