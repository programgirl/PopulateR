#' @importFrom data.table :=
#' @importFrom dplyr between bind_cols bind_rows filter left_join rename select slice_sample
#' @importFrom magrittr %>%
#' @importFrom rlang sym !!
#' @importFrom sn dsn psn qsn
#' @importFrom stats dnorm pnorm qnorm
#' @importFrom tidyselect ends_with
NULL

#' Pair two people, using either a normal or skew-normal distribution, into households
#'
#' Creates a data frame of couples, based on a distribution of age differences. The function will use either a skew normal or normal distribution, depending on whether a skew ("alphaused") parameter is provided. The default value for the skew is 0, and using the default will cause a normal distribution to be used.
#' Two data frames are required. One person from each data frame will be matched, based on the age difference distribution specified. If the data frames are different sizes, the smalldf data frame must be the smaller of the two. In this situation, a random subsample of the largedf data frame will be used.
#' Both data frames must be restricted to only those people that will have a couples match performed.
#' @export
#' @param smalldf A data frame containing one set of people to be paired. If the two data frames contain different numbers of people, this must be the data frame containing the smallest number.
#' @param smlid The variable containing the unique ID for each person, in the smalldf data frame.
#' @param smlage The age variable, in the smalldf data frame.
#' @param largedf A data frame containing the second set of people to be paired. If the two data frames contain different numbers of people, this must be the data frame containing the largest number.
#' @param lrgid The variable containing the unique ID for each person, in the largedf data frame.
#' @param lrgage The age variable, in the largedf data frame.
#' @param directxi If a skew-normal distribution is used, this is the location value. If the default alphaused value of 0 is used, this defaults to the mean value for the normal distribution.
#' @param directomega If a skew-normal distribution is used, this is the scale value. If the default alphaused value of 0 is used, this defaults to the standard deviation value for the normal distribution.
#' @param alphaused The skew. If a normal distribution is to be used, this can be omitted as the default value is 0 (no skew).
#' @param HHStartNum The starting value for HHNumVar Must be numeric.
#' @param HHNumVar The name for the household variable.
#' @param userseed If specified, this will set the seed to the number provided. If not, the normal set.seed() function will be used.
#' @param ptostop The critical p-value stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param numiters The maximum number of iterations used to construct the output data frame ($Matched) containing the couples. The default value is 1000000, and is the stopping rule if the algorithm does not converge.
#' @param verbose Whether the distribution used, number of iterations used, the critical chi-squared value, and the final chi-squared value are printed to the console. The default value is FALSE.

#' @return A list of two data frames. $Matched contains the data frame of pairs. $Unmatched contains the unmatched observations from largedf. If there are no unmatched people, $Unmatched will be an empty data frame.
#'
#' @examples
#'
#' library(dplyr)
#'
#' # matched dataframe sizes first, using a normal distribution
#' # females younger by a mean of -2 and a standard deviation of 3
#' set.seed(1)
#' PartneredFemales1 <- Township %>%
#'   filter(Sex == "Female", Relationship == "Partnered") %>%
#'   slice_sample(n=120, replace = FALSE)
#' PartneredMales1 <- Township %>%
#'  filter(Sex == "Male", Relationship == "Partnered") %>%
#'  slice_sample(n = nrow(PartneredFemales1), replace = FALSE)
#'
#' # partners females and males, using a normal distribution, with the females
#' # being younger by a mean of -2 and a standard deviation of 3
#' OppSexCouples1 <- pairnorm(PartneredFemales1, smlid = "ID", smlage = "Age", PartneredMales1,
#'                            lrgid = "ID", lrgage = "Age", directxi = -2, directomega = 3,
#'                            HHStartNum = 1, HHNumVar = "HouseholdID", userseed = 4, ptostop=.3)
#' Couples1 <- OppSexCouples1$Matched
#'
#' # different size dataframes
#' # there are more partnered males than partnered females
#' # so all partnered males will have a matched female partner
#' # but not all females will be matched
#' # being the smallest data frame, the female one must be the first
#' #
#' # PartneredFemales2 <- Township %>%
#' #   filter(Sex == "Female", Relationship == "Partnered") %>%
#' #   slice_sample(n=120, replace = FALSE)
#' # PartneredMales2 <- Township %>%
#' #   filter(Sex == "Male", Relationship == "Partnered") %>%
#' #   slice_sample(n=140, replace = FALSE)
#' #
#' # OppSexCouples2 <- pairnorm(PartneredFemales2, smlid = "ID", smlage = "Age", PartneredMales2,
#' #                            lrgid = "ID", lrgage = "Age", directxi = -2, directomega = 3,
#' #                            HHStartNum = 1, HHNumVar="HouseholdID", userseed = 4, ptostop=.3)
#' # Couples2 <- OppSexCouples2$Matched


pairnorm <- function(smalldf, smlid, smlage, largedf, lrgid, lrgage, directxi=NULL, directomega=NULL,
                    alphaused=0, HHStartNum, HHNumVar, userseed=NULL, ptostop=NULL, numiters=1000000,
                    verbose = FALSE) {





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


  if(is.null(HHNumVar)) {
    stop("A name for the household count variable must be supplied.")
  }

  if(is.null(directomega) | directomega < 0) {
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

  # set up bins for iterations
  # enable at least some extreme age differences to be assigned to the Inf categories
  # otherwise the bins will be wrong

  # this bit needs to be changed depending on the sign of the mean

  if(directxi >= 0) {

    MaxAgeDifference <-  (max(smalldf[[smlagecolName]]) -
                            min(largedf[[lrgagecolName]]))-5

  } else {

    MaxAgeDifference <-  (max(largedf[[lrgagecolName]]) -
                            min(smalldf[[smlagecolName]]))-5

    # closes if(directxi >= 0)
  }

  # estimate expected minimum and maximum ages from the distribution, and bin these
  # need to do the bins etc separately for the sn and normal distributions

  if (alphaused==0) {

    if(verbose == TRUE) {
    message("Normal distribution was used", "\n")
    }

    min_bin <- round(qnorm(1/100000, mean=directxi, sd=directomega))-0.5
    max_bin <- round(qnorm(1-(1/100000), mean=directxi, sd=directomega))+0.5


    bins <- c(-9999, min_bin:max_bin, 9999)


    # construct the probabilities for each bin, gives n(bins)-1
    Probabilities <- pnorm(bins[-1], mean=directxi, sd=directomega) -
      pnorm(bins[-length(bins)], mean=directxi, sd=directomega)


    # assign realistic expected probabilities in the bins outside the bins constructed earlier
    # use minAge and maxAge for this, only need range for included ages
    # Uses midpoint rule.
    logProbLow <- dnorm(-MaxAgeDifference:(min_bin-0.5), mean=directxi, sd=directomega, log = TRUE)
    logProbHigh <- dnorm((max_bin+0.5):MaxAgeDifference, mean=directxi, sd=directomega, log = TRUE)

    logProb <- c(logProbLow, log(Probabilities[-c(1, length(Probabilities))]), logProbHigh)
    logBins    <- c(-Inf, -(MaxAgeDifference-.5):(MaxAgeDifference-.5), Inf)

  } else {

    if(verbose == TRUE) {
    message("Skew-normal distribution has been used", "\n")
    }

    min_bin <- round(sn::qsn(1/100000,xi=directxi, omega=directomega, alpha=alphaused))-0.5
    max_bin <- round(sn::qsn(1-(1/100000),xi=directxi, omega=directomega, alpha=alphaused))+0.5


    bins <- c(-9999, min_bin:max_bin, 9999)


    # construct the probabilities for each bin, gives n(bins)-1
    Probabilities <- sn::psn(bins[-1], xi=directxi, omega=directomega, alpha=alphaused) -
      sn::psn(bins[-length(bins)], xi=directxi, omega=directomega, alpha=alphaused)


    # assign realistic expected probabilities in the bins outside the bins constructed earlier
    # use minAge and maxAge for this, only need range for included ages
    # Uses midpoint rule.
    logProbLow <- sn::dsn(-MaxAgeDifference:(min_bin-0.5), xi=directxi, omega=directomega, alpha=alphaused, log = TRUE)
    logProbHigh <- sn::dsn((max_bin+0.5):MaxAgeDifference, xi=directxi, omega=directomega, alpha=alphaused, log = TRUE)

    logProb <- c(logProbLow, log(Probabilities[-c(1, length(Probabilities))]), logProbHigh)
    logBins    <- c(-Inf, -(MaxAgeDifference-.5):(MaxAgeDifference-.5), Inf)


    # closes else for if (alphaused==0)
  }



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
  ObservedAgeDifferences <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,3], breaks = bins, plot = FALSE)$counts

  # set up for chi-squared
  log0ObservedAges <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,3], breaks = logBins, plot = FALSE)$counts
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
      filter(!(smalldf[[smlidcolName]] %in% c(PropPair1[,1], PropPair2[,1])))

    PropAgeMatch <- bind_rows(PropAgeMatch, PropPair1, PropPair2)


    # do chi-squared
    Proplog0 <- hist(PropAgeMatch[,2] - PropAgeMatch[,3], breaks = logBins, plot = FALSE)$counts
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

  if(verbose == TRUE) {
  message(i, "iterations were used, the critical chi-squared value was", round(Critical_log_chisq,3),", and the final chi-squared value is", round(log_chisq,3), "\n")
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
    select("largedfAge") %>%
    group_by(.data$largedfAge) %>%
    mutate(largedfAgeCount = row_number()) %>%
    ungroup()


  #   # generate same AgeCount second ID variable for the largedf data
  #   # the AgeCount is used to ensure that the first largedf with a specific age is matched first
  #   # the second largedf with a specific age is matched second
  #   # and so forth
  largedfsToMatch <- largedf %>%
    group_by(!!lrgagecolName) %>%
    mutate(largedfAgeCount = row_number()) %>%
    ungroup()


  # do the rename first and then join
  largedfsToMatch <- largedfsToMatch %>%
    rename(largedfAge = {{lrgagecolName}})

  largedfsMatched <- left_join(MatchedlargedfAges, largedfsToMatch, by = c("largedfAge", "largedfAgeCount")) %>%
    mutate(!!lrgagecolName := "largedfAge")




  # construct same file for the smalldfs
  # need both largedf age and largedf age count so that the join between the smalldfs and the largedfs works
  # do not need smalldf age as this will be a duplicate column on the merge

  smalldfsMatchPrep <- CurrentAgeMatch %>%
    group_by(.data$largedfAge) %>%
    mutate(largedfAgeCount = row_number()) %>%
    dplyr::select(-c(2))


  smalldfsReadyToMatch <- suppressMessages(left_join(smalldf, smalldfsMatchPrep))


  # now merge the full data of the subset largedfs to the smalldfs
  # by largedf age and largedf age count
  # smalldf data frame is the one to which observations must be joined
  # also add the household numbers at this point


  MaxHHStartNum <- (nrow(smalldfsReadyToMatch)-1) + HHStartNum



  FullMatchedDataFrame <- left_join(smalldfsReadyToMatch, largedfsMatched, by=c("largedfAge", "largedfAgeCount")) %>%
    dplyr::select(- c("largedfAge", "largedfAgeCount")) %>%
    ungroup() %>%
    mutate({{HHNumVar}} := seq(HHStartNum, MaxHHStartNum))

  # convert from wide to long, use .x and .y to do the split

  FirstDataframeSplit <- FullMatchedDataFrame %>%
    dplyr::select(ends_with(".x"), {{HHNumVar}}) %>%
    rename_all(list(~gsub("\\.x$", "", .)))

  SecondDataframeSplit <- FullMatchedDataFrame %>%
    dplyr::select(ends_with(".y"), {{HHNumVar}}) %>%
    rename_all(list(~gsub("\\.y$", "", .)))

   OutputDataframe <- rbind(FirstDataframeSplit, SecondDataframeSplit)


  # #####################################
  # #####################################
  # # pairing the actual couples ends here
  # #####################################
  # #####################################


  MatchedIDs <- OutputDataframe %>%
    pull({{lrgidcolName}})

  UnmatchedDataframe <- largedf %>%
    filter(!({{lrgidcolName}} %in% MatchedIDs))


  MergedList <- list()

  MergedList$Matched <- OutputDataframe
  MergedList$Unmatched <- UnmatchedDataframe

  return(MergedList)

}
