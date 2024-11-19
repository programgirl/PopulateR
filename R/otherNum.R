#' @importFrom data.table :=
#' @importFrom dplyr bind_rows filter left_join rename select slice_sample ungroup
#' @importFrom graphics hist
#' @importFrom magrittr %>%
#' @importFrom stats pnorm qchisq qnorm dnorm
#' @importFrom rlang sym !!
NULL

#' Match people into existing households
#'
#' Creates a data frame of household inhabitants, with the specified number of inhabitants.
#' Two data frames are required. The 'existing' data frame contains the people already in households. The 'additions' data frame contains the people. The use of an age distribution for the matching ensures that an age structure is present in the households. A less correlated age structure can be produced by entering a larger standard deviation.
#' The output data frame of matches will only contain households of the required size.
#'
#' @export
#' @param existing A data frame containing the people already in households.
#' @param exsid The variable containing the unique ID for each person, in the existing data frame.
#' @param exsage The age variable, in the existing data frame.
#' @param HHNumVar The household identifier variable. This must exist in only one data frame.
#' @param additions A data frame containing the people to be added to the existing households.
#' @param addid The variable containing the unique ID for each person, in the additions data frame.
#' @param addage The age variable, in the additions data frame.
#' @param numadd The number of people to be added to the household.
#' @param sdused The standard deviation of the normal distribution for the distribution of ages in a household.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param attempts The number of times the function will randomly change two matches to improve the fit.
#' @param numiters The maximum number of iterations used to construct the household data frame. This has a default value of 10000, and is the stopping rule if the algorithm does not converge.
#'
#' @return A list of three data frames $Matched contains the data frame of households containing matched people. All households will be of the specified size. $Existing, if populated, contains the excess people in the existing data frame, who could not be allocated additional people. $Additions, if populated, contains the excess people in the additions data frame who could not be allocated to an existing household.
#'
#' @examples
#'
#' library("dplyr")
#'
#' AdultsID <- IntoSchools %>%
#' filter(Age > 20) %>%
#' select(-c(SchoolStatus, SexCode))
#' set.seed(2)
#' NoHousehold <- Township %>%
#'   filter(Age > 20, Relationship == "NonPartnered", !(ID %in% c(AdultsID$ID))) %>%
#'   slice_sample(n = 1500)
#'
#'# toy example with few interations
#' OldHouseholds <- otherNum(AdultsID, exsid = "ID", exsage = "Age", HHNumVar = "HouseholdID",
#'                           NoHousehold, addid = "ID", addage = "Age", numadd = 2, sdused = 3,
#'                           userseed=4, attempts= 10, numiters = 80)
#' CompletedHouseholds <- OldHouseholds$Matched # will match even if critical p-value not met
#' IncompleteHouseholds <- OldHouseholds$Existing # no-one available to match in
#' UnmatchedOthers <- OldHouseholds$Additions # all people not in households were matched}


otherNum <- function(existing, exsid, exsage, HHNumVar = NULL, additions, addid, addage,
                     numadd = NULL, sdused = NULL, userseed=NULL, attempts= 10, numiters = 10000)
{

  withr::local_options(dplyr.summarise.inform=F)

  # content check
  if (is.null(numadd)) {
    stop("The number of people to be added to the household must be supplied.")
  }


  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  # smalldf ID variable
  exsidcolName <- sym(names(existing[exsid]))
  # smalldf age variable
  exsagecolName <- sym(names(existing[exsage]))

  # largedf ID variable
  addidcolName <- sym(names(additions[addid]))
  # largedf age variable
  addagecolName <- sym(names(additions[addage]))



  if (!any(duplicated(existing[HHNumVar])) == FALSE) {
    stop("The column number for the household ID variable in the 'existing' data frame must be supplied, and the household number must be unique to each person")
  }

  if (!any(duplicated(existing[[exsid]])) == FALSE) {
    stop("The column number for the ID variable in the parent data frame must be supplied, and the ID must be unique to each person")
  }

  if (!any(duplicated(additions[[addid]])) == FALSE) {
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
  # only include if a sd is specified
  #####################################
  #####################################

  if(!is.null(sdused) == TRUE) {
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

  MaxAgeDifference1 <-  (max(existing[exsage]) -
                          min(additions[addage]))-5

  MaxAgeDifference2 <-  abs((min(existing[exsage]) -
                           max(additions[addage]))-5)

  # MaxAgeDifference2 <-  (max(additions[addage]) -
  #                          min(existing[exsage]))-5

  MaxAgeDifference <- max(MaxAgeDifference1, MaxAgeDifference2)

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

  # closes if(!is.null(sdused) == TRUE)
  }

  ####################################
  ####################################
  # sub functions end
  ####################################
  ####################################





  #####################################
  #####################################
  # create internal data frames
  #####################################
  #####################################


  existingRenamed <- as.data.frame(existing %>%
    rename(existID = !! exsidcolName,
           existAge = !! exsagecolName,
           internalHHID = !! HHNumVar) %>%
    ungroup())


  additionsRenamed <- as.data.frame(additions %>%
    rename(addID = !! addidcolName,
           addAge = !! addagecolName) %>%
    ungroup())

  existingOriginal <- existingRenamed

  additionsOriginal <- additionsRenamed





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

  NumExisting <- as.numeric(nrow(existingRenamed))

  NumNeeded <- NumExisting * numadd

  NumProvided <- as.numeric(nrow(additionsRenamed))

  if(NumNeeded > NumProvided) {

    cat("The additions data frame should contain", NumNeeded, "people but only contains", NumProvided, "\n")

    NumCanUse <- floor(NumProvided / numadd)

    cat(NumCanUse, "will be randomly sampled from the", NumExisting, "people already in households \n")

    existingRenamed <- existingRenamed %>%
      slice_sample(n = NumCanUse)
  }

  if(NumNeeded < NumProvided) {

    cat("The additions data frame should contain", NumNeeded, "people and contains", NumProvided, "\n")

    cat(NumNeeded, "will be randomly sampled from the", NumProvided, "people to add to households \n")

    additionsRenamed <- additionsRenamed %>%
      slice_sample(n = NumNeeded)

  }


  #####################################
  # matching
  #####################################


  BaseSize <- as.numeric((nrow(existingRenamed)))


  for (i in 1: numadd) {

    # if(BaseSize < 1) {
    #   stop("Sample size is less than 1", "\n")
    # }

    MatchingSample <- additionsRenamed %>%
      slice_sample(n = BaseSize, replace = FALSE)

       # cat("MatchingSample size is", nrow(MatchingSample), "\n")
    #
    additionsRenamed <- additionsRenamed %>%
      filter(!(.data$addID %in% MatchingSample$addID))

    # get age differences

    CurrentAgeMatch <- existingRenamed %>%
      select("internalHHID", "existAge", "existID")

    MatchedAgeExtract <- MatchingSample %>%
      select("addAge", "addID")

    CurrentAgeMatch <- cbind(CurrentAgeMatch, MatchedAgeExtract)

    # cat("Current age match is", nrow(CurrentAgeMatch), "Matched age extract is",
    #     nrow(MatchedAgeExtract), "combined age match is", nrow(CurrentAgeMatch), "\n")


    # ONLY ITERATE MATCHES IF THERE IS A SD
    if(!is.null(sdused) == TRUE) {

    ExpectedAgeProbs <- Probabilities * nrow(CurrentAgeMatch)
    logEAgeProbs <- logProb + log(nrow(CurrentAgeMatch))

    ObservedAgeDifferences <- hist(CurrentAgeMatch$existAge - CurrentAgeMatch$addAge,
                                   breaks = bins, plot=FALSE)$counts


    # set up for chi-squared
    log0ObservedAges <- hist(CurrentAgeMatch$existAge - CurrentAgeMatch$addAge,
                             breaks = logBins, plot=FALSE)$counts
    logKObservedAges = ifelse(log0ObservedAges == 0, 2*logEAgeProbs,
                              log((log0ObservedAges - exp(logEAgeProbs))^2)) - logEAgeProbs
    log_chisq = max(logKObservedAges) + log(sum(exp(logKObservedAges - max(logKObservedAges))))

    # print(log_chisq)


    Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))

    cat("The critical chi-squared value is", round(Critical_log_chisq,6), "\n")


    #####################################
    #####################################
    # iteration for matching  ages starts here
    #####################################
    #####################################

    # str(CurrentAgeMatch)

    # cat("Gets to matching age iterations", "\n")

    if(log_chisq > Critical_log_chisq) {

    for (j in 1:numiters) {

      # print(j)


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
        filter(!(.data$existID %in% c(PropPair1[,2], PropPair2[,2])))

      PropAgeMatch <- bind_rows(PropAgeMatch, PropPair1, PropPair2)

      # cat("PropAgeMatch has", nrow(PropAgeMatch), "rows", "\n")

      # do chi-squared
      Proplog0 <- hist(PropAgeMatch$existAge - PropAgeMatch$addAge, breaks = logBins, plot=FALSE)$counts
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

        # cat("log chi-square is", log_chisq, "\n")

        # closes pair swqp

      }



      if (log_chisq <= Critical_log_chisq) {


        break

        }

      # closes for (j in 1:numiters) {
    }

      cat(j, "iterations were used, and the final chi-squared value was", round(log_chisq,6), "\n")

      # closes if(log_chisq > Critical_log_chisq) {
    }

    # closes if(!is.null(sdused) == TRUE) {
    }

    #####################################
    #####################################
    # getting the data frames ready for output
    # this has to be done for EACH set of pairs constructed
    # approach - keep the base data as is
    # remove the added pairs into a separate dataframe
    # only need to do full matching at the end
    #####################################
    #####################################


    if(exists("theMatched")) {


      theMatched <- bind_rows(theMatched, CurrentAgeMatch)

      # no need to delete

    } else {

      theMatched <- CurrentAgeMatch

     # closes else to if(exists("othersMatched")) {
    }

    # closes for (i in 1: numadd) {
    # i.e. closes loop through each addition needed

  }

  # the data frame theMatched is now a long one with a duplicate set of base ids if > 1 person needed matching


  #####################################
  #####################################
  # create dataframes for output
  #####################################
  #####################################

  HHIDcolName <- sym(names(existing[HHNumVar]))

  # the data frame that already had a household ID

  baseMini <- theMatched %>%
    select("existID") %>%
    unique()


  # merge in the extra info
  theOriginal <- left_join(baseMini, existingOriginal, by = c("existID"))

  theOriginal <- theOriginal %>%
    rename(!!exsidcolName := "existID",
           !!exsagecolName := "existAge",
           !!HHIDcolName := "internalHHID")

  # the data frame that contributed new people to the household

  # data for the matched people
   addedMini <- theMatched %>%
     select("addID", "internalHHID")


   # merge in their details
   theMatched <- left_join(addedMini, additionsOriginal, by = c("addID"))

   theMatched <- theMatched %>%
     rename(!!addidcolName := "addID",
            !!addagecolName := "addAge",
            !!HHIDcolName := "internalHHID")


   # merge the two into the matched output data frame

  OutputDataframe <- bind_rows(theOriginal, theMatched)

  cat("The individual dataframes are $Matched, $Existing, and $Additions", "\n")
  cat("$Existing contains unmatched observations from the data frame with a household ID", "\n")
  cat("$Additions contains unmatched observations from the data frame without a household ID", "\n")

  MatchedIDs <- OutputDataframe %>%
    pull({{exsidcolName}})

  UnmatchedExisting <- existing %>%
    filter(!({{exsidcolName}} %in% MatchedIDs))

  # UnmatchedExisting was a tibble, return a data frame
  UnmatchedExisting <- as.data.frame(UnmatchedExisting)

  UnmatchedAdditions <- additions %>%
    filter(!({{addidcolName}} %in% MatchedIDs)) # this filter is not working


  MergedList <- list()

  MergedList$Matched <- OutputDataframe
  MergedList$Existing <- UnmatchedExisting
  MergedList$Additions <- UnmatchedAdditions

  return(MergedList)

}

