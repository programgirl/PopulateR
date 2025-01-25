#' @importFrom data.table :=
#' @importFrom dplyr bind_rows filter left_join mutate pull rename rename_with select slice_sample
#' @importFrom graphics hist
#' @importFrom magrittr %>%
#' @importFrom stats dnorm qchisq qnorm pnorm
#' @importFrom rlang sym !!
#' @importFrom tidyselect last_col
NULL

#' Match people into new households
#'
#' This function creates a data frame of household inhabitants, with the specified number of inhabitants.
#' One data frame, containing the people to match, is required. The use of an age distribution for the matching ensures that an age structure is present in the households. A less correlated age structure can be produced by entering a larger standard deviation.
#' The output data frame of matches will only contain households of the required size. If the number of rows in the people data frame is not divisible by household size, the overcount will be output to a separate data frame.
#'
#' @export
#' @param people A data frame containing the people to be matched into households.
#' @param pplid The variable containing the unique ID for each person.
#' @param pplage The age variable.
#' @param numppl The number of people in the households.
#' @param sdused The standard deviation of the normal distribution for the distribution of ages in a household.
#' @param HHStartNum The starting value for HHNumVar. Must be numeric.
#' @param HHNumVar The name for the household variable.
#' @param userseed If specified, this will set the seed to the number provided. If not, the normal set.seed() function will be used.
#' @param ptostop The critical p-value stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param numiters The maximum number of iterations used to construct the output data frame ($Matched) containing the household inhabitants. The default value is 1000000, and is the stopping rule if the algorithm does not converge.
#' @param verbose Whether the number of iterations used, the critical chi-squared value, and the final chi-squared value are printed to the console. The information will be printed for each set of pairs. For example, if there are three people in each household, the information will be printed twice. The default is FALSE, so no information will be printed to the console.
#'
#' @return A list of two data frames $Matched contains the data frame of households containing matched people. All households will be of the specified size. $Unmatched, if populated, contains the people that were not allocated to households. If the number of rows in the people data frame is divisible by the household size required, $Unmatched will be an empty data frame.
#'
#' @examples
#' library(dplyr)
#'
#' # creating three-person households toy example with few iterations
#' NewHouseholds <- other(AdultsNoID, pplid = "ID", pplage = "Age", numppl = 3, sdused = 3,
#'                        HHStartNum = 1, HHNumVar = "Household", userseed=4, ptostop = .05,
#'                        numiters = 500, verbose = TRUE)
#'
#' PeopleInHouseholds <- NewHouseholds$Matched
#' PeopleNot <- NewHouseholds$Unmatched      # 2213 not divisible by 3


other <- function(people, pplid, pplage, numppl = NULL, sdused, HHStartNum, HHNumVar, userseed=NULL,
                    ptostop = NULL, numiters = 1000000, verbose = FALSE) {

  withr::local_options(dplyr.summarise.inform = FALSE)


  #####################################
  # check for missing input information
  #####################################

  if (!pplid %in% names(people)) {
    stop("The ID variable does not exist.")
  }

  if (!pplage %in% names(people)) {
    stop("The age variable does not exist.")
  }

  if(is.null(HHNumVar)) {
    stop("A name for the household count variable must be supplied.")
  }

  if (is.null(numppl)) {
    stop("The household size must be supplied.")
  }

  if (is.null(HHNumVar)) {
    stop("A name for the household count variable must be supplied.")
  }

  if (!any(duplicated(people[[pplid]])) == FALSE) {
    stop("The ID variable has duplicated values.")
  }


  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  # smalldf ID variable
  pplidcolName <- sym(names(people[pplid]))
  # smalldf age variable
  pplagecolName <- sym(names(people[pplage]))

 # also need number of columns later for putting back together
  # need column count for turning wide dataframe into long
  # the +1 is there because this happens after the household ID is added
  NumberColspplPlusOne <- as.numeric(ncol(people))+1


  peopleRenamed <- people %>%
    rename(RenamedID = !! pplid, RenamedAge = !! pplage)


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

  MaxAgeDifference <-  (max(people[pplage]) -
                          min(people[pplage]))-5

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
  logProbLow <- dnorm(-MaxAgeDifference:(min_bin-0.5), mean = 0, sd = sdused, log = TRUE)
  logProbHigh <- dnorm((max_bin+0.5):MaxAgeDifference, mean = 0, sd = sdused, log = TRUE)

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


  ####################################
  # user seed
  ####################################

  if (!is.null(userseed)) {
    set.seed(userseed)
  }


  #####################################
  #####################################

  # fix dataset to be a factor of household size

  ModuloDF <- nrow(peopleRenamed) %% numppl


    # test if each Sex is divisible by household size
    if(ModuloDF > 0) {

      SampleSizeUsed <- nrow(peopleRenamed) - ModuloDF

      peopleRenamed <- peopleRenamed %>%
        slice_sample(n = SampleSizeUsed)

      # closes if(ModuloDF > 0)
    }





  #####################################
  # matching
  #####################################

  BaseSize <- nrow(peopleRenamed)/numppl

  BasePeople <- peopleRenamed %>%
    slice_sample(n = BaseSize) %>%
    mutate({{HHNumVar}} := seq(HHStartNum, (HHStartNum + BaseSize - 1)))

  RemainingPeople <- peopleRenamed %>%
    filter(!(.data$RenamedID %in% c(BasePeople$RenamedID)))

    while(!(is.na(RemainingPeople$RenamedAge[1])) == TRUE) {

      if(BaseSize < 1) {
        stop("Sample size is less than 1", "\n")
      }

      MatchingSample <- RemainingPeople %>%
        slice_sample(n = BaseSize)

      RemainingPeople <- RemainingPeople %>%
        filter(!(.data$RenamedID %in% MatchingSample$RenamedID))

      # get age differences

      CurrentAgeMatch <- BasePeople %>%
        select("RenamedAge","RenamedID")

      MatchedAgeExtract <- MatchingSample %>%
        select("RenamedAge", "RenamedID") %>%
        rename(MatchedAge = "RenamedAge",
               MatchedID = "RenamedID")

      CurrentAgeMatch <- cbind(CurrentAgeMatch, MatchedAgeExtract)

      ExpectedAgeProbs <- Probabilities * nrow(CurrentAgeMatch)
      logEAgeProbs <- logProb + log(nrow(CurrentAgeMatch))

      ObservedAgeDifferences <- hist(CurrentAgeMatch[,1] - CurrentAgeMatch[,3],
                                     breaks = bins, plot = FALSE)$counts


      # set up for chi-squared
      log0ObservedAges <- hist(CurrentAgeMatch[,1] - CurrentAgeMatch[,3],
                               breaks = logBins, plot = FALSE)$counts
      logKObservedAges = ifelse(log0ObservedAges == 0, 2*logEAgeProbs,
                                log((log0ObservedAges - exp(logEAgeProbs))^2)) - logEAgeProbs
      log_chisq = max(logKObservedAges) + log(sum(exp(logKObservedAges - max(logKObservedAges))))


      if (is.null(ptostop)) {

        Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))

      } else {

        Critical_log_chisq <- log(qchisq(ptostop, df=(length(logEAgeProbs-1)), lower.tail = TRUE))

        # closes p-value stopping rule
      }


      #####################################
      #####################################
      # iteration for matching  ages starts here
      #####################################
      #####################################


      for (i in 1:numiters) {

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
          filter(!(.data$RenamedID %in% c(PropPair1[,2], PropPair2[,2])))

        PropAgeMatch <- bind_rows(PropAgeMatch, PropPair1, PropPair2)

        # do chi-squared
        Proplog0 <- hist(PropAgeMatch[,1] - PropAgeMatch[,3], breaks = logBins, plot = FALSE)$counts
        ProplogK = ifelse(Proplog0 == 0, 2*logEAgeProbs,
                          log((Proplog0 - exp(logEAgeProbs))^2)) - logEAgeProbs

        prop_log_chisq = max(ProplogK) + log(sum(exp(ProplogK - max(ProplogK))))

        if (compare_logK(ProplogK, logKObservedAges) < 0) {

          CurrentAgeMatch[Pick1,] <- PropPair1
          CurrentAgeMatch[Pick2,] <- PropPair2


          log0ObservedAges <- Proplog0
          logKObservedAges <- ProplogK
          log_chisq <- prop_log_chisq

          # closes pair swqp

        }

        if (log_chisq <= Critical_log_chisq) {
          break


        }

        # closes iterations through the age matching
      }


      if(exists("TheMatched")) {

        InterimDataFrame <- BasePeople %>%
          left_join(CurrentAgeMatch, by=c("RenamedID", "RenamedAge")) %>%
          left_join(MatchingSample, by= c("MatchedID" = "RenamedID")) %>%
          select(all_of(NumberColspplPlusOne:last_col()))

        TheMatched <- bind_rows(InterimDataFrame, TheMatched)

      } else {

        TheMatched <- BasePeople %>%
          left_join(CurrentAgeMatch, by=c("RenamedID", "RenamedAge")) %>%
          left_join(MatchingSample, by= c("MatchedID" = "RenamedID")) %>%
          select(all_of(NumberColspplPlusOne:last_col()))

      }

      if(verbose == TRUE) {
        cat(i, "iterations were used, the critical chi-squared value was", Critical_log_chisq,", and the final chi-squared value is", round(log_chisq,6), "\n")
      }

      # closes the loop through the number of sets of people to match,
      # e.g. 1 set for two-person households, 2 sets for three-person households
    }
  #
  #   # need to ensure that the base for BOTH sexes exists, not just one
    if(exists("AppendedBase")){

      AppendedBase <- bind_rows(AppendedBase, BasePeople)

    } else {

      AppendedBase <- BasePeople

    }

  # correct the names of the variables in the interim and base data frames
  # row bind these and output

  drop_dot_y <- function(x) { gsub("\\.y$", "", x) }

  TheMatched <- TheMatched %>%
    rename_with(drop_dot_y) %>%
    select(-"RenamedAge") %>%
    rename(!!pplidcolName := "MatchedID",
           !!pplagecolName := "MatchedAge")

  TheBase <- AppendedBase %>%
    rename(!!pplidcolName := "RenamedID",
           !!pplagecolName := "RenamedAge")

  OutputDataframe <- bind_rows(TheBase, TheMatched)

  MatchedIDs <- OutputDataframe %>%
    pull({{pplidcolName}})

  UnmatchedDataframe <- people %>%
    filter(!({{pplidcolName}} %in% MatchedIDs))


  MergedList <- list()

  MergedList$Matched <- OutputDataframe
  MergedList$Unmatched <- UnmatchedDataframe

  return(MergedList)

}

