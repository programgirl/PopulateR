#' Create a subset of observations containing only opposite-sex couples
#'
#' This function creates a data frame of couples, based on a population distribution of age differences. The distribution used is the skew normal.
#' Two data frames are required. The recipient data frame contains the age data, of one sex, to which the distribution will be applied. The donor data frame contains the age data, of the other sex, from which the age counts to match are constructed. If the two data frames are different lengths, the recipient data frame must be the shortest of the two. In this situation, a random subsample of the donor data frame will be used.
#' Both data frames must be restricted to only those ages that will have a couples match performed. No age reasonableness check is made. An even number of observations is output, using the defined age-difference distribution between the female and male ages for the couples.
#' The function performs a reasonableness check for the first five variables. If any other parameters are missing, the usual error messages from the imported functions will be output.
#'
#' If desired, this can be used to construct same-sex couples.
#'
#' @export
#' @param Recipient A data frame containing observations limited to one sex. An age column is required. Only include the ages that are eligible for partner allocation.
#' @param RecipientIDCol The column number for the ID variable in the Recipient data frame.
#' @param RecipientAgeCol The column number for the age variable in the Recipient data frame.
#' @param Donor A data frame containing observations limited to one sex. An age column is required. Only include the ages that will be allocated to partners.
#' @param DonorIDCol The column number for the donor ID. Must be numeric.
#' @param DonorAgeCol The column number for the age variable in the Donor data frame.
#' @param DirectXi The location parameter of the difference in couple ages. The value must be positive, so that the Recipients must tend to be older than the Donors.
#' @param DirectOmega The scale parameter of the difference in ages. The value must be positive.
#' @param AlphaUsed The alpha value for the skew normal distribution.
#' @param IDStartValue The starting number for generating a variable that identifies the observations in a couple. Must be numeric.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied in quotes.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param pValueToStop The primary stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param NumIterations The maximum number of iterations used to construct the coupled data frame. This has a default value of 1000000, and is the stopping rule if the algorithm does not converge.
#'
#' @return A data frame of an even number of observations that have been allocated into opposite-sex couples.
#'
#' @examples
#' Recipients <- data.frame(cbind(PersonID = c(1:1000),
#'                               PersonAge = c(round(runif(200, min=18, max=23),0), round(runif(300, min=24, max=50),0), round(runif(500, min=51, max=90),0))))
#'
#' Donors <- data.frame(cbind(PersonID = c(2001:4000),
#'                               PersonAge = c(round(runif(400, min=18, max=23),0), round(runif(500, min=24, max=50),0), round(runif(1100, min=51, max=90),0))))
#'
#' ExampleOutput <- OppositeSex(Recipients, RecipientIDCol=1, RecipientAgeCol=2, Donors, DonorIDCol=1, DonorAgeCol=2, DirectXi=-2, DirectOmega=4,
#'                               AlphaUsed=5, UserSeed=NULL, pValueToStop=.001, NumIterations=1000, IDStartValue = 10001, HouseholdNumVariable="TheHouseholds")


Networks <- function(People, IDCol=NULL, AgeCol=NULL, NetworkCol=NULL, MeanUsed=NULL, SDUsed=NULL, ProbSameNetwork = .5,
                     NetworkVariable = NULL, UserSeed=NULL, pValueToStop=NULL, NumIterations=1000000) {


  if(is.null(IDCol)) {
    stop("The ID column number must be supplied.")
  }

  if (!is.numeric(AgeCol)) {
    stop("The age column number must be supplied.")
  }

  if (!is.numeric(NetworkCol)) {
    stop("The network size column number must be supplied.")
  }

  if(is.null(NetworkVariable)) {
    stop("A name for the network variable must be supplied.")
  }

  #################################################
  # end data check
  ################################################

  # construct start value for networkID

  #NetworkIDCounter <- 1

  # subset by number of people to match
  # move from smallest contact network to largest
  # otherwise may run out of reasonable age pairs for the small numbers

  # find smallest and largest contact sizes

  WorkingDataFrame <- People %>%
    rename(ID = !! IDCol, Age = !! AgeCol, Network = !! NetworkCol) %>%
    filter(Network > 0)

  if(!(sum(WorkingDataFrame$Network) %% 2 == 0) == TRUE) {
    stop("The network sizes must sum to a factor of 2.")
  }

  NetworkSizeCounts <- WorkingDataFrame %>%
    group_by(Network) %>%
    summarise(CountofSize = n())

  MinimumSize <- min(NetworkSizeCounts$Network)
  MaximumSize <- max(NetworkSizeCounts$Network)

  cat("Minimum network size is", MinimumSize, "and maximum network size is", MaximumSize, "\n")

  # construct empty data frame to hold the matches
  # one network per row
  # initialise so that the number of columns is the maximum size of people in a network +1
  ColCountNeeded <- MaximumSize + 1

  OutputDataFrame <- setNames(data.frame(matrix(ncol = ColCountNeeded, nrow = 0)),
                              paste0("Person", c(1:ColCountNeeded)))


  # work through the data frame, as people are extracted they will be given contacts
  # and once the contacts are added, they are removed from the data frame
  # will randomly draw

  for(i in 1:1) {
 # while(!(is.na(WorkingDataFrame$ID[1])) == TRUE) {

    SelectedPerson <- WorkingDataFrame %>%
      slice_sample(n=1)

    NetworkSizeForSelected <- SelectedPerson$Network

    cat("Network size is", NetworkSizeForSelected, "\n")

    # remove this person from the working data frame
    # this also prevents them from being selected to match against themselves
    # that would be bad

    WorkingDataFrame <- WorkingDataFrame %>%
      filter(!ID==SelectedPerson$ID)

  #  cat("The working data frame is now", nrow(WorkingDataFrame), "rows long.", "\n")

    # for each person in their network size, redo the age difference so that we don't get
    # everyone aged the same as their network contacts


    for(i in 1:NetworkSizeForSelected) {

      AgeDiffNeeded <- rnorm(1, MeanUsed, SDUsed)
      AgeNeeded <- round(SelectedPerson$Age + AgeDiffNeeded)

      cat("Age needed is", AgeNeeded, "\n")

      OperativeDataFrame <- WorkingDataFrame %>%
        filter(Age==AgeNeeded)

      cat("It got to here", "\n")

      # loop for extracting people if OperativeDataFrame is empty
      # which will occur if there are no people of the required age
      # principle here is to widen the age range that is capable of being selected

      if(!(is.na(OperativeDataFrame$Age[1])) == TRUE) {

        RandomlySelectedMatch <- OperativeDataFrame %>%
          slice_sample(n = 1)

      } else {

      n = 0

      while(!(is.na(OperativeDataFrame$Age[1])) == TRUE) {

        n = n + 1

        AgeRangeMin <- AgeNeeded - n
        AgeRangeMax <- AgeNeeded + n
        # stop the age ranges going beyond the data values
        if(AgeRangeMin > min(WorkingDataFrame$Age)) {
          AgeRangeMin <- min(WorkingDataFrame$Age)
        }
        if(AgeRangeMax < max(WorkingDataFrame$Age)) {
        AgeRangeMax <- max(WorkingDataFrame$Age)
        }

        cat("No age match made, new age range is", AgeRangeMin, "to", AgeRangeMax, "\n")

        # filter also removes people already selected as a match
        OperativeDataFrame <- WorkingDataFrame %>%
          filter(between(Age, AgeRangeMin,AgeRangeMax), !(ID %in% OperativeDataFrame$ID))

      }

      RandomlySelectedMatch <- OperativeDataFrame %>%
        slice_sample(n = 1)
        # closes while loop for getting a match
      }

      # put selected person into a data frame that will contain all matches

      if(exists("DataframeOfMatches")) {

        DataframeOfMatches <- bind_rows(DataframeOfMatches, RandomlySelectedMatch)
        WorkingDataFrame <- WorkingDataFrame %>%
          filter(!ID == RandomlySelectedMatch)

      } else {

        DataframeOfMatches <- RandomlySelectedMatch

        WorkingDataFrame <- WorkingDataFrame %>%
          filter(!ID == RandomlySelectedMatch)

        # constructs the data frame of matches for the current person
      }


      # TODO: decrement the contact number count from the selected people.
      # if contact == 0, remove from the working data frame

      # TODO: Get the people added so that we end up with one column per person, all on the one row. The randomly selected person with whom to match must be the first person in the row.


      # need to add people to the original person

      # TODO: need to add the people into the columns, can do this per person.

      # closes for loop for selecting all the people into their network size
    }

    # TODO: check if there is a probability > 0 that a person can be a friend of a friend
    # do this for each person, but not for checks already made
    # e.g. if network has A matched to B, C, D, and E, then need to:
    # 1. get remaining contacts for B (e.g. F, G)
    # 2. decrement the contact size for F and G by 1 for each. If either now has network size == 0, remove
    # 3. remove B from list of people who can be matched.
    # 4. test if either of F or G are friends of C, D, or E (assuming F and G have > 1 friend)
    # 5. if so, add these to the relevant contact list for C, D, or E
    # 6. if so, decrement the network size of C, D, and E
    # 7. and in all of these, C, D, E, F, and G need to be held out as selected people
    # this will get complicated as need to loop through each match
    # non-matches can be ignored


    #closes while loop for selecting people from the working data frame
  }

  #
  #       if(exists("MatchedPair")==TRUE) {
  #
  #         MatchedPairDF <- Mat
  #       }
  #
  #       MatchedPair <- bind_cols(RandomlySelectedMatch, )
  #
  #
  #       # closes loop through matching people to this person

  #
  return(DataframeOfMatches)

    # closes function
}
