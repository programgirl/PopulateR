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


Networks <- function(People, IDCol=NULL, AgeCol=NULL, MeanUsed=NULL, SDUsed=NULL, ProbSameNetwork = .5,
                     NetworkVariable = NULL, UserSeed=NULL, pValueToStop=NULL, NumIterations=1000000) {


  if(is.null(IDCol)) {
    stop("The ID column number must be supplied.")
  }
  if (!is.numeric(AgeCol)) {
    stop("The age column number must be supplied.")
  }

  if(is.null(NetworkVariable)) {
    stop("A name for the network variable must be supplied.")
  }

  #################################################
  # end data check
  ################################################

  # construct start value for networkID

  NetworkIDCounter <- 1

  # subset by number of people to match
  # move from smallest contact network to largest
  # otherwise may run out of reasonable age pairs for the small numbers

  # find smallest and largest contact sizes

  WorkingDataFrame <- People %>%
    rename(ID = !! IDCol, Age = !! AgeCol)

  ContactsCountsByID <- WorkingDataFrame %>%
    group_by(ID, Age) %>%
    summarise(Size = n())

  NetworkSizeCounts <- ContactsCountsByID %>%
    group_by(Size) %>%
    summarise(CountofSize = n())

  MinimumSize <- min(NetworkSizeCounts$Size)
  MaximumSize <- max(NetworkSizeCounts$Size)

  cat("Minimum network size is", MinimumSize, "and maximum network size is", MaximumSize, "\n")

  while(!(is.na(NetworkSizeCounts$Size[1])) == TRUE) {

    CurrentNetworkSize <- NetworkSizeCounts$Size[1]

    PeopleWithThisNetworkSize <- ContactsCountsByID %>%
      filter(Size == CurrentNetworkSize)

    for(i in 1:nrow(PeopleWithThisNetworkSize)) {

      WorkingID <- PeopleWithThisNetworkSize$ID[i]

      for(j in 1:CurrentNetworkSize) {

        AgeDiffNeeded <- rnorm(1, MeanUsed, SDUsed)
        AgeNeeded <- round(PeopleWithThisNetworkSize$ID[i] + AgeDiffNeeded)

        OperativeDataFrame <- WorkingDataFrame %>%
          filter(Age==AgeNeeded, !ID==WorkingID)

        while(!(is.na(OperativeDataFrame$Age[1])) == TRUE) {
          AgeRangeMin <- AgeNeeded - 1
          AgeRangeMax <- AgeNeeded + 1

          OperativeDataFrame <- WorkingDataFrame %>%
            filter(between(Age, AgeRangeMin,AgeRangeMax, !ID==WorkingID))

          # finding match for the person, widens the age options for each iteration.
          # will exhausting find a match
        }

        RandomlySelectedMatch <- OperativeDataFrame %>%
          slice_sample(n = 1)

        MatchedPair <- bind_cols()


        # closes loop through matching people to this person
    }

      # closes loop people with this network size
    }

    # closes loop through network sizes
  }

  #
  return(ContactsCountsByID)

    # closes function
}
