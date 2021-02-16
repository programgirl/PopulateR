#' Create a subset of observations containing only same-sex couples
#'
#' This is a wrapper for randomly sampling observations into same-sex couples.
#' It is mainly used for data frames that contain a subset of observations that require over-sampling.
#' However, it can also be used to generate a simple random sample of observations.
#' An even number of observations is output.
#'
#' @export
#' @param dataframe A data frame containing observations limited to one sex that includes an age column.
#' @param ProbSameSex The probability of any observation being assigned to a same-sex couple.
#' @param UpWeightProp The proportion of individuals who are to be over-sampled.
#' @param UpWeightLowerAge The youngest age for the over-sampling. Required if UpWeightProp value is provided.
#' @param UpWeightUpperAge The oldest age for the over-sampling. Required if UpWeightProp value is provided.
#' @param AgeVariableIndex The column number of the data frame that contains the ages.
#' @param CoupleIDValue The starting number for generating a variable that identifies the observations in a couple. Must be numeric.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied in quotes.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return A data frame of an even number of observations for allocation into same-sex couples. If CoupleIDValue is specified, household allocation will be performed.
#'
#' @examples
#' PersonDataframe <- data.frame(cbind(PersonID = c(1:1000),
#'                               PersonAge = c(round(runif(200, min=18, max=23),0), round(runif(300, min=24, max=50),0), round(runif(500, min=51, max=90),0))))
#' # create unweighted sample with no household variable
#' UnweightedExample <- SameSex(PersonDataframe,.1)
#' # create unweighted sample with household numbers
#' UnweightedExampleHouseholds <- SameSex(PersonDataframe, ProbSameSex=.1, AgeVariableIndex=2, CoupleIDValue=51, HouseholdNumVariable = "TheHouseholds")
#' # must supply the required columns when using household assignment
#' # doesn't work
#' ExampleHouseholdsWrong <- SameSex(PersonDataframe, ProbSameSex=.1, CoupleIDValue=51, HouseholdNumVariable = "TheHouseholds")
#' ExampleHouseholdsAlsoWrong <- SameSex(PersonDataframe, ProbSameSex=.1, AgeVariableIndex=2, CoupleIDValue=51, HouseholdNumVariable = TheHouseholds)
#' # No CoupleIDValue means that the household numbering subfunction is not performed
#' ExampleHouseholdsAlsoAlsoWrong <- SameSex(PersonDataframe, ProbSameSex=.1, AgeVariableIndex=2, HouseholdNumVariable = "TheHouseholds")
#' # create weighted example where 40% of people in same-sex couples are aged between 24 and 50 years
#' WeightedExample <- SameSex(PersonDataframe, .1, .4, 24, 50, 2)
#' # add household numbering
#' WeightedWithNumbering <- SameSex(PersonDataframe, .1, .4, 24, 50, 2, 101, "TheHouseholds")
#' # check weighted subfunction worked
#' WeightedWithNumbering %>%
#' filter(PersonAge >=24 & PersonAge <=50) %>%
#' summarise(CountsCreated=n()) %>%
#' mutate(PercentCounts = CountsCreated/nrow(WeightedWithNumbering))
#' Example of downweights, can be used with upweight range is not contiguous
#' DownWeightedWithNumbering <- SameSex(PersonDataframe, .1, .2, 24, 50, 2, 101, "TheHouseholds")
#' DownWeightedWithNumbering %>%
#' filter(PersonAge >=24 & PersonAge <=50) %>%
#' summarise(CountsCreated=n()) %>%
#' mutate(PercentCounts = CountsCreated/nrow(DownWeightedWithNumbering))
#'
#'
SameSex <- function(dataframe, ProbSameSex = NULL, UpWeightProp = NULL, UpWeightLowerAge = NULL, UpWeightUpperAge = NULL,
                    AgeVariableIndex = NULL, CoupleIDValue = NULL, HouseholdNumVariable = NULL, UserSeed = NULL) {

  # ProbExpected only used if UpWeight is not NULL, is the probability associated with the upweighted age range
  # UpWeightLowerAge/UpweightUpperAge only used if UpWeight is not NULL

  # get total number of partnered men
  CountPartneredCouples <- as.numeric(nrow(dataframe))
  NumberRequired <- as.numeric(plyr::round_any((ProbSameSex*CountPartneredCouples), 2))
  #InternalHouseholdNumVariable <- get()

  # ensure a probability for same sex couples is included
  if(is.null(ProbSameSex)) {
    stop("The probability of being in a same sex couple must be supplied.")
  }


  # create simple random sample without weights

   if (is.null(UpWeightProp)) {
    SameSexCouples <- dataframe[sample(1:CountPartneredCouples, NumberRequired, replace=FALSE),]

  }

  # create weighted samples
  # now need to fix this code in accordance with amendment above.
  if (is.numeric(UpWeightProp)) {

    if(is.null(UpWeightLowerAge) == TRUE) {
      stop("A minimum age for the upweights is required.")
    }
    if(is.null(UpWeightUpperAge) == TRUE) {
      stop("A maximum age for the upweights is required.")
    }
    if(is.null(AgeVariableIndex) == TRUE) {
      stop("The column index for the age variable is required.")
    }


  # get proportion of ages for upweighted observations

  PropToUpWeight <- dataframe %>%
    filter(dataframe[,AgeVariableIndex] >= UpWeightLowerAge & dataframe[,AgeVariableIndex] <= UpWeightUpperAge) %>%
    summarise(Value=n()) %>%
    mutate(PropResult = Value/nrow(dataframe)) %>%
    pull(PropResult)


  UpWeightObs <- dataframe %>%
    filter(dataframe[,AgeVariableIndex] >= UpWeightLowerAge & dataframe[,AgeVariableIndex] <= UpWeightUpperAge)

  # seed must come before  sample is cut
  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  # check against actual proportion
  # only adjust proportion if this differs to expected

  if (PropToUpWeight != UpWeightProp) {

    # upweight fix
    # create upweighted subset
    # create downweighted subset
    # merge to form output dataset

    UpWeightCount <- plyr::round_any(as.numeric((ProbSameSex*(UpWeightProp/PropToUpWeight)*nrow(UpWeightObs))), 2)

    UpWeightObsSample <- UpWeightObs[sample(1:as.numeric(nrow(UpWeightObs)), UpWeightCount, replace=FALSE),]

    DownWeightObs <- dataframe %>%
      filter(dataframe[,AgeVariableIndex] < UpWeightLowerAge | dataframe[,AgeVariableIndex] > UpWeightUpperAge)

    DownWeightObsSample <- DownWeightObs[sample(1:as.numeric(nrow(DownWeightObs)), (NumberRequired - UpWeightCount), replace=FALSE),]

    SameSexCouples <- rbind(UpWeightObsSample, DownWeightObsSample)


  } else {

    # the expected and actual proportions are the same so just output a random sample

    SameSexCouples <- dataframe[sample(1:CountPartneredCouples, NumberRequired, replace=FALSE),]

  }

  }

  # create households if a start household number is provided
  if (is.numeric(CoupleIDValue)) {

    if(is.null(AgeVariableIndex)) {
      stop("The column number for the age variable must be supplied.")
    }

    if(is.null(HouseholdNumVariable)) {
      stop("A name for the household count variable must be supplied.")
    }

    MaxCoupleIDValue <- (nrow(SameSexCouples)/2)-1
    SameSexCouples <- SameSexCouples %>%
      arrange(SameSexCouples[,AgeVariableIndex]) %>%
      mutate({{HouseholdNumVariable}} := rep((CoupleIDValue):(CoupleIDValue+MaxCoupleIDValue),
                                             each=2))

    }




 return(SameSexCouples)
}
