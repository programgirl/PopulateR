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
#'
#' @return A data frame of an even number of observations for allocation into same-sex couples. If CoupleIDValue is specified, household allocation will be performed.
#'
#' @examples
#' PersonDataframe <- data.frame(cbind(PersonID = c(1:1000),
#'                               PersonAge = c(round(runif(200, min=18, max=23),0), round(runif(300, min=24, max=50),0), round(runif(500, min=51, max=90),0))))
#' # create unweighted sample with no household variable
#' UnweightedExample <- same_sex(PersonDataframe,.1)
#' # create unweighted sample with household numbers
#' UnweightedExampleHouseholds <- same_sex(PersonDataframe, ProbSameSex=.1, AgeVariableIndex=2, CoupleIDValue=51, HouseholdNumVariable = "TheHouseholds")
#' # must supply the required columns when using household assignment
#' # doesn't work
#' ExampleHouseholdsWrong <- same_sex(PersonDataframe, ProbSameSex=.1, CoupleIDValue=51, HouseholdNumVariable = "TheHouseholds")
#' ExampleHouseholdsAlsoWrong <- same_sex(PersonDataframe, ProbSameSex=.1, AgeVariableIndex=2, CoupleIDValue=51, HouseholdNumVariable = TheHouseholds)
#' # No CoupleIDValue means that the household numbering subfunction is not performed
#' ExampleHouseholdsAlsoAlsoWrong <- same_sex(PersonDataframe, ProbSameSex=.1, AgeVariableIndex=2, HouseholdNumVariable = "TheHouseholds")
#' # create weighted example where 40% of people in same-sex couples are aged between 24 and 50 years
#' WeightedExample <- same_sex(PersonDataframe, .1, .4, 24, 50, 2)
#' # add household numbering
#' WeightedWithNumbering <- same_sex(PersonDataframe, .1, .4, 24, 50, 2, 101, "TheHouseholds")
#' # check weighted subfunction worked
#' WeightedWithNumbering %>%
#' filter(PersonAge >=24 & PersonAge <=50) %>%
#' summarise(CountsCreated=n()) %>%
#' mutate(PercentCounts = CountsCreated/nrow(WeightedWithNumbering))
#' Example of downweights, can be used with upweight range is not contiguous
#' DownWeightedWithNumbering <- same_sex(PersonDataframe, .1, .2, 24, 50, 2, 101, "TheHouseholds")
#' DownWeightedWithNumbering %>%
#' filter(PersonAge >=24 & PersonAge <=50) %>%
#' summarise(CountsCreated=n()) %>%
#' mutate(PercentCounts = CountsCreated/nrow(DownWeightedWithNumbering))
#'
#'
same_sex <- function(dataframe, ProbSameSex = NULL, UpWeightProp = NULL, UpWeightLowerAge = NULL, UpWeightUpperAge = NULL, AgeVariableIndex = NULL,
                                  CoupleIDValue = NULL, HouseholdNumVariable = NULL) {

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
    select(PropResult) %>%
    as.numeric()

  UpWeightObs <- dataframe %>%
    filter(dataframe[,AgeVariableIndex] >= UpWeightLowerAge & dataframe[,AgeVariableIndex] <= UpWeightUpperAge)

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


library("dplyr")


#  bring in data - this uses the test file
# HH3P <- readRDS("~/Sync/PhD/PopSim/R/HH3P.Rds")

# import data on the number of children that same sex couples have
# TABLECODE8163 <- read.csv(
#   "~/Sync/PhD/egg project/Timaru census data cleaning/Same sex couples information/Number of children/TABLECODE8163_Data_ecdbd669-529d-4052-96e1-ebf7934e8b0e.csv")
#  countSameSexMales <- nrow(PartneredMales)*("~/Sync/PhD/egg project/Timaru census data cleaning/TABLECODE8160_Data_aa91bc90.8521.4921.9d1f.67334c9ded14[3,3]")
# import same sex couple household information
# TABLECODE8160 <- read.csv(
  # "~/Sync/PhD/egg project/Timaru census data cleaning/Same sex couples information/Family type with type of couple/TABLECODE8160_Data_aa91bc90-8521-4921-9d1f-67334c9ded14.csv")
# import same sex couple age information
# TABLECODE8161 <- read.csv(
  # "~/Sync/PhD/egg project/Timaru census data cleaning/Same sex couples information/Age group of people in same-sex couples in occupied private dwellings/TABLECODE8161_Data_88aa206a-26ef-4733-87a5-434df88589b7.csv")
# calculate peak same sex age proportions
# SameSexPeakAgeCounts <- TABLECODE8161 %>%
#   filter(Age.group.of.people.in.same.sex.couples %in% c("25-34 years", "35-44 years", "45-54 years")) %>%
#   summarise(Value=sum(Value))
# get proportion of peak same sex ages as a function of totals
# SameSexPeakAgeProp <- SameSexPeakAgeCounts/TABLECODE8161[1,3]

# split out the males, and do the prep
# PartneredMales <- HH3P %>%
#   filter(SEX=="Male", RELATIONSHIP=="Partnered")
# ProbSameSexMales <- (TABLECODE8160[3,3]/TABLECODE8160[5,3])*2

# run function
# SameSexMales <- same_sex(PartneredMales, ProbSameSexMales, as.numeric(SameSexPeakAgeProp), 25, 54, 8, 1, "Households")

# split out females
# PartneredFemales <- HH3P %>%
#   filter(SEX=="Female", RELATIONSHIP=="Partnered")
# SameSexFemales <- same_sex(PartneredFemales , .1, TRUE, .6, 25, 54, 8, 50, MyHouseholds)
# ProbSameSexFemales <- (TABLECODE8160[4,3]/TABLECODE8160[5,3])*2
# SameSexFemales <- same_sex(PartneredFemales, ProbSameSexFemales, as.numeric(SameSexPeakAgeProp), 25, 54, 8, (nrow(SameSexMales)/2)+1, "Households")

# Add both dataframes into one same sex coupled data frame
# SameSex <- rbind(SameSexMales, SameSexFemales)

# Subset original data frame so that the same sex couples no longer remain
# SubsetNoSameSex <- HH3P %>%
# filter(!ID %in% SameSex$ID)
# saveRDS(SubsetNoSameSex, "SubsetNoSameSex.rds")
