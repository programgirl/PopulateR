#' Create a subset of observations containing only same-sex couples
#'
#' This is a wrapper for randomly sampling observations into same-sex couples.
#' It is mainly used for data frames that contain a subset of observations that require over-sampling.
#' However, it can also be used to generate a simple random sample.
#'
#' @export
#' @param x A data frame containing observations limited to one sex and includes an age column.
#' @param ProbSameSex The probability of any observation being assigned to a same-sex couple.
#' @param UpWeight If TRUE, a subset of ages will be over-sampled.
#' @param UpWeightLowerAge The youngest age for the over-sampling. Required if UpWeight is TRUE.
#' @param UpWeightUpperAge The oldest age for the over-sampling. Required if UpWeight is TRUE.
#' @param AgeVariableIndex The column number of the data frame that contains the ages. Only used if over-sampling is specified. Required if UpWeight is TRUE.
#'
#' @return A data frame of an even number of observations for allocation into same-sex couples.
Create.SameSexCouples <- function(x, ProbSameSex = NULL, UpWeight = FALSE, UpWeightProp = NULL, UpWeightLowerAge = NULL, UpWeightUpperAge = NULL, AgeVariableIndex = NULL) {

  # ProbExpected only used if UpWeight is not NULL, is the probability associated with the upweighted age range
  # UpWeightLowerAge/UpweightUpperAge only used if UpWeight is not NULL

  # get total number of partnered men
  CountPartneredCouples <- as.numeric(nrow(dataframe))
  NumberRequired <- as.numeric(plyr::round_any((ProbSameSex*CountPartneredCouples), 2))

  # ensure a probability for same sex couples is included
  if(is.null(ProbSameSex)) {
    stop("The probability of being in a same sex couple must be supplied.")
  }

  # create simple random sample without weights
  if (isFALSE(UpWeight)) {
    SameSexCouples <- dataframe[sample(1:CountPartneredCouples, NumberRequired, replace=FALSE),]
    }

  # create weighted samples
  if (isTRUE(UpWeight)) {
    if (is.null(UpWeightProp) == TRUE) {
      stop("An expected proportion of upweighted ages must be provided.")
    }
    if(is.null(UpWeightLowerAge) == TRUE) {
      stop("A minimum age for the upweights is required.")
    }
    if(is.null(UpWeightUpperAge) == TRUE) {
      stop("A maximum age for the upweights is required.")
    }
    if(is.null(AgeVariableIndex) == TRUE) {
      stop("The column index for the age variable is required.")
    }

  # }

  # get proportion of ages for upweighted observations

  PropToUpWeight <- dataframe %>%
    filter(dataframe[,8] >= UpWeightLowerAge & dataframe[,8] <= UpWeightUpperAge) %>%
    summarise(Value=n()) %>%
    mutate(PropResult = Value/nrow(dataframe)) %>%
    select(PropResult) %>%
    as.numeric()

  UpWeightObs <- dataframe %>%
    filter(dataframe[,8] >= UpWeightLowerAge & dataframe[,8] <= UpWeightUpperAge)

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
      filter(dataframe[,8] < UpWeightLowerAge | dataframe[,8] > UpWeightUpperAge)

    DownWeightObsSample <- UpWeightObs[sample(1:as.numeric(nrow(DownWeightObs)), (NumberRequired - UpWeightCount), replace=FALSE),]

    SameSexCouples <- rbind(UpWeightObsSample, DownWeightObsSample)

  } else {

    # the expected and actual proportions are the same so just output a random sample

    SameSexCouples <- dataframe[sample(1:CountPartneredCouples, NumberRequired, replace=FALSE),]

  }

  }
 return(SameSexCouples)
}
# TODO examples.

#library("dplyr")
#  bring in data - this uses the test file
# HH3P <- readRDS("~/Sync/PhD/PopSim/R/HH3P.Rds")

# split out the males for testing
# HH3PPartneredMales <- HH3P %>%
#   filter(SEX=="Male", RELATIONSHIP=="Partnered")


# same sex couples

# TestNoWeights <- Create.SameSexCouples(HH3PPartneredMales, .1)

# values for testing below
# dataframe <- HH3PPartneredMales
# ProbSameSex <- .1
# UpWeight <- NULL
# TestOpposite <- HH3PPartneredMales[sample(1:CountPartneredCouples, 10, replace=FALSE),]
# NumberRequired <- plyr::round_any(as.numeric(.1*CountPartneredCouples), 2)
# SameSexCouples <- HH3PPartneredMales[sample(1:HH3PPartneredMales, NumberRequired, replace=FALSE),]

# testing for UpWeight == TRUE


# AgeVariableIndex <- 8
# #AgeVariable <- dataframe[,AgeVariableIndex]
# UpWeight <- TRUE
# ProbSameSex <- .1
# UpWeightProp <- .6
# UpWeightLowerAge <- 25
# UpWeightUpperAge <- 54

# put the above into an input format
# TestWeights <- Create.SameSexCouples(HH3PPartneredMales , .1, TRUE, .6, 25, 54, 8)


