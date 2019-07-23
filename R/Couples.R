library("dplyr")
#  bring in data - this uses the test file
# HH3P <- readRDS("~/Sync/PhD/PopSim/R/HH3P.Rds")
# split out the males for testing

# HH3PPartneredMales <- HH3P %>%
#   filter(SEX=="Male", RELATIONSHIP=="Partnered")


# same sex couples
Create.SameSexCouples <- function(dataframe, ProbSameSex = NULL, UpWeight = FALSE, ProbExpected = NULL, UpweightLowerAge = NULL, UpweightUpperAge = NULL) {

  # TODO need age column number, test is age is integer

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
  if (is.null(UpWeight)) {
    SameSexCouples <- dataframe[sample(1:CountPartneredCouples, NumberRequired, replace=FALSE),]
    return(SameSexCouples)
  }

  # create weighted samples
  if (UpWeight == TRUE) {
    if (is.null(ProbExpected) == TRUE) {
      stop("An expected probability for upweighted ages must be provided.")
    }
    if(is.null(UpweightLowerAge) == TRUE) {
      stop("A minimum age for the upweights is required")
    }
    if(is.null(UpweightUpperAge) == TRUE) {
      stop("A maximum age for the upweights is required")
    }

  }

  #split file into high
  #
  # # higher probability is for age bands 6 through 8
  # # get proportion of men in these age bands
  # CountHH2PMaleSameSexBands <- Partnered2PHH %>%
  #   filter(SEX=="Male" & AssignedAge >=25 &AssignedAge <=54) %>%
  #   summarise(Value=n())
  #
  # PropHH2PMaleSameSexBands <- CountHH2PMaleSameSexBands/CountHH2PartneredMen
  # # only 24% of partnered men in the higher same sex proportion age bands
  # # but expectation is 67% so need to weight the age groups
  # # need to upweight the higher-contributing ages
  # # and make this an even number
  # SameSexMaleHigher <- plyr::round_any(as.numeric((ProbSameSexManP2HH*(SameSexPeakAgeProp/PropHH2PMaleSameSexBands)*CountHH2PMaleSameSexBands)), 2)
  #
  # # select the count created from the loop above, from the high probability age bands
  # Partnered2PHHSameSexHPDataframe <- Partnered2PHH %>%
  #   filter(SEX=="Male", AssignedAge >=25, AssignedAge<=54)
  # set.seed(161018)
  # Partnered2PHHSameSexHP <- sample_n(Partnered2PHHSameSexHPDataframe,SameSexMaleHigher)
  #
  #
  # # repeat for the lower-contributing ages
  # SameSexMaleLower <- plyr::round_any(as.numeric((ProbSameSexManP2HH*((1-SameSexPeakAgeProp)/PropHH2PMaleSameSexBands)*CountHH2PMaleSameSexBands)), 2)
  #
  # Partnered2PHHSameSexLPDataframe <- Partnered2PHH %>%
  #   filter(SEX=="Male", AssignedAge < 25 | AssignedAge > 54)
  # set.seed(161018)
  # Partnered2PHHSameSexLP <- sample_n(Partnered2PHHSameSexLPDataframe,SameSexMaleLower)
  #
  # # merge the two data frames into actual same sex data frame
  # # both data frames extracted are even so no need to adjust at this point
  # Partnered2PHHSameSexMen <- rbind(Partnered2PHHSameSexHP,Partnered2PHHSameSexLP)
  # table(Partnered2PHHSameSexMen$AssignedAge)

  # return(SameSexCouples)
}

TestOpposite <- Create.SameSexCouples(HH3PPartneredMales, .1)

# values for testing below
# dataframe <- HH3PPartneredMales
# ProbSameSex <- .1
# UpWeight <- NULL
# TestOpposite <- HH3PPartneredMales[sample(1:CountPartneredCouples, 10, replace=FALSE),]
# NumberRequired <- plyr::round_any(as.numeric(.1*CountPartneredCouples), 2)
# SameSexCouples <- HH3PPartneredMales[sample(1:HH3PPartneredMales, NumberRequired, replace=FALSE),]


# # opposite sex couples
# Create.OppositeSexCouples <- function(dataframe, ) {
#
#   # TODO dataframe, lower age for upscaling, upper age for upscaling (test if either or both present)
#   # TODO age column, expected prop same sex, expected prop upscaled age
#
#   # need to return a data frame/vector
# }


# saveRDS(HH3PPartneredMales, "HH3PPartneredMales.Rds")
# saveRDS(HH3P, "HH3P.Rds")
