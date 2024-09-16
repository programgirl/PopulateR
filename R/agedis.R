#' Add a sex/age structure to a data frame of grouped ages.
#' This function creates a data frame that adds an age structure to a data frame that contains age groups.
#' Two data frames are required: the data frame that contains individuals with age bands ("individuals"), and a data frame used as the basis for constructing a sex/age pyramid ("pyramid"). 
#' The individuals data frame requires two columns relating to the age groups. One is the minimum age in the age group The second is the maximum age in the age group For example, the age group 0 - 4 years would have 0 as the minimum age value and 4 as the maximum age value. Each person in the individuals data frame must have both the minimum and maximum age variables populated.
#' The pyramid data frame must contain counts by sex/age in the population of interest.
#' The variables specifying sex can be numeric, character, or factor. The sole requirement is that the codes must match.  For example, if "F" and "M" are used in the individuals data frame to denote sex, then "F" and "M" are the codes required in the pyramid data frame. Any number of sex code values can be used, so long as they are unique.
#' @export
#' @param individuals A data frame containing observations with grouped ages. These are the observations to which the sex/age pyramid is applied.
#' @param indsx The variable containing the codes for sex, in the individuals data frame.
#' @param minage The variable containing the minimum age for the age group, in the individuals data frame.
#' @param maxage The variable containing the maximum age for the age group, in the individuals data frame.
#' @param pyramid A data frame containing the sex/age pyramid to be used.
#' @param pyrsx The variable containing the codes for sex, in the pyramid data frame. 
#' @param pyrage The variable containing the ages, in the pyramid data frame.
#' @param pyrcount The variable containing the counts for each sex/age combination, in the pyramid data frame.
#' @param agevarname The name to use for the constructed age variable in the output data frame. For each row, this will contain one integer.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.

#' @return A data frame of an observations, with an added column that contains the age.

#' @examples
#' DisaggregateAge <- agedis(InitialDataframe, Sex, LowerAge, UpperAge, SingleAges, Sex, Age, Value,
#'                           agevarname = "TheAge", userseed = 4)

agedis <- function(individuals, indsx, minage, maxage, pyramid, pyrsx, pyrage, pyrcount, agevarname,
                   userseed = NULL)

{

  options(dplyr.summarise.inform=F)

  #####################################
  # check for missing input information
  #####################################
  
  if (!indsx %in% names(individuals)) {
    stop("Sex variable name in the individuals data frame is incorrect.")
  }
  
  if (!minage %in% names(individuals)) {
    stop("Minimum age variable name in the individuals data frame is incorrect.")
  }
  
  if (!maxage %in% names(individuals)) {
    stop("Maximum age variable name in the individuals data frame is incorrect.")
  }
  
  if (!pyrsx %in% names(pyramid)) {
    stop("Sex variable name in the pyramid data frame is incorrect.")
  }
  
  if (!pyrage %in% names(pyramid)) {
    stop("Age variable name in the pyramid data frame is incorrect.")
  }
  
  if (!pyrcount %in% names(pyramid)) {
    stop("Variable name for the age counts in the pyramid data frame is incorrect.")
  }

 
  # #####################################
  # #####################################
  # # rename variables so don't need to use quosures inside code
  # #####################################
  # #####################################

  BaseDataFrame <- as.data.frame(individuals %>%
    rename(Sex = !! indsx, MinAge = !! minage, MaxAge = !! maxage) %>%
    mutate(Sex = as.character(Sex)))

  Agepyramid <- as.data.frame(pyramid %>%
    rename(Sex = !! pyrsx, Age = !! pyrage, Prob = !! pyrcount) %>%
    mutate(Sex = as.character(Sex)))
  
  #####################################
  #####################################
  # end column names
  #####################################
  #####################################

  #####################################
  # check alignment of the two sex variables codes
  #####################################

  BaseSexCodes <- BaseDataFrame %>%
    select(Sex) %>%
    distinct(Sex) %>%
    arrange(Sex)

  pyramidSexCodes <- Agepyramid %>%
    select(Sex) %>%
    distinct(Sex) %>%
    arrange(Sex)


  if (isFALSE(identical(BaseSexCodes, pyramidSexCodes))) {

    stop("The sex variable values are not the same for both data frames.")

  }
  

  #####################################
  #####################################
  # perform the age allocation
  #####################################
  #####################################

  # seed must come before first sample is cut
  if (!is.null(userseed)) {
    set.seed(userseed)
  }

  # !!!!!!!!!!!!!!!!! note testing restriction
 for (x in 1:nrow(BaseDataFrame)) {
  # for (x in 1:10) {

    CurrentIndividual <- BaseDataFrame[x,]
    
 #   print("Selected current individual")

    AgeRestricted <- Agepyramid %>%
      filter(between(Age, CurrentIndividual$MinAge, CurrentIndividual$MaxAge),
             Sex == CurrentIndividual$Sex) %>%
      select(Age, Prob)
    
 #  print("Constructed age pyramid of correct ages")

    OutputAge <- AgeRestricted %>%
      slice_sample(n=1, weight_by = Prob) %>%
      select(Age) %>%
      pull(Age)
    
#    print("Probabilistic age selected")

    CurrentIndividual <- CurrentIndividual %>%
      mutate(!!quo_name(agevarname) := OutputAge)

 #   print("Age column has been added")

    if (exists("DataFrameWithAges") == TRUE) {

      DataFrameWithAges <- bind_rows(DataFrameWithAges, CurrentIndividual)

    } else {

      DataFrameWithAges <- CurrentIndividual

      # close evaluation for creating the output data frame
    }

    # closes age assignment loop

  }

  #
  return(DataFrameWithAges)

  #closes function
}
