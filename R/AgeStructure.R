#' Add a sex/age structure to a data frame of grouped ages.
#' This function creates a data frame that adds an age structure to a data frame that contains age bands. The output is the same data as Individuals, but with an added column that contains the age.
#' Two data frames are required: the data frame that contains individuals with age bands ("Individuals"), and a data frame used as the basis for constructing a sex/age pyramid ("Pyramid"). The Pyramid data fram is counts by sex/age in the population of interest.
#' The Individuals data frame requires two columns relating to the age band: one is the minimum age in the age band. The second is the maximum age in the age band. For example, the age band 0 - 4 years would have 0 as the minimum age band value and 4 as the maximum age band value. Each individual must have both the minimum and maximum age variables populated.
#' The variables specifying sex can be numeric, character, or factor. The sole requirement is that the same code is used in both the Individuals and the Pyramid data frames. For example, if "F" and "M" are used in the Individuals data frame to denote sex, then "F" and "M" are the codes required in the Pyramid data frame. Any number of values can be used, so long as they are unique.
#' @export
#' @param Individuals A data frame containing observations with grouped ages. These are the observations to which the sex/age pyramid is applied.
#' @param IndividualSxVariable The column number for the variable that contain the codes specifying females and males.
#' @param MinimumAgeVariable The column number for the variable that contains the minimum age for the age band.
#' @param MaximumAgeVariable The column number for the variable that contains the maximum age for the age band.
#' @param Pyramid A data frame containing the sex/age pyramid to be used.
#' @param PyramicSxVariable The column number for the variable that contain the codes specifying females and males.
#' @param PyramidAgeVariable The column number containing the individual ages.
#' @param Count The column number containing the counts for each sex/age combination in the data
#' @param NewAgeVariable The name to use for the constructed age variable in the output data frame. For each row, this will contain one integer. If not specified, the column name is "SingleAge".
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.


AgeStructure <- function(Individuals, IndividualSxVariable = NULL, MinimumAgeVariable = NULL, MaximumAgeVariable = NULL, Pyramid, PyramidSxVariable = NULL,
                         PyramidAgeVariable = NULL, Count = NULL, NewAgeVariable = "SingleAge", UserSeed = NULL)

{

  options(dplyr.summarise.inform=F)

  #####################################
  # quick reasonableness checks
  #####################################

  if (is.null(IndividualSxVariable)) {
    stop("The column number containing the sex information in the Individuals data frame must be supplied.")
  }

  if (is.null(MinimumAgeVariable)) {
    stop("The column number for the minimum age band value must be supplied.")
  }

  if (is.null(MaximumAgeVariable)) {
    stop("The column number for the maximum age band value must be supplied.")
  }

  if (is.null(PyramidSxVariable)) {
    stop("The column number containing the sex information in the Pyramid data frame must be supplied.")
  }

  if (is.null(PyramidAgeVariable)) {
    stop("The column number containing the age information in the Pyramid data frame must be supplied.")
  }

  if (is.null(Count)) {
    stop("The column number for the sex/age counts must be supplied.")
  }

  #####################################
  #####################################
  # rename variables so don't need to use quosures inside code
  #####################################
  #####################################

  BaseDataFrame <- as.data.frame(Individuals %>%
    rename(Sex = !! IndividualSxVariable, MinAge = !! MinimumAgeVariable, MaxAge = !! MaximumAgeVariable) %>%
    mutate(Sex = as.character(Sex)))

  AgePyramid <- as.data.frame(Pyramid %>%
    rename(Sex = !! PyramidSxVariable, Age = !! PyramidAgeVariable, Prob = !! Count) %>%
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

  PyramidSexCodes <- AgePyramid %>%
    select(Sex) %>%
    distinct(Sex) %>%
    arrange(Sex)


  if (isFALSE(identical(BaseSexCodes, PyramidSexCodes))) {

    stop("The sex variable values are not the same for both data frames.")

  }


  #####################################
  #####################################
  # perform the age allocation
  #####################################
  #####################################

  # seed must come before first sample is cut
  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  # !!!!!!!!!!!!!!!!! note testing restriction
 for (x in 1:nrow(BaseDataFrame)) {
  # for (x in 1:10) {

    CurrentIndividual <- BaseDataFrame[x,]

    AgeRestricted <- AgePyramid %>%
      filter(between(Age, CurrentIndividual$MinAge, CurrentIndividual$MaxAge),
             Sex == CurrentIndividual$Sex) %>%
      select(Age, Prob)

    OutputAge <- AgeRestricted %>%
      slice_sample(n=1, weight_by = Prob) %>%
      select(Age) %>%
      pull(Age)

    CurrentIndividual <- CurrentIndividual %>%
      mutate(!!NewAgeVariable := OutputAge)


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
