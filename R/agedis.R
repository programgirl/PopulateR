#' Add a sex/age structure to a data frame of grouped ages.
#' This function creates a data frame that adds an age structure to a data frame that contains age bands.
#' Two data frames are required: the data frame that contains individuals with age bands ("individuals"), and a data frame used as the basis for constructing a sex/age pyramid ("pyramid"). The pyramid data frame must contain counts by sex/age in the population of interest.
#' The individuals data frame requires two columns relating to the age band. One is the minimum age in the age band. The second is the maximum age in the age band. For example, the age band 0 - 4 years would have 0 as the minimum age band value and 4 as the maximum age band value. Each person in the individuals data frame must have both the minimum and maximum age variables populated.
#' The variables specifying sex can be numeric, character, or factor. The sole requirement is that the codes must match.  For example, if "F" and "M" are used in the individuals data frame to denote sex, then "F" and "M" are the codes required in the pyramid data frame. Any number of sex code values can be used, so long as they are unique.
#' @export
#' @param individuals A data frame containing observations with grouped ages. These are the observations to which the sex/age pyramid is applied.
#' @param indsxcol The column number for the variable that contain the codes specifying females and males.
#' @param minagecol The column number for the variable that contains the minimum age for the age band.
#' @param maxagecol The column number for the variable that contains the maximum age for the age band.
#' @param pyramid A data frame containing the sex/age pyramid to be used.
#' @param pyrsxcol The column number for the variable that contain the codes specifying females and males.
#' @param pyragecol The column number containing the individual ages.
#' @param pyrcountcol The column number containing the counts for each sex/age combination in the data
#' @param agevarname The name to use for the constructed age variable in the output data frame. For each row, this will contain one integer. If not specified, the column name is "SingleAge".
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.

#' @return A data frame of an observations, with an added column that contains the age.

#' @examples
#' DisaggregateAge <- agedis(Relationships, indsxcol = 1, minagecol = 4, maxagecol = 5, SingleAges, pyrsxcol = 2,
#' pyragecol = 4, pyrcountcol = 3, agevarname = "TheAge", UserSeed = 4)

agedis <- function(individuals, indsxcol = NULL, minagecol = NULL, maxagecol = NULL, pyramid, pyrsxcol = NULL,
                         pyragecol = NULL, pyrcountcol = NULL, agevarname = "SingleAge", UserSeed = NULL)

{

  options(dplyr.summarise.inform=F)

  #####################################
  # quick reasonableness checks
  #####################################

  if (is.null(indsxcol)) {
    stop("The column number containing the sex information in the individuals data frame must be supplied.")
  }

  if (is.null(minagecol)) {
    stop("The column number for the minimum age band value must be supplied.")
  }

  if (is.null(maxagecol)) {
    stop("The column number for the maximum age band value must be supplied.")
  }

  if (is.null(pyrsxcol)) {
    stop("The column number containing the sex information in the pyramid data frame must be supplied.")
  }

  if (is.null(pyragecol)) {
    stop("The column number containing the age information in the pyramid data frame must be supplied.")
  }

  if (is.null(pyrcountcol)) {
    stop("The column number for the sex/age counts must be supplied.")
  }

  #####################################
  #####################################
  # rename variables so don't need to use quosures inside code
  #####################################
  #####################################

  BaseDataFrame <- as.data.frame(individuals %>%
    rename(Sex = !! indsxcol, MinAge = !! minagecol, MaxAge = !! maxagecol) %>%
    mutate(Sex = as.character(Sex)))

  Agepyramid <- as.data.frame(pyramid %>%
    rename(Sex = !! pyrsxcol, Age = !! pyragecol, Prob = !! pyrcountcol) %>%
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
  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  # !!!!!!!!!!!!!!!!! note testing restriction
 for (x in 1:nrow(BaseDataFrame)) {
  # for (x in 1:10) {

    CurrentIndividual <- BaseDataFrame[x,]

    AgeRestricted <- Agepyramid %>%
      filter(between(Age, CurrentIndividual$MinAge, CurrentIndividual$MaxAge),
             Sex == CurrentIndividual$Sex) %>%
      select(Age, Prob)

    OutputAge <- AgeRestricted %>%
      slice_sample(n=1, weight_by = Prob) %>%
      select(Age) %>%
      pull(Age)

    CurrentIndividual <- CurrentIndividual %>%
      mutate(!!agevarname := OutputAge)


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
