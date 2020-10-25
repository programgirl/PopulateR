#' Add a variable indicating whether an adolescents is  a child remaining at school or is a school leaver.
#' This function adds a variable to a data frame of adolescents to indicate whether a child is still in school, or has left. The output is the same data as Adolescents, but with an added column that contains the school status variable. The function has been constructed so that the Adolescents data can be at the same level of aggregation, or less aggregate, than the Leavers data.
#' Three data frames are required: the data frame that contains the adolescents ("Adolescents"), a data frame containing school leaver counts ("Leavers"), and a data frame containing or constructing a sex/age pyramid ("Pyramid").
#' The Leavers and Pyramid data frames should be summary data frames of counts. They must also have the same geopraphical base. For example, if the Leavers data is for the Canterbury region in New Zealand, then the Pyramid data must also be at the Canterbury region level of aggregation. In this example, the maximum aggregation level for the Adolescents data is Canterbury region, but could be for any sub-region, such as Christchurch city. However, the function should not be used if the Adolescents data is more aggregate, for example at the South Island or total New Zealand levels of aggregation
#' For each year, counts of school leavers by age and sex are required. For example, if the school leaving age is 16, then the 2020 rows could be: 16-year-old females, 16-year-old males, 17-year-old females, 17-year-old males.
#' The variables specifying sex can be numeric, character, or factor. The sole requirement is that the same code is used in all three data frames. For example, if "F" and "M" are used in the Adolescents data frame to denote sex, then "F" and "M" are the codes required in the Leavers and Pyramid data frames. Any number of values can be used, so long as they are unique.
#' @export
#' @param Adolescents A data frame containing all adolescents who potentially have left school due to their age.
#' @param AdolescentSxVariable The column number for the variable that contain the codes specifying females and males.
#' @param AdolescentAgeVariable The column number for the variable that contains the ages of the adolescents. This must be integer format.
#' @param LeavingAge The minimum age at which an adolescent can leave school.
#' @param Leavers A data frame containing the counts of the school leavers for each year.
#' @param LeaversSxVariable The column number for the variable that contain the codes specifying females and males.
#' @param LeaversAgeVariable The column number containing the ages for school leavers.
#' @param LeaversCount The column number containing the counts for each sex/age combination in the data. This must be integer format.
#' @param Year The column number containing the year data for each count. This must be integer format. The most recent year of leaver data is assumed to align with the ages of the adolescents. For example, if the most recent year is 2020, then the 2020 leaver information for 17-year-olds is applied to the 17-year-olds in the Adolescent data frame.
#' @param Pyramid A data frame containing the sex/age pyramid to be used.
#' @param PyramicSxVariable The column number for the variable that contain the codes specifying females and males.
#' @param PyramidAgeVariable The column number containing the individual ages.
#' @param PyramidCount The column number containing the counts for each sex/age combination in the data
#' @param SchoolStatus The name of the variable that contains the school stayer/leaver indicator. If not specified, the column name is "Status".
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.


SchoolLeavers <- function(Adolescents, AdolescentSxVariable = NULL, AdolescentAgeVariable = NULL, LeavingAge = NULL, Leavers, LeaversSxVariable = NULL,
                          LeaversAgeVariable = NULL, LeaversCount = NULL, Year = NULL, Pyramid, PyramicSxVariable = NULL, PyramidAgeVariable = NULL,
                          PyramidCount = NULL, SchoolStatus = "Status", UserSeed = NULL)

{

  options(dplyr.summarise.inform=F)

  #####################################
  # quick reasonableness checks
  #####################################

  if (is.null(AdolescentSxVariable)) {
    stop("The column number containing the sex information in the Adolescents data frame must be supplied.")
  }

  if (is.null(AdolescentAgeVariable)) {
    stop("The column number containing the age information in the Adolescents data frame must be supplied.")
  }

  if (is.null(LeavingAge)) {
    stop("The minimum school leaving age must be supplied.")
  }

  if (is.null(LeaversSxVariable)) {
    stop("The column number containing the sex information in the Leavers data frame must be supplied.")
  }

  if (is.null(LeaversAgeVariable)) {
    stop("The column number containing the age information in the Leavers data frame must be supplied.")
  }

  if (is.null(Count)) {
    stop("The column number for the sex/age school leaver counts must be supplied.")
  }

  if (is.null(Year)) {
    stop("The column number containing the year information in the LeaversData data frame must be supplied.")
  }

  if (is.null(PyramidSxVariable)) {
    stop("The column number containing the sex information in the Pyramid data frame must be supplied.")
  }

  if (is.null(PyramidAgeVariable)) {
    stop("The column number containing the age information in the Pyramid data frame must be supplied.")
  }

  if (is.null(PyramidCount)) {
    stop("The column number for the sex/age information in the Pyramid data counts must be supplied.")
  }

  #####################################
  #####################################
  # rename variables so don't need to use quosures inside code
  #####################################
  #####################################

  Children <- as.data.frame(Adolescents %>%
                                   rename(Sex = !! AdolescentSxVariable, Age = !! AdolescentAgeVariable) %>%
                                   mutate(Sex = as.character(Sex)))

  Schooling <- as.data.frame(Leavers %>%
                              rename(Sex = !! LeaversSxVariable, Age = !! AdolescentAgeVariable) %>%
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
  # check alignment of the three sex variables codes
  #####################################

  ChildrenSexCodes <- Children %>%
    select(Sex) %>%
    distinct(Sex)

  SchoolingSexCodes <- Schooling %>%
    select(Sex) %>%
    distinct(Sex)

  PyramidSexCodes <- AgePyramid %>%
    select(Sex) %>%
    distinct(Sex)


#   if (isFALSE(identical(BaseSexCodes, PyramidSexCodes))) {
#
#     stop("The sex variable values are not the same for both data frames.")
#
#   }
#
#
#   #####################################
#   #####################################
#   # perform the age allocation
#   #####################################
#   #####################################
#
#   # seed must come before first sample is cut
#   if (!is.null(UserSeed)) {
#     set.seed(UserSeed)
#   }
#
#   # !!!!!!!!!!!!!!!!! note testing restriction
#   for (x in 1:nrow(BaseDataFrame)) {
#     # for (x in 1:10) {
#
#     CurrentIndividual <- BaseDataFrame[x,]
#
#     AgeRestricted <- AgePyramid %>%
#       filter(between(Age, CurrentIndividual$MinAge, CurrentIndividual$MaxAge),
#              Sex == CurrentIndividual$Sex) %>%
#       select(Age, Prob)
#
#     OutputAge <- AgeRestricted %>%
#       slice_sample(n=1, weight_by = Prob) %>%
#       select(Age) %>%
#       pull(Age)
#
#     CurrentIndividual <- CurrentIndividual %>%
#       mutate(!!NewAgeVariable := OutputAge)
#
#
#     if (exists("DataFrameWithAges") == TRUE) {
#
#       DataFrameWithAges <- bind_rows(DataFrameWithAges, CurrentIndividual)
#
#     } else {
#
#       DataFrameWithAges <- CurrentIndividual
#
#       # close evaluation for creating the output data frame
#     }
#
#     # closes age assignment loop
#
#   }

  #
  return(DataFrameWithAges)

  #closes function
}
