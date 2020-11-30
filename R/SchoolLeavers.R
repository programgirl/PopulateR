#' Add a variable indicating whether an adolescents is  a child remaining at school or is a school leaver.
#' This function adds a variable to a data frame of adolescents to indicate whether a child is still in school, or has left. The output is the same data as Adolescents, but with an added column that contains the school status variable. The function has been constructed so that the Adolescents data can be at the same level of aggregation, or less aggregate, than the Leavers data. The function works for all ages,so there is no need to use a subset data frame containing only the ages at which a child/adolescent can leave school.
#' Three data frames are required: the data frame that contains the adolescents ("Adolescents"), a data frame containing school leaver counts ("Leavers"), and a data frame containing or constructing a sex/age pyramid ("Pyramid").
#' The Leavers and Pyramid data frames should be summary data frames of counts. They must also have the same geopraphical base. For example, if the Leavers data is for the Canterbury region in New Zealand, then the Pyramid data must also be at the Canterbury region level of aggregation. In this example, the maximum aggregation level for the Adolescents data is Canterbury region, but could be for any sub-region, such as Christchurch city. However, the function should not be used if the Adolescents data is more aggregate, for example at the South Island or total New Zealand levels of aggregation
#' For each year, counts of school leavers by age and sex are required. For example, if the school leaving age is 16, then the 2020 rows could be: 16-year-old females, 16-year-old males, 17-year-old females, 17-year-old males.
#' The variables specifying sex can be numeric, character, or factor. The sole requirement is that the same code is used in all three data frames. For example, if "F" and "M" are used in the Adolescents data frame to denote sex, then "F" and "M" are the codes required in the Leavers and Pyramid data frames. Any number of values can be used, so long as they are unique.
#' @export
#' @param Adolescents A data frame containing all adolescents who potentially have left school due to their age.
#' @param AdolescentSxVariable The column number for the variable that contain the codes specifying females and males.
#' @param AdolescentAgeVariable The column number for the variable that contains the ages of the adolescents. This must be integer format.
#' @param AdolescentsYear The year that is most relevant to the timing of the adolescents survey date. For example, an adolescents survey date in early 2013, with a school year of Febrary to November, may mean that the ages of the school leavers in 2012 are the same as the adolescents' current ages. In this situation, most of the school exits in 2013 will occur after the date of the adolescent survey, and the adolescent may have had a birthday inbetween. The 2012 school leaver data is therefore latest and most accurate data to use for the school leaver estimates of the 2013 survey. Must be integer or numeric.
#' @param MinSchoolAge The minimum age of a person, normally a child, can enter school, excluding higher education.
#' @param MaxSchoolAge The maximum age of a person, normally an adolescent, can leaver school, excluding higher education.
#' @param Leavers A data frame containing the counts of the school leavers for each year.
#' @param LeaversSxVariable The column number for the variable that contain the codes specifying females and males.
#' @param LeaversAgeVariable The column number containing the ages for school leavers.
#' @param LeaversCount The column number containing the counts for each sex/age combination in the data. This must be in numeric or integer format.
#' @param LeaversYear The column number containing the year data for each count. This must be integer format. The most recent year of leaver data is assumed to align with the ages of the adolescents. For example, if the most recent year is 2013, then the 2013 leaver information for 17-year-old females is applied to the 17-year-old females in the Adolescent data frame.
#' @param Pyramid A data frame containing the sex/age pyramid to be used.
#' @param PyramicSxVariable The column number for the variable that contain the codes specifying females and males.
#' @param PyramidAgeVariable The column number containing the individual ages.
#' @param PyramidCount The column number containing the counts for each sex/age combination in the data
#' @param SchoolStatus The name of the variable to contain the status of the children/adolescents for schooling. The output is "Yes" for those still in school and "No" for those not in school.. If not specified, the column name is "Status".
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.

SchoolLeavers <- function(Adolescents, AdolescentSxVariable = NULL, AdolescentAgeVariable = NULL, AdolescentsYear = NULL, MinSchoolAge = NULL, MaxSchoolAge = NULL,
                          Leavers, LeaversSxVariable = NULL, LeaversAgeVariable = NULL, LeaversCount = NULL, LeaversYear = NULL,
                          Pyramid, PyramidSxVariable = NULL, PyramidAgeVariable = NULL, PyramidCount = NULL, SchoolStatus = "Status", UserSeed = NULL)

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

  if (is.null(AdolescentsYear)) {
    stop("The year in which the adolescents data was collected.")
  }

  if (is.null(LeaversSxVariable)) {
    stop("The column number containing the sex information in the Leavers data frame must be supplied.")
  }

  if (is.null(LeaversAgeVariable)) {
    stop("The column number containing the age information in the Leavers data frame must be supplied.")
  }

  if (is.null(LeaversCount)) {
    stop("The column number for the sex/age school leaver counts must be supplied.")
  }

  if (is.null(LeaversYear)) {
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

  # dataset names
 # cat("The children data frame is called", deparse(substitute(Adolescents)), "\n")

  Children <- as.data.frame(Adolescents %>%
                                   rename(IntSex = !! AdolescentSxVariable, IntAge = !! AdolescentAgeVariable) %>%
                                   mutate(IntSex = as.character(IntSex),
                                          IntAge = as.integer(IntAge)))

  Schooling <- as.data.frame(Leavers %>%
                              rename(IntSex = !! LeaversSxVariable, IntAge = !! LeaversAgeVariable, Year = !! LeaversYear, NumLeftSchool = !! LeaversCount) %>%
                              mutate(IntSex = as.character(IntSex),
                                     IntAge = as.integer(IntAge),
                                     Year = as.integer(Year),
                                     NumLeftSchool = as.integer(NumLeftSchool)) %>%
                               select(IntSex, IntAge, Year, NumLeftSchool))


  AgePyramid <- as.data.frame(Pyramid %>%
                                rename(IntSex = !! PyramidSxVariable, IntAge = !! PyramidAgeVariable, PyramidCount = !! LeaversCount) %>%
                                mutate(IntSex = as.character(IntSex),
                                       IntAge = as.integer(IntAge)) %>%
                                select(IntSex, IntAge, PyramidCount))

  # get the original variable names

  ChildrenAgeColName <- sym(names(Adolescents[AdolescentAgeVariable]))
  ChildrenSexColName <- sym(names(Adolescents[AdolescentSxVariable]))

  #####################################
  #####################################
  # end column names
  #####################################
  #####################################

  #####################################
  # check alignment of the three sex variables codes
  #####################################

  ChildrenSexCodes <- Children %>%
    select(IntSex) %>%
    distinct(IntSex) %>%
    arrange(IntSex)

  SchoolingSexCodes <- Schooling %>%
    select(IntSex) %>%
    distinct(IntSex) %>%
    arrange(IntSex)

  PyramidSexCodes <- AgePyramid %>%
    select(IntSex) %>%
    distinct(IntSex) %>%
    arrange(IntSex)


  if (isFALSE(identical(ChildrenSexCodes, PyramidSexCodes))) {

    stop("The sex variable values are not the same for the ", deparse(substitute(Adolescents)), " and ", deparse(substitute(Pyramid)), " data frames.", "\n")

  }

  if (isFALSE(identical(ChildrenSexCodes, SchoolingSexCodes))) {

    stop("The sex variable values for ", deparse(substitute(Leavers)), " differ from the other two data frames.", "\n")

  }


  #####################################
  #####################################
  # check leaver counts are summarised
  #####################################
  #####################################

  DuplicateTesting <- Schooling %>%
    group_by(Year, IntSex, IntAge) %>%
    summarise(Duplicates = n()) %>%
    filter(Duplicates > 1)


  if (!(is.na(DuplicateTesting$Year[1])) == TRUE) {

    stop(deparse(substitute(Leavers)), " contains duplicates.", "\n")
  }

  #####################################
  #####################################
  # sum the leaver counts by current age
  #####################################
  #####################################

  Schooling <- Schooling %>%
    filter(Year <= AdolescentsYear) %>%
    mutate(Deduction = AdolescentsYear - Year,
           CurrentAge = IntAge + Deduction) %>%
    group_by(IntSex, CurrentAge) %>%
    summarise(TotalLeaverCount = sum(NumLeftSchool)) %>%
    filter(CurrentAge <= MaxSchoolAge)

  ####################################
  ####################################
  # join after the leavers are summarised
  ####################################
  ####################################
  CombinedData <- Schooling %>%
    left_join(AgePyramid, by = c("IntSex", "CurrentAge" =  "IntAge")) %>%
    mutate(PropLeft = TotalLeaverCount / PyramidCount,
           PropLeft = ifelse(PropLeft > 1, 1, PropLeft)) %>%
    filter(!(is.na(PropLeft))) %>%
    rename(IntAge = CurrentAge) %>%
    select(-(c(TotalLeaverCount, PyramidCount)))


  # remove any NAs as these will cause problems with the maths
  CombinedData <- CombinedData %>%
    filter(!(is.na(PyramidCount)))

  cat("The proportion of adolescents who have left school are shown in the table below, by sex and age.", "\n")

  print(CombinedData)

  ####################################
  ####################################
  # assume that out-of-age people may be in the data frame and remove

  WrongAged <- Children %>%
    filter(IntAge < MinSchoolAge | IntAge > MaxSchoolAge) %>%
    mutate(Status = "No") %>%
    rename(!!ChildrenAgeColName := IntAge,
           !!ChildrenSexColName := IntSex)

  Children <- Children %>%
    filter(between(IntAge, MinSchoolAge, MaxSchoolAge))
  ####################################
  ####################################

  # join the probabilities to the children and calculate the probability of leaving school
  Children <- Children %>%
    left_join(CombinedData, by = c("IntAge", "IntSex")) %>%
    mutate(PropLeft = ifelse(is.na(PropLeft), 0, PropLeft),
           Status = "None")

  # seed must come before first sample is cut
  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  # apply the probabilities to the children

  for (x in 1:nrow(Children)) {

    RandomRoll <- runif(1, 0, 1)

 #   cat("Random roll is ", RandomRoll, " for the line ", Children$PropLeft[x], "\n")

    if (isTRUE(RandomRoll <= Children$PropLeft[x])) {

   #   print("Entered loop")

      Children$Status[x] <- "No"

    } else {
      Children$Status[x] <- "Yes"

      # closes random assignment to school statust
    }

    # closes loop through children data frame
  }

  Children <- Children %>%
    rename(!!ChildrenAgeColName := IntAge,
           !!ChildrenSexColName := IntSex) %>%
  select(-c(PropLeft))

  CompleteDF <- bind_rows(Children, WrongAged) %>%
    rename(!!SchoolStatus := Status)


  return(CompleteDF)

  #closes function
}
