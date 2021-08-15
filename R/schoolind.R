#' Add a variable indicating whether the person is at school, or has left school.
#' This function creates a data frame with a variable indicating whether the person still attending school or is not attending school. This is an factor with two levels. Pre-cleaning so that only people inside the age range for attending school is not required.
#' Three data frames are required. The first is the data frame that contains the people ("adolescents") to whom the indicator will be applied. The other two data frames are counts: school leaver counts ("leavers"), and the sex/age pyramid counts ("structure") that apply to the school leaver counts. As cumulative proportions of school leavers are calculated, the leavers data frames must contain multiple years of data. For example, if the minimum school leaving age is 17 and the maximum age is 18, then there must be two years of data in the leavers and structure data frames. The structure data frame contains the sex/age counts for the year of the adolescent data. For example, if the adolescents data frame is based on 2021, then the structure data frame should be the counts for 2021.
#' The variables specifying sex can be numeric, character, or factor. The sole requirement is that the same code is used in all three data frames. For example, if "F" and "M" are used in the adolescents data frame to denote sex, then "F" and "M" are the codes required in both the leavers and structure data frames. Any number of values can be used, so long as they are unique.
#'
#' The proportion of adolescents, by age and sex, who have left school is printed to the Console.
#' @export
#' @param adolescents A data frame containing all adolescents who potentially have left school due to their age.
#' @param adlsxcol The column number for the variable that contain the codes specifying females and males.
#' @param adlagecol The column number for the variable that contains the ages of the adolescents. This must be integer format.
#' @param adlyear The year of the adolescents data.
#' @param minschage The minimum age of a person, normally a child, can enter school, excluding higher education.
#' @param maxschage The maximum age of a person, normally an adolescent, can leave school, excluding higher education.
#' @param leavers A data frame containing the counts of the school leavers for each year.
#' @param lvrsxcol The column number for the variable that contain the codes specifying females and males.
#' @param lvragecol The column number containing the ages for school leavers.
#' @param lvrctcol The column number containing the counts for each sex/age combination in the data. This must be in numeric or integer format.
#' @param lvryearcol The column number containing the year data for each count.
#' @param structure A data frame containing the sex/age pyramid to be used.
#' @param strusxcol The column number for the variable that contain the codes specifying females and males.
#' @param struagecol The column number containing the individual ages.
#' @param structcol The column number containing the counts for each sex/age combination in the data
#' @param stvarname The name of the variable to contain the status of the children/adolescents for schooling. The output is "Yes" for those still in school and "No" for those not in school. If not specified, the column name is "Status".
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return A data frame of an observations, with an added column that contains the school status of each person.
#'
#' @examples
#' WithSchoolInd <- schoolind(Township, 1, 4, 2018, 5, 18, LeftSchool, 2, 3, 4, 1,
#' RegionalStructure, 1, 4, 3, "SchoolStatus", userseed = 4)

schoolind <- function(adolescents, adlsxcol = NULL, adlagecol = NULL, adlyear = NULL, minschage = NULL, maxschage = NULL,
                      leavers, lvrsxcol = NULL, lvragecol = NULL, lvrctcol = NULL, lvryearcol = NULL,
                      structure, strusxcol = NULL, struagecol = NULL, structcol = NULL, stvarname = "Status",
                      userseed = NULL)

{

  options(dplyr.summarise.inform=F)

  #####################################
  # quick reasonableness checks
  #####################################

  if (is.null(adlsxcol)) {
    stop("The column number containing the sex information in the adolescents data frame must be supplied.")
  }

  if (is.null(adlagecol)) {
    stop("The column number containing the age information in the adolescents data frame must be supplied.")
  }

  if (is.null(adlyear)) {
    stop("The year in which the adolescents data was collected.")
  }

  if (is.null(lvrsxcol)) {
    stop("The column number containing the sex information in the leavers data frame must be supplied.")
  }

  if (is.null(lvragecol)) {
    stop("The column number containing the age information in the leavers data frame must be supplied.")
  }

  if (is.null(lvrctcol)) {
    stop("The column number for the sex/age school leaver counts must be supplied.")
  }

  if (is.null(lvryearcol)) {
    stop("The column number containing the year information in the leaversData data frame must be supplied.")
  }

  if (is.null(strusxcol)) {
    stop("The column number containing the sex information in the structure data frame must be supplied.")
  }

  if (is.null(struagecol)) {
    stop("The column number containing the age information in the structure data frame must be supplied.")
  }

  if (is.null(structcol)) {
    stop("The column number for the sex/age information in the structure data counts must be supplied.")
  }

  #####################################
  #####################################
  # rename variables so don't need to use quosures inside code
  #####################################
  #####################################

  # dataset names
 # cat("The children data frame is called", deparse(substitute(adolescents)), "\n")

  Children <- as.data.frame(adolescents %>%
                                   rename(IntSex = !! adlsxcol, IntAge = !! adlagecol) %>%
                                   mutate(IntSex = as.character(IntSex),
                                          IntAge = as.integer(IntAge)))

  # cat("Children dataframe generated", "\n")

  Schooling <- as.data.frame(leavers %>%
                              rename(IntSex = !! lvrsxcol, IntAge = !! lvragecol,
                                     Year = !! lvryearcol, NumLeftSchool = !! lvrctcol) %>%
                              mutate(IntSex = as.character(IntSex),
                                     IntAge = as.integer(IntAge),
                                     Year = as.integer(Year),
                                     NumLeftSchool = as.integer(NumLeftSchool)) %>%
                               select(IntSex, IntAge, Year, NumLeftSchool))

  # cat("Schooling dataframe generated", "\n")


  AgePyramid <- as.data.frame(structure %>%
                                rename(IntSex = !! strusxcol, IntAge = !! struagecol,
                                       AllPeople = !! structcol) %>%
                                mutate(IntSex = as.character(IntSex),
                                       IntAge = as.integer(IntAge)) %>%
                                select(IntSex, IntAge, AllPeople))

 # print(str(AgePyramid))

  # get the original variable names

  ChildrenAgeColName <- sym(names(adolescents[adlagecol]))
  ChildrenSexColName <- sym(names(adolescents[adlsxcol]))

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

    stop("The sex variable values are not the same for the ", deparse(substitute(adolescents)), " and ", deparse(substitute(structure)), " data frames.", "\n")

  }

  if (isFALSE(identical(ChildrenSexCodes, SchoolingSexCodes))) {

    stop("The sex variable values for ", deparse(substitute(leavers)), " differ from the other two data frames.", "\n")

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

    stop(deparse(substitute(leavers)), " contains duplicates.", "\n")
  }

  #####################################
  #####################################
  # sum the leaver counts by current age
  #####################################
  #####################################

  Schooling <- Schooling %>%
    filter(Year <= adlyear) %>%
    mutate(Deduction = adlyear - Year,
           CurrentAge = IntAge + Deduction) %>%
    group_by(IntSex, CurrentAge) %>%
    summarise(TotalLeaverCount = sum(NumLeftSchool)) %>%
    filter(CurrentAge <= maxschage)

  ####################################
  ####################################
  # join after the leavers are summarised
  ####################################
  ####################################
  CombinedData <- Schooling %>%
    left_join(AgePyramid, by = c("IntSex", "CurrentAge" =  "IntAge")) %>%
    mutate(PropLeft = TotalLeaverCount / AllPeople,
           PropLeft = ifelse(PropLeft > 1, 1, PropLeft)) %>%
    filter(!(is.na(PropLeft))) %>%
    rename(IntAge = CurrentAge) %>%
   select(-(c(TotalLeaverCount, AllPeople)))

  # remove any NAs as these will cause problems with the maths
  CombinedData <- CombinedData %>%
    filter(!(is.na(structcol)))

  cat("The proportion of adolescents who have left school are shown in the table below, by sex and age.", "\n")

  print(CombinedData)

  ####################################
  ####################################
  # assume that out-of-age people may be in the data frame and remove

  WrongAged <- Children %>%
    filter(IntAge < minschage | IntAge > maxschage) %>%
    mutate(Status = "N") %>%
    rename(!!ChildrenAgeColName := IntAge,
           !!ChildrenSexColName := IntSex)

  Children <- Children %>%
    filter(between(IntAge, minschage, maxschage))
  ####################################
  ####################################

  # join the probabilities to the children and calculate the probability of leaving school
  Children <- Children %>%
    left_join(CombinedData, by = c("IntAge", "IntSex")) %>%
    mutate(PropLeft = ifelse(is.na(PropLeft), 0, PropLeft),
           Status = "None")

  # seed must come before first sample is cut
  if (!is.null(userseed)) {
    set.seed(userseed)
  }

  # apply the probabilities to the children

  for (x in 1:nrow(Children)) {

    RandomRoll <- runif(1, 0, 1)

 #   cat("Random roll is ", RandomRoll, " for the line ", Children$PropLeft[x], "\n")

    if (isTRUE(RandomRoll <= Children$PropLeft[x])) {

   #   print("Entered loop")

      Children$Status[x] <- "N"

    } else {
      Children$Status[x] <- "Y"

      # closes random assignment to school statust
    }

    # closes loop through children data frame
  }

  Children <- Children %>%
    rename(!!ChildrenAgeColName := IntAge,
           !!ChildrenSexColName := IntSex) %>%
  select(-c(PropLeft))


  # print(str(Children))

  CompleteDF <- bind_rows(Children, WrongAged) %>%
    mutate(Status = factor(Status, levels = c("N", "Y"))) %>%
    rename(!!stvarname := Status)

  return(CompleteDF)

  #closes function
}
