#' Add a variable indicating whether the person is at school, or has left school.
#' This function creates a data frame with a variable indicating whether the person still attending school or is not attending school. This is an factor with two levels. Pre-cleaning so that only people inside the age range for attending school is not required.
#' Three data frames are required. The first is the data frame that contains the people ("adolescents") to whom the indicator will be applied. The other two data frames are counts: school leaver counts ("leavers"), and the sex/age pyramid counts ("structure") that apply to the school leaver counts. As cumulative proportions of school leavers are calculated, the leavers data frames must contain multiple years of data. For example, if the minimum school leaving age is 17 and the maximum age is 18, then there must be two years of data in the leavers and structure data frames. The structure data frame contains the sex/age counts for the year of the adolescent data. For example, if the adolescents data frame is based on 2021, then the structure data frame should be the counts for 2021.
#' The variables specifying sex can be numeric, character, or factor. The sole requirement is that the same code is used in all three data frames. For example, if "F" and "M" are used in the adolescents data frame to denote sex, then "F" and "M" are the codes required in both the leavers and structure data frames. Any number of values can be used, so long as they are unique.
#'
#' The proportion of adolescents, by age and sex, who have left school is printed to the Console.
#' @export
#' @param people A data frame containing individual people.
#' @param pplsx The variable that contains the codes specifying females and males, in the people data.
#' @param pplage The variable that contains the ages of the adolescents, in the people data. This must be integer format.
#' @param pplyear The year of the adolescents data.
#' @param minschage The minimum age of a person, normally a child, can enter school, excluding higher education.
#' @param maxschage The maximum age of a person, normally an adolescent, can leave school, excluding higher education.
#' @param leavers A data frame containing the counts of the school leavers for each year.
#' @param lvrsx The variable that contain the codes specifying females and males, in the leavers data.
#' @param lvrage The variable the ages for school leavers, in the leavers data.
#' @param lvrcount The variable containing the counts for each sex/age combination in the leavers data. This must be in numeric or integer format.
#' @param lvryear The variable containing the year data for each count.
#' @param pyramid A data frame containing the sex/age pyramid to be used.
#' @param pyrsx The variable containing the codes specifying females and males, in the pyramid data.
#' @param pyrage The variable containing the ages, in the pyramid data.
#' @param pyrcount The variable containing the counts for each sex/age combination, in the pyramid data
#' @param stvarname The name of the variable to contain the status of the children/adolescents for schooling. The output is "Yes" for those still in school and "No" for those not in school. If not specified, the column name is "Status".
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return A data frame of an observations, with an added column that contains the school status of each person.
#'
#' @examples
#' WithSchoolInd <- schoolind(Township, 1, 4, 2018, 5, 18, LeftSchool, 2, 3, 4, 1,
#' RegionalStructure, 1, 4, 3, "SchoolStatus", userseed = 4)

schoolind <- function(people, pplsx, pplage, pplyear, minschage = NULL, maxschage = NULL,
                      leavers, lvrsx, lvrage, lvrcount, lvryear,
                      pyramid, pyrsx, pyrage, pyrcount, stvarname = "Status",
                    #  structure, strusxcol = NULL, struagecol = NULL, structcol = NULL, stvarname = "Status",
                      userseed = NULL)

{

  options(dplyr.summarise.inform=F)

  #####################################
  # quick reasonableness checks
  #####################################

  if (!pplsx %in% names(people)) {
    stop("The sex variable name in the people data frame is incorrect.")
  }
  
  if (!pplage %in% names(people)) {
    stop("The age variable name in the people data frame is incorrect.")
  }
  
  if (!lvrsx %in% names(leavers)) {
    stop("The sex variable name in the leavers data frame is incorrect.")
  }
  
  if (!lvrage %in% names(leavers)) {
    stop("The count variable name in the leavers data frame is incorrect.")
  }
  
  if (!lvrcount %in% names(leavers)) {
    stop("The leaver count variable name in the leavers data frame is incorrect.")
  }
  
  if (!pyrsx %in% names(pyramid)) {
    stop("The sex variable name in the pyramid data frame is incorrect.")
  }
  
  if (!pyrage %in% names(pyramid)) {
    stop("The age variable name in the pyramid data frame is incorrect.")
  }
  
  if (!pyrcount %in% names(pyramid)) {
    stop("The count variable name in the pyramid data frame is incorrect.")
  }

  #####################################
  #####################################
  # rename variables so don't need to use quosures inside code
  #####################################
  #####################################

  # dataset names
 # cat("The children data frame is called", deparse(substitute(adolescents)), "\n")

  PeopleDataframe <- as.data.frame(people %>%
                                     rename(IntSex = !! pplsx, IntAge = !!pplage) %>%
                                     mutate(IntSex = as.character(IntSex),
                                            IntAge = as.integer(IntAge)))

  # cat("PeopleDataframe generated", "\n")

  Schooling <- as.data.frame(leavers %>%
                              rename(IntSex = !! lvrsx, IntAge = !! lvrage,
                                     Year = !! lvryear, NumLeftSchool = !! lvrcount) %>%
                              mutate(IntSex = as.character(IntSex),
                                     IntAge = as.integer(IntAge),
                                     Year = as.integer(Year),
                                     NumLeftSchool = as.integer(NumLeftSchool)) %>%
                               select(IntSex, IntAge, Year, NumLeftSchool))

  # cat("Schooling dataframe generated", "\n")


  AgePyramid <- as.data.frame(pyramid %>%
                                rename(IntSex = !! pyrsx, IntAge = !! pyrage,
                                       AllPeople = !! pyrcount) %>%
                                mutate(IntSex = as.character(IntSex),
                                       IntAge = as.integer(IntAge)) %>%
                                select(IntSex, IntAge, AllPeople))

  # print(str(AgePyramid))

  # get the original variable names

  PeopleSexColName <- sym(names(people[pplsx]))
  PeopleAgeColName <- sym(names(people[pplage]))
  StatusName <- sym(stvarname)


  #####################################
  #####################################
  # end column names
  #####################################
  #####################################

  #####################################
  # check alignment of the three sex variables codes
  #####################################

  PeopleSexCodes <- PeopleDataframe %>%
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


  if (isFALSE(identical(PeopleSexCodes, PyramidSexCodes))) {

    stop("The sex variable values are not the same for the people and pyramid data frame", "\n")

  }

  if (isFALSE(identical(PeopleSexCodes, SchoolingSexCodes))) {

    stop("The sex variable values for leavers data frame differs from the other two data frames.", "\n")

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
    filter(Year <= pplyear) %>%
    mutate(Deduction = pplyear - Year,
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
    filter(!(is.na(pyrcount)))

 
  ####################################
  ####################################
  # assume that out-of-age people may be in the data frame and remove

  WrongAged <- PeopleDataframe %>%
    filter(IntAge < minschage | IntAge > maxschage) %>%
    mutate(Status = "N") %>%
    rename(!!PeopleAgeColName := IntAge,
           !!PeopleSexColName := IntSex)

  PeopleDataframe <- PeopleDataframe %>%
    filter(between(IntAge, minschage, maxschage))
  ####################################
  ####################################

  # join the probabilities to the children and calculate the probability of leaving school
  PeopleDataframe <- PeopleDataframe %>%
    left_join(CombinedData, by = c("IntAge", "IntSex")) %>%
    mutate(PropLeft = ifelse(is.na(PropLeft), 0, PropLeft),
           Status = "N")


  # seed must come before first sample is cut
  if (!is.null(userseed)) {
    set.seed(userseed)
  }

  # apply the probabilities to the children

  for (x in 1:nrow(PeopleDataframe)) {

    RandomRoll <- runif(1, 0, 1)

  
    if (isTRUE(RandomRoll <= PeopleDataframe$PropLeft[x])) {

   #   print("Entered loop")

      PeopleDataframe$Status[x] <- "N"

    } else {
      PeopleDataframe$Status[x] <- "Y"

      # closes random assignment to school status
    }

    # closes loop through children data frame
  }

  PeopleDataframe <- PeopleDataframe %>%
    rename(!!PeopleAgeColName := IntAge,
           !!PeopleSexColName := IntSex) %>%
  select(-c(PropLeft))


  # print(str(Children))

  CompleteDF <- bind_rows(PeopleDataframe, WrongAged) %>%
    mutate(Status = factor(Status, levels = c("N", "Y"))) %>%
    rename(!!stvarname := Status)
  
  ReportedProps <- CompleteDF %>%
    filter(between(!!PeopleAgeColName, min(AgePyramid$IntAge), max(AgePyramid$IntAge))) %>%
    group_by(!!PeopleAgeColName, !!PeopleSexColName, !!StatusName)  %>%
    summarise(n = n()) %>%
    mutate(PropLeavers = n / sum(n)) %>%
    filter(!!StatusName == "N") %>%
    select(-n)
  
  cat("The proportion of adolescents who have left school are shown in the table below, by sex and age.", "\n")
  
  print(ReportedProps)

  return(CompleteDF)

  #closes function
}
