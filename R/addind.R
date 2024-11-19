#' @importFrom data.table :=
#' @importFrom dplyr arrange between bind_rows distinct filter group_by left_join mutate n rename select summarise
#' @importFrom magrittr %>%
#' @importFrom rlang sym !!
NULL

#' Add a variable indicating whether the person is in education, or has left education
#'
#' Creates a data frame with a variable indicating whether the person is a student, or is not in education. This is an factor with two levels. Pre-cleaning so that only people inside the student age range is not required.
#' Three data frames are required. The first is the data frame that contains the people ("people") to whom the indicator will be applied. The other two data frames are counts: school leaver counts ("leavers"), and the sex/age pyramid counts ("pyramid") that apply to the school leaver counts. As cumulative proportions of school leavers are calculated, the leavers data frames must contain multiple years of data. For example, if the minimum school leaving age is 17 and the maximum age is 18, then there must be two years of data in the leavers data frame. The pyramid data frame contains the sex/age counts for the relevant year. For example, if the people data frame is based on 2021 data frame, then the pyramid data frame should be the counts for 2021, and the value for pplyear would be 2021.
#' The variables specifying sex can be numeric, character, or factor. The sole requirement is that the same code is used in all three data frames. For example, if "F" and "M" are used in the adolescents data frame to denote sex, then "F" and "M" are the codes required in both the leavers and pyramid data frames. Any number of values can be used, so long as they are unique.
#'
#' The proportion of people, by age and sex, who have left school is printed to the console.
#' @export
#' @param people A data frame containing individual people.
#' @param pplid The variable containing the unique identifier for each person, in the people data frame
#' @param pplsx The variable containing the codes for sex, in the people data frame.
#' @param pplage The variable containing the ages, in the people data frame.
#' @param pplyear The year associated with the people data frame.
#' @param minedage The minimum age that a person, normally a child, can enter education.
#' @param maxedage The maximum age that a person, normally an adolescent, can leave education.
#' @param leavers A data frame containing the counts, by sex, age, and year, of the people who have left education.
#' @param lvrsx The variable containing the codes for sex, in the leavers data.
#' @param lvrage The variable containing the codes for sex, in the leavers data.
#' @param lvryear The variable containing the year for the lvrcount.
#' @param lvrcount The variable containing the counts for each sex/age combination in the leavers data.
#' @param pyramid A data frame containing the sex/age pyramid to be used.
#' @param pyrsx The variable containing the codes for sex, in the pyramid data.
#' @param pyrage The variable containing the ages, in the pyramid data.
#' @param pyrcount The variable containing the counts for each sex/age combination, in the pyramid data
#' @param stvarname The name of the variable to contain the education status. The output is "Y" for those still in education and "N" for those not in education.
#' @param userseed If specified, this will set the seed to the number provided. If not, the normal set.seed() function will be used.
#'
#' @return A data frame of an observations, with an added column that contains the education status of each person.
#'
#' @examples
#' WithInd <- addind(Township, pplid = "ID", pplsx = "Sex", pplage = "Age", pplyear = 2018,
#'                   minedage = 5, maxedage = 18, LeftSchool, lvrsx = "Sex", lvrage = "Age",
#'                   lvryear = "YearLeft", lvrcount = "Total", RegionalStructure,
#'                   pyrsx = "Sex", pyrage = "Age", pyrcount = "Value", stvarname = "Status",
#'                   userseed = 4)

addind <- function(people, pplid, pplsx, pplage, pplyear, minedage = NULL, maxedage = NULL,
                   leavers, lvrsx, lvrage, lvryear, lvrcount,
                   pyramid, pyrsx, pyrage, pyrcount, stvarname = "Status",
                   userseed = NULL)

{

  withr::local_options(dplyr.summarise.inform=F)

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
                                     rename(IntSex = !! pplsx,
                                            IntAge = !!pplage,
                                            IntID = !!pplid) %>%
                                     mutate(IntSex = as.character(.data$IntSex),
                                            IntAge = as.integer(.data$IntAge)))

  # cat("PeopleDataframe generated", "\n")

  Schooling <- as.data.frame(leavers %>%
                               rename(IntSex = !! lvrsx, IntAge = !! lvrage,
                                      Year = !! lvryear, NumLeftSchool = !! lvrcount) %>%
                               mutate(IntSex = as.character(.data$IntSex),
                                      IntAge = as.integer(.data$IntAge),
                                      Year = as.integer(.data$Year),
                                      NumLeftSchool = as.integer(.data$NumLeftSchool)) %>%
                               select("IntSex", "IntAge", "Year", "NumLeftSchool"))

  # cat("Schooling dataframe generated", "\n")


  AgePyramid <- as.data.frame(pyramid %>%
                                rename(IntSex = !! pyrsx, IntAge = !! pyrage,
                                       AllPeople = !! pyrcount) %>%
                                mutate(IntSex = as.character(.data$IntSex),
                                       IntAge = as.integer(.data$IntAge)) %>%
                                select("IntSex", "IntAge", "AllPeople"))

  # print(str(AgePyramid))

  # get the original variable names

  PeopleSexColName <- sym(names(people[pplsx]))
  PeopleAgeColName <- sym(names(people[pplage]))
  PeopleIDColName <- sym(names(people[pplid]))
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
    select("IntSex") %>%
    distinct(.data$IntSex) %>%
    arrange(.data$IntSex)

  SchoolingSexCodes <- Schooling %>%
    select("IntSex") %>%
    distinct(.data$IntSex) %>%
    arrange(.data$IntSex)

  PyramidSexCodes <- AgePyramid %>%
    select("IntSex") %>%
    distinct(.data$IntSex) %>%
    arrange(.data$IntSex)


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
    group_by(.data$Year, .data$IntSex, .data$IntAge) %>%
    summarise(Duplicates = n()) %>%
    filter(.data$Duplicates > 1)


  if (!(is.na(DuplicateTesting$Year[1])) == TRUE) {

    stop(deparse(substitute(leavers)), " contains duplicates.", "\n")
  }

  #####################################
  #####################################
  # sum the leaver counts by current age
  #####################################
  #####################################

  Schooling <- Schooling %>%
    filter(.data$Year <= pplyear) %>%
    mutate(Deduction = pplyear - .data$Year,
           CurrentAge = .data$IntAge + .data$Deduction) %>%
    group_by(.data$IntSex, .data$CurrentAge) %>%
    summarise(TotalLeaverCount = sum(.data$NumLeftSchool)) %>%
    filter(.data$CurrentAge <= maxedage)

  ####################################
  ####################################
  # join after the leavers are summarised
  ####################################
  ####################################
  CombinedData <- Schooling %>%
    left_join(AgePyramid, by = c("IntSex", "CurrentAge" =  "IntAge")) %>%
    mutate(PropLeft = .data$TotalLeaverCount / .data$AllPeople,
           PropLeft = ifelse(.data$PropLeft > 1, 1, .data$PropLeft)) %>%
    filter(!(is.na(.data$PropLeft))) %>%
    rename(IntAge = "CurrentAge") %>%
    select(-(c("TotalLeaverCount", "AllPeople")))

  # remove any NAs as these will cause problems with the maths
  CombinedData <- CombinedData %>%
    filter(!(is.na(pyrcount)))


  # create data frame that is not amended as the function works through the ages for the school indicator
  potEducationDataframe <- PeopleDataframe %>%
    filter(between(.data$IntAge, minedage, maxedage))
  ####################################
  ####################################


  # seed must come before first sample is cut
  if (!is.null(userseed)) {
    set.seed(userseed)
  }

  # apply the probabilities to the children
  # do by age and sex

  for(i in 1:nrow(CombinedData)) {

    currentCombinedData <- CombinedData[i,]

    currentAge <- currentCombinedData$IntAge
    currentSex <- currentCombinedData$IntSex
    currentProp <- 1- currentCombinedData$PropLeft

    if(is.na(currentProp) == TRUE) {
      currentProp <- 0
    }

    workingSubset <- potEducationDataframe %>%
      filter(.data$IntAge == currentAge) %>%
      filter(.data$IntSex == currentSex)

    expectedCount <- round(nrow(workingSubset)*currentProp)

    if(expectedCount > 0) {

      selectedPeople <- workingSubset %>%
        slice_sample(n=expectedCount, replace = FALSE) %>%
        mutate(Status = "Y")

     if(exists("internalDFinSchool")) {

       internalDFinSchool <- bind_rows(internalDFinSchool, selectedPeople)

     } else {

       internalDFinSchool <- bind_rows(selectedPeople)

       # closes else to if(exists("internalDFKids")) {
     }


      # closes if(expectedCount > 0) {
    }

      # closes for(i in 1:nrow(CombinedData)) {
  }


  kidsAllInSchool <- PeopleDataframe %>%
    filter(between(.data$IntAge, minedage, maxedage),
           ! .data$IntAge %in% CombinedData$IntAge,
           ! .data$IntID %in% internalDFinSchool$IntID) %>%
    mutate(Status = "Y")

  kidsNotInSchoolInAges <- PeopleDataframe %>%
    filter(between(.data$IntAge, minedage, maxedage),
           .data$IntAge %in% CombinedData$IntAge,
           ! .data$IntID %in% internalDFinSchool$IntID) %>%
    mutate(Status = "N")

  internalPplNotInSchool <- PeopleDataframe %>%
    filter(!(between(.data$IntAge, minedage, maxedage))) %>%
    mutate(Status = "N")

  CompleteDF <- bind_rows(internalDFinSchool, kidsAllInSchool, kidsNotInSchoolInAges, internalPplNotInSchool) %>%
    mutate(Status = factor(.data$Status, levels = c("N", "Y"))) %>%
    rename(!!stvarname := "Status",
           !!PeopleIDColName := "IntID",
           !!PeopleSexColName := "IntSex",
           !!PeopleAgeColName := "IntAge")

  ReportedProps <- CompleteDF %>%
    filter(between(!!PeopleAgeColName, min(AgePyramid$IntAge), max(AgePyramid$IntAge))) %>%
    group_by(!!PeopleSexColName, !!PeopleAgeColName, !!StatusName)  %>%
    summarise(n = n()) %>%
    mutate(Prop_who_left = n / sum(n)) %>%
    filter(!!StatusName == "N") %>%
    select(-c(n, !!StatusName))

  cat("The proportion of adolescents who have left school are shown in the table below, by age within sex \n")

  print(ReportedProps)

  return(CompleteDF)

  #closes function
}
