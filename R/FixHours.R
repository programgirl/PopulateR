#' Reallocates the working hours of adolescents based on schooling.
#' This function reallocates working hours so that adolescents in school work fewer hours than adolescents still in school. As hours worked may be conditional on sex, the re-allocation is performed separately for each sex. If desired, the re-allocation can take age into account as well. This is the default. Under this approach, the shorter hours will be initially re-allocated to the youngest children in school, then the next-youngest and so forth.
#'The re-allocation is performed initially for the adolescents still in school. This ensures that the shorter hours worked have a higher probability of being allocated to adolescents who are very unlikely to have longer hours worked. The approach is designed to prevent longer working hours, for example full-time hours, being allocated to adolescents who are still studying.
#' The variables specifying sex can be numeric, character, or factor. Any number of values can be used, so long as they are unique.
#' @export
#' @param Adolescents A data frame containing all adolescents who have working hours.
#' @param AdolescentSxVariable The column number for the variable that contain the codes specifying females and males.
#' @param AdolescentAgeVariable The column number for the variable that contains the ages of the adolescents. This must be integer format.
#' @param AdolescentInSchool The column number containing the indicator of whether an adolescent is in school or has left school. Can be either an ordered factor or numeric. If this is a factor, factor level 1 must be in-school. If it is a numeric variable, the lowest number must be the in-school value.
#' @param HoursWorked The column number containing the hours worked by each adolescent. Must be an ordered factor or numeric. The levels/values must be ascending for hours worked.
#' @param HoursCutOff The maximum hours worked by adolescents in-school. Must be the relevant factor level/number from HoursWorked.
#' @param AgeWeight If age should be taken into account, when allocating hours. The default is to incorporate age. If yes, the default, hours worked will be allocated starting with the youngest adolescents still in school. If no, the only condition for re-allocating hours will be whether the adolescent is in school.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.


FixHours <- function(Adolescents, AdolescentSxVariable = NULL, AdolescentAgeVariable = NULL, AdolescentInSchool = NULL, HoursWorked = NULL, HoursCutOff = NULL, AgeWeight = "Y", UserSeed = NULL) {

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

  if (is.null(AdolescentInSchool)) {
    stop("The column number containing the information relating to whether an adolescent is still in school, or has left school, must be supplied.")
  }

  if (is.null(HoursWorked)) {
    stop("The column number containing the hours worked values must be supplied.")
  }

  if (is.null(HoursCutOff)) {
    stop("The maximum value for the number of hours worked, for adolescents still in school, must be supplied.")
  }

  #####################################
  #####################################
  # rename variables so don't need to use quosures inside code
  #####################################
  #####################################

  Children <- as.data.frame(Adolescents %>%
                              rename(IntSex = !! AdolescentSxVariable, IntAge = !! AdolescentAgeVariable, InSchool = !! AdolescentInSchool, IntHours = !! HoursWorked) %>%
                              mutate(IntSex = as.character(IntSex),
                                     IntAge = as.integer(IntAge),
                                     IntHours = as.ordered(IntHours)))

  # get the original variable names

  ChildrenAgeColName <- sym(names(Adolescents[AdolescentAgeVariable]))
  ChildrenSexColName <- sym(names(Adolescents[AdolescentSxVariable]))
  ChildrenSchoolColName <- sym(names(Adolescents[AdolescentInSchool]))
  ChildrenHoursColName <- sym(names(Adolescents[HoursWorked]))

  #####################################
  #####################################
  # end column names
  #####################################
  #####################################

  # check if HoursWorked is an ordered factor or numeric

  if (!(is.ordered(Children$IntHours)) == TRUE & !(is.numeric(Children$IntHours)) == TRUE) {

    stop("Hours worked must be an ordered factor or numeric.")
  }

  # #####################################
  # # check alignment of the three sex variables codes
  # #####################################
  #
  # ChildrenSexCodes <- Children %>%
  #   select(Sex) %>%
  #   distinct(Sex) %>%
  #   arrange(Sex)
  #
  # SchoolingSexCodes <- Schooling %>%
  #   select(Sex) %>%
  #   distinct(Sex) %>%
  #   arrange(Sex)
  #
  # PyramidSexCodes <- AgePyramid %>%
  #   select(Sex) %>%
  #   distinct(Sex) %>%
  #   arrange(Sex)
  #
  #
  # if (isFALSE(identical(ChildrenSexCodes, PyramidSexCodes))) {
  #
  #   stop("The sex variable values are not the same for the ", deparse(substitute(Adolescents)), " and ", deparse(substitute(Pyramid)), " data frames.", "\n")
  #
  # }
  #
  # if (isFALSE(identical(ChildrenSexCodes, SchoolingSexCodes))) {
  #
  #   stop("The sex variable values for ", deparse(substitute(Leavers)), " differ from the other two data frames.", "\n")
  #
  # }
  #
  # #####################################
  # #####################################
  # # check leaver counts are summarised
  # #####################################
  # #####################################
  #
  # DuplicateTesting <- Schooling %>%
  #   group_by(Year, Sex, Age) %>%
  #   summarise(Duplicates = n()) %>%
  #   filter(Duplicates > 1)
  #
  #
  # if (!(is.na(DuplicateTesting$Year[1])) == TRUE) {
  #
  #   stop(deparse(substitute(Leavers)), " contains duplicates.", "\n")
  # }
  #
  # #####################################
  # #####################################
  # # sum the leaver counts by current age
  # #####################################
  # #####################################
  #
  # Schooling <- Schooling %>%
  #   filter(Year <= AdolescentsYear) %>%
  #   mutate(Deduction = AdolescentsYear - Year,
  #          CurrentAge = Age + Deduction) %>%
  #   group_by(Sex, CurrentAge) %>%
  #   summarise(TotalLeaverCount = sum(NumLeftSchool))
  #
  # ####################################
  # ####################################
  # # join after the leavers are summarised
  # ####################################
  # ####################################
  # CombinedData <- Schooling %>%
  #   left_join(AgePyramid, by = c("Sex", "CurrentAge" =  "Age")) %>%
  #   mutate(PropLeft = TotalLeaverCount / PyramidCount,
  #          PropLeft = ifelse(PropLeft > 1, 1, PropLeft)) %>%
  #   filter(!(is.na(PropLeft))) %>%
  #   rename(Age = CurrentAge) %>%
  #   select(-(c(TotalLeaverCount, PyramidCount)))
  #
  #
  # # remove any NAs as these will cause problems with the maths
  # CombinedData <- CombinedData %>%
  #   filter(!(is.na(PyramidCount)))
  #
  # cat("The proportion of adolescents who have left school are shown in the table below, by sex and age.", "\n")
  #
  # print(CombinedData)
  #
  #
  # # join the probabilities to the children and calculate the probability of leaving school
  # Children <- Children %>%
  #   left_join(CombinedData, by = c("Age", "Sex")) %>%
  #   mutate(PropLeft = ifelse(is.na(PropLeft), 0, PropLeft),
  #          Status = "None")
  #
  # # seed must come before first sample is cut
  # if (!is.null(UserSeed)) {
  #   set.seed(UserSeed)
  # }
  #
  # # apply the probabilities to the children
  #
  # for (x in 1:nrow(Children)) {
  #
  #   RandomRoll <- runif(1, 0, 1)
  #
  #   #   cat("Random roll is ", RandomRoll, " for the line ", Children$PropLeft[x], "\n")
  #
  #   if (isTRUE(RandomRoll <= Children$PropLeft[x])) {
  #
  #     #   print("Entered loop")
  #
  #     Children$Status[x] <- "Left"
  #
  #   } else {
  #     Children$Status[x] <- "Stayed"
  #
  #     # closes random assignment to school statust
  #   }
  #
  #   # closes loop through children data frame
  # }
  #
  # Children <- Children %>%
  #   mutate(!!ChildrenAgeColName := Age,
  #          !!ChildrenSexColName := Sex) %>%
  #   select(-c(PropLeft, Age, Sex))
  #

  return(Children)

  #closes function
}
