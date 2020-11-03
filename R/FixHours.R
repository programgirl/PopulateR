#' Reallocates the working hours of adolescents based on schooling.
#' This function reallocates working hours so that adolescents in school work fewer hours than adolescents still in school. As hours worked may be conditional on sex, the re-allocation is performed separately for each sex. If desired, the re-allocation can take age into account as well. This is the default. Under this approach, the shorter hours will be initially re-allocated to the youngest children in school, then the next-youngest and so forth.
#'The re-allocation is performed initially for the adolescents still in school. This ensures that the shorter hours worked have a higher probability of being allocated to adolescents who are very unlikely to have longer hours worked. The approach is designed to prevent longer working hours, for example full-time hours, being allocated to adolescents who are still studying.
#' The variables specifying sex can be numeric, character, or factor. Any number of values can be used, so long as they are unique.
#' @export
#' @param Adolescents A data frame containing all adolescents who have working hours.
#' @param AdolescentID The column number for the unique value that identifies unique adolescents.
#' @param AdolescentSxVariable The column number for the variable that contain the codes specifying females and males.
#' @param AdolescentAgeVariable The column number for the variable that contains the ages of the adolescents. This must be integer format.
#' @param AdolescentInSchool The column number containing the indicator of whether an adolescent is in school or has left school. Can be either an ordered factor or numeric. If this is a factor, factor level 1 must be in-school. If it is a numeric variable, the lowest number must be the in-school value.
#' @param HoursWorked The column number containing the hours worked by each adolescent. Must be an ordered factor or numeric. The levels/values must be ascending for hours worked.
#' @param HoursCutOff The maximum hours worked by adolescents in-school. Must be the relevant factor level/number from HoursWorked.
#' @param AgeWeight If age should be taken into account, when allocating hours. The default is to incorporate age. If yes, the default, hours worked will be allocated starting with the youngest adolescents still in school. If no, the only condition for re-allocating hours will be whether the adolescent is in school.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.


FixHours <- function(Adolescents, AdolescentID = NULL, AdolescentSxVariable = NULL, AdolescentAgeVariable = NULL, AdolescentInSchool = NULL, HoursWorked = NULL, HoursCutOff = NULL, AgeWeight = "Y", UserSeed = NULL) {

  options(dplyr.summarise.inform=F)

  #####################################
  # quick reasonableness checks
  #####################################

  if (is.null(AdolescentID)) {
    stop("The column number containing the ID information in the Adolescents data frame must be supplied.")
  }

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
                              rename(IntSex = !! AdolescentSxVariable, IntAge = !! AdolescentAgeVariable, InSchool = !! AdolescentInSchool,
                                     IntHours = !! HoursWorked, IntID = !! AdolescentID) %>%
                              mutate(IntSex = as.character(IntSex),
                                     IntAge = as.integer(IntAge),
                                     IntHours = as.ordered(IntHours)))

  # get the original variable names

  ChildrenIDColName <- sym(names(Adolescents[AdolescentID]))
  ChildrenAgeColName <- sym(names(Adolescents[AdolescentAgeVariable]))
  ChildrenSexColName <- sym(names(Adolescents[AdolescentSxVariable]))
  ChildrenStatusColName <- sym(names(Adolescents[AdolescentInSchool]))
  ChildrenHoursColName <- sym(names(Adolescents[HoursWorked]))

  # extract the column names from Children in order to match-merge later
  # ignore any variables without duplicate values
  MergingCols <- Children %>%
    select(where(~any(duplicated(.))),
           -c(InSchool,IntHours)) %>%
    colnames()

  #####################################
  #####################################
  # end column names
  #####################################
  #####################################

  # check if HoursWorked is an ordered factor or numeric

  if (!(is.ordered(Children$IntHours)) == TRUE & !(is.numeric(Children$IntHours)) == TRUE) {

    stop("Hours worked must be an ordered factor or numeric.")
  }


  #####################################
  #####################################
  # split out the correctly assigned hours for school status
  #####################################
  #####################################

  CorrectShorterHours <- Children %>%
    filter(as.integer(IntHours) <= HoursCutOff,
           as.integer(InSchool) == 1)

  CorrectLongerHours <- Children %>%
    filter(as.integer(IntHours) > HoursCutOff,
           as.integer(InSchool) == 2)

# merge the two correct data frames
  CorrectHours <- bind_rows(CorrectShorterHours, CorrectLongerHours)


  #####################################
  #####################################
  # Work on the mismatches
  #####################################
  #####################################

  MismatchedHours <- Children %>%
    filter(!(IntID %in% CorrectHours$IntID))

  # split out the two school statuses

  MismatchedInSchool <- MismatchedHours %>%
    filter(as.integer(InSchool) == 1)

  MismatchedWorking <- MismatchedHours %>%
    filter(as.integer(InSchool) == 2)

  # seed must come before first sample is cut
  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }


  # identify which one is smaller and use that one
  # use an if

  if (nrow(MismatchedInSchool) > nrow(MismatchedWorking)) {





  } else {

    cat("The mismatched working data frame is larger", "\n")

   for (x in 1:nrow(MismatchedInSchool)) {
    # for (x in 1:3) {

      AdolescentToMatch <- MismatchedInSchool[x,]

      MatchingOptions <- AdolescentToMatch %>%
        left_join(MismatchedWorking, by = c(MergingCols))

      if (!(is.na(MatchingOptions$IntID.y[1]))) {

        # cat("There was a match for", AdolescentToMatch$IntID)

        MatchIDs <- MatchingOptions %>%
          select(IntID.y)

        MatchedIDChosen <- MatchIDs %>%
          slice_sample(n = 1)

        MatchedPerson <- MismatchedWorking %>%
          filter(IntID == MatchedIDChosen$IntID)

        # swap the hours

        LargerHours <- AdolescentToMatch$IntHours
        SmallerHours <- MatchedPerson$IntHours

        AdolescentToMatch$IntHours <- SmallerHours
        MatchedPerson$IntHours <- LargerHours

        CorrectHours <- bind_rows(CorrectHours, AdolescentToMatch, MatchedPerson)

        MismatchedWorking <- MismatchedWorking %>%
          filter(!(IntID == MatchedPerson$IntID))

        # closes loop for dealing with a match
      }

      #close the loop through the Mismatched school data frame
    }


    CorrectHours <- bind_rows(CorrectHours, MismatchedWorking)
    # closes swapping between the two mismatched data frames
  }




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

  return(CorrectHours)

  #closes function
}
