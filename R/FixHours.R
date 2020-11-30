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
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.


FixHours <- function(Adolescents, AdolescentID = NULL, AdolescentSxVariable = NULL, AdolescentAgeVariable = NULL, AdolescentInSchool = NULL, HoursWorked = NULL, HoursCutOff = NULL, UserSeed = NULL) {

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
                                     IntHours = as.integer(IntHours),
                                     InSchool = as.integer(InSchool)))
                                    # IntHours = as.ordered(IntHours)))

  # get the original variable names

  ChildrenIDColName <- sym(names(Adolescents[AdolescentID]))
  ChildrenAgeColName <- sym(names(Adolescents[AdolescentAgeVariable]))
  ChildrenSexColName <- sym(names(Adolescents[AdolescentSxVariable]))
  ChildrenStatusColName <- sym(names(Adolescents[AdolescentInSchool]))
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


  #####################################
  #####################################
  # split out the correctly assigned hours for school status
  #####################################
  #####################################

  CorrectShorterHours <- Children %>%
    filter(as.integer(IntHours) <= HoursCutOff,
           InSchool == 1
          # as.integer(InSchool) == 1)
    )

  CorrectLongerHours <- Children %>%
    filter(as.integer(IntHours) > HoursCutOff,
           InSchool == 2
          # as.integer(InSchool) == 2)
    )

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
    filter(InSchool == 1)

  MismatchedWorking <- MismatchedHours %>%
    filter(InSchool == 2)

  # seed must come before first sample is cut
  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  if (nrow(MismatchedInSchool) > nrow(MismatchedWorking)) {

  for (x in 1:nrow(MismatchedWorking)) {

        AdolescentToMatch <- MismatchedWorking[x,]

        MatchingOptions <- AdolescentToMatch %>%
       #   left_join(Donor, by = c(MergingCols))
          left_join(MismatchedInSchool, by = c("IntAge", "IntSex"))


        if (!(is.na(MatchingOptions$IntID.y[1]))) {

          # cat("There was a match for", AdolescentToMatch$IntID)

          MatchIDs <- MatchingOptions %>%
            select(IntID.y)

          MatchedIDChosen <- MatchIDs %>%
            slice_sample(n = 1)

          MatchedPerson <- MismatchedInSchool %>%
            filter(IntID == MatchedIDChosen$IntID)

          # swap the school status

          LargerHours <- AdolescentToMatch$InSchool
          SmallerHours <- MatchedPerson$InSchool

          AdolescentToMatch$InSchool <- SmallerHours
          MatchedPerson$InSchool <- LargerHours

          cat("Base is", AdolescentToMatch$IntID, "match is", MatchedPerson$IntID, "larger hours are", AdolescentToMatch$IntHours, "shorter hours are", MatchedPerson$IntHours, "\n")

          CorrectHours <- bind_rows(CorrectHours, AdolescentToMatch, MatchedPerson)

          MismatchedInSchool <- MismatchedInSchool %>%
            filter(!(IntID %in% CorrectHours$IntID))

          # closes loop for dealing with a match
        }


        #close the loop through the smaller data frame
  }
    NonMatchedChildren <- MismatchedWorking %>%
      filter(!(IntID %in% CorrectHours$IntID))

    OutputDataFrame <- bind_rows(CorrectHours, MismatchedInSchool, NonMatchedChildren)


  } else {

    for (x in 1:nrow(MismatchedInSchool)) {


      AdolescentToMatch <- MismatchedInSchool[x,]

      MatchingOptions <- AdolescentToMatch %>%
        left_join(MismatchedWorking, by = c("IntAge", "IntSex"))


      if (!(is.na(MatchingOptions$IntID.y[1]))) {

        # cat("There was a match for", AdolescentToMatch$IntID)

        MatchIDs <- MatchingOptions %>%
          select(IntID.y)

        MatchedIDChosen <- MatchIDs %>%
          slice_sample(n = 1)

        MatchedPerson <- MismatchedWorking %>%
          filter(IntID == MatchedIDChosen$IntID)

        # swap the school status

        LargerHours <- AdolescentToMatch$InSchool
        SmallerHours <- MatchedPerson$InSchool

        AdolescentToMatch$InSchool <- SmallerHours
        MatchedPerson$InSchool <- LargerHours


   #     cat("Base is", AdolescentToMatch$IntID, "match is", MatchedPerson$IntID, "larger hours are", AdolescentToMatch$IntHours, "shorter hours are", MatchedPerson$IntHours, "\n")

        CorrectHours <- bind_rows(CorrectHours, AdolescentToMatch, MatchedPerson)

        MismatchedWorking <- MismatchedWorking %>%
          filter(!(IntID %in% CorrectHours$IntID))

        # closes loop for dealing with a match
      }


      #close the loop through the smaller data frame
    }

    NonMatchedChildren <- MismatchedInSchool %>%
      filter(!(IntID %in% CorrectHours$IntID))

    # closes loop for when the mismatched in work is larger

    OutputDataFrame <- bind_rows(CorrectHours, MismatchedWorking, NonMatchedChildren, )
  }


  if (is.factor(Adolescents[,AdolescentInSchool]) == TRUE) {

 #   cat("School identifier is a factor")

    InSchoolLabels <- levels(Adolescents[,AdolescentInSchool])

    OutputDataFrame <- OutputDataFrame %>%
      mutate(InSchool = factor(InSchool, labels = c(InSchoolLabels), order = TRUE))

    #close factor test for school variable
  }

  if (is.factor(Adolescents[,HoursWorked]) == TRUE) {

  #  cat("Hours worked is a factor")

    HoursLabels <- levels(Adolescents[,HoursWorked])

    OutputDataFrame <- OutputDataFrame %>%
      mutate(IntHours = factor(IntHours, labels = c(HoursLabels), order = TRUE))

    #close factor test for hours worked variable
  }


  OutputDataFrame <- OutputDataFrame %>%
    rename(!!ChildrenAgeColName := IntAge,
           !!ChildrenIDColName := IntID,
           !!ChildrenSexColName := IntSex,
           !!ChildrenStatusColName := InSchool,
           !!ChildrenHoursColName := IntHours)


  return(OutputDataFrame)

  #closes function
}
