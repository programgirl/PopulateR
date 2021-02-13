#' Reallocates the working hours of adolescents based on schooling.
#' This function reallocates working hours so that adolescents in school work fewer hours than adolescents still in school. As hours worked may be conditional on sex, the re-allocation is performed separately for each sex. If desired, the re-allocation can take age into account as well. This is the default. Under this approach, the shorter hours will be initially re-allocated to the youngest children in school, then the next-youngest and so forth.
#'The re-allocation is performed initially for the adolescents still in school. This ensures that the shorter hours worked have a higher probability of being allocated to adolescents who are very unlikely to have longer hours worked. The approach is designed to prevent longer working hours, for example full-time hours, being allocated to adolescents who are still studying.
#' The variables specifying sex can be numeric, character, or factor. Any number of values can be used, so long as they are unique.
#' @export
#' @param Adolescents A data frame containing all adolescents who have working hours.
#' @param AdolescentIDCol The column number for the unique value that identifies unique adolescents.
#' @param AdolescentSxCol The column number for the variable that contain the codes specifying females and males.
#' @param AdolescentAgeCol The column number for the variable that contains the ages of the adolescents. This must be integer format.
#' @param InSchoolCol The column number containing the indicator of whether an adolescent is in school or has left school. Can be either an ordered factor or numeric. If this is a factor, factor level 1 must be in-school. If it is a numeric variable, the lowest number must be the in-school value. This is output as an ordered factor.
#' @param HoursWorkedCol The column number containing the hours worked by each adolescent. Must be an ordered factor or numeric. The levels/values must be ascending for hours worked. This is output as an ordered factor.
#' @param HoursCutOff The maximum hours worked by adolescents in-school. Must be the relevant factor level/number from HoursWorked.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.


FixHours <- function(Adolescents, AdolescentIDCol = NULL, AdolescentSxCol = NULL,
                     AdolescentAgeCol = NULL, InSchoolCol= NULL, HoursWorkedCol= NULL,
                     HoursCutOff = NULL, UserSeed = NULL) {

  options(dplyr.summarise.inform=F)

  #####################################
  # quick reasonableness checks
  #####################################

  if (is.null(AdolescentIDCol)) {
    stop("The column number containing the ID information in the Adolescents data frame must be supplied.")
  }

  if (is.null(AdolescentSxCol)) {
    stop("The column number containing the sex information in the Adolescents data frame must be supplied.")
  }

  if (is.null(AdolescentAgeCol)) {
    stop("The column number containing the age information in the Adolescents data frame must be supplied.")
  }

  if (is.null(InSchoolCol)) {
    stop("The column number containing the information relating to whether an adolescent is still in school, or has left school, must be supplied.")
  }

  if (!(is.factor(InSchoolCol)) & !(is.numeric(InSchoolCol))) {
    stop("The school indicator variable must be a factor or be numeric.")
  }

  if (is.null(HoursWorkedCol)) {
    stop("The column number containing the hours worked values must be supplied.")
  }

  if (!(is.ordered(HoursWorkedCol)) & !(is.numeric(HoursWorkedCol))) {
    stop("Hours worked must be a factor or be numeric.")
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
                              rename(IntSex = !! AdolescentSxCol, IntAge = !! AdolescentAgeCol,
                                     InSchool= !! InSchoolCol,
                                     IntHours = !! HoursWorkedCol, IntID = !! AdolescentIDCol) %>%
                              mutate(IntSex = as.character(IntSex),
                                     IntAge = as.integer(IntAge),
                                     IntHours = as.integer(IntHours),
                                     InSchool = as.integer(InSchool)))
                                    # IntHours = as.ordered(IntHours)))

  # get the original variable names

  ChildrenIDColName <- sym(names(Adolescents[AdolescentIDCol]))
  ChildrenAgeColName <- sym(names(Adolescents[AdolescentAgeCol]))
  ChildrenSexColName <- sym(names(Adolescents[AdolescentSxCol]))
  ChildrenStatusColName <- sym(names(Adolescents[InSchoolCol]))
  ChildrenHoursColName <- sym(names(Adolescents[HoursWorkedCol]))

  #####################################
  #####################################
  # end column names
  #####################################
  #####################################

  # check if HoursWorkedColis an ordered factor or numeric

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
    filter(InSchool == 1) %>%
    select(-IntHours)

  LongerHoursUnused <- MismatchedHours %>%
    filter(IntID %in% MismatchedInSchool$IntID) %>%
    select(IntHours)

  MismatchedWorking <- MismatchedHours %>%
    filter(InSchool == 2)

#  cat("There are", nrow(MismatchedWorking), "out of school adolescents with shorter hours", "\n")


  # just use the damn counts
  RemainingShorterHours <- MismatchedHours %>%
    filter(as.integer(IntHours) <= HoursCutOff) %>%
    select(IntHours)


  if((nrow(RemainingShorterHours) < nrow(MismatchedInSchool)) == TRUE) {

    stop("There are not enough shorter hours to assign to adolescents in school.")
  }

  # seed must come before first sample is cut
  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  # assign the shorter hours to the children in school
  NewShortHours <- RemainingShorterHours %>%
    slice_sample(n = nrow(MismatchedInSchool), replace = FALSE)

  FixedInSchool <- bind_cols(MismatchedInSchool, NewShortHours)

  # get the matched lower hours
  UsedShorterHours <- FixedInSchool %>%
    group_by(IntHours) %>%
    summarise(Used = n())

  # take the longer hours from the  participants, these
  # need to swap with the left-over shorter hours from adjusting the in-school adolescent hours
  # using the -index is creating an empty data frame
  LongerHoursUnused <- LongerHoursUnused %>%
    slice_sample(n = nrow(LongerHoursUnused), replace = FALSE)

  cat("There are ", nrow(LongerHoursUnused), "available to swap in", "\n")
#
  for (x in 1:nrow(UsedShorterHours)) {

      HoursLevel <- as.numeric(UsedShorterHours[x,1])
      NumberToChange <- as.numeric(UsedShorterHours[x,2])

       cat("The hours category is", HoursLevel, "and the count is", NumberToChange, "\n")

      # sample NumberToChange with that hours level from the incorrect InWork data frame
      SampleOfNotInSchool <- MismatchedWorking %>%
        filter(IntHours == HoursLevel) %>%
        slice_sample(n = NumberToChange, replace = FALSE) %>%
        select(-IntHours)

       cat("The number of sampled rows is", nrow(SampleOfNotInSchool), "\n")

    # take the head of the unused longer hours
      SampledLongerHours <- LongerHoursUnused %>%
      slice_head(n = nrow(SampleOfNotInSchool))

      LongerHoursUnused <- LongerHoursUnused %>%
        slice_tail(n = (nrow(LongerHoursUnused) - nrow(SampledLongerHours)))

      cat("There are ", nrow(LongerHoursUnused), "available to swap in after matching", "\n")

    FixedInWork <- bind_cols(SampleOfNotInSchool,SampledLongerHours)

      if(exists("WorkFixed") == TRUE) {

 #       cat("Enters this loop with", nrow(FixedInWork), "rows in the created data frame", "\n")

        WorkFixed <- bind_rows(WorkFixed, FixedInWork)

      } else {

        WorkFixed <- FixedInWork

        # closes if loop for constructing adolescents with shorter hours
      }

      MismatchedWorking <- MismatchedWorking %>%
        filter(!(IntID %in% SampleOfNotInSchool$IntID))

  #    cat("MismatchedWorking contains", nrow(MismatchedWorking), "at this point", "\n")

      # closes loop through the hours
  }

  OutputDataFrame <- bind_rows(CorrectHours, FixedInSchool, WorkFixed, MismatchedWorking)

  if (is.factor(Adolescents[,InSchoolCol]) == TRUE) {

#   cat("School identifier is a factor")

   InSchoolLabels <- levels(Adolescents[,InSchoolCol])

   OutputDataFrame <- OutputDataFrame %>%
     mutate(InSchool= factor(InSchool, labels = c(InSchoolLabels), order = TRUE))

   #close factor test for school variable
 }

  if (is.ordered(Adolescents[,HoursWorkedCol]) == TRUE) {

  #  cat("Hours worked is a factor")

    HoursLabels <- levels(Adolescents[,HoursWorkedCol])

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
