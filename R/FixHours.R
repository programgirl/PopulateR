#' Reallocates the working hours of adolescents based on schooling.
#' This function reallocates working hours so that adolescents in school work fewer hours than adolescents still in school. As hours worked may be conditional on sex, the re-allocation is performed separately for each sex. If desired, the re-allocation can take age into account as well. This is the default. Under this approach, the shorter hours will be initially re-allocated to the youngest children in school, then the next-youngest and so forth.
#'The re-allocation is performed initially for the adolescents still in school. This ensures that the shorter hours worked have a higher probability of being allocated to adolescents who are very unlikely to have longer hours worked. The approach is designed to prevent longer working hours, for example full-time hours, being allocated to adolescents who are still studying.
#' The variables specifying sex can be numeric, character, or factor. Any number of values can be used, so long as they are unique.
#' @export
#' @param Adolescents A data frame containing all adolescents who have working hours.
#' @param AdolescentID The column number for the unique value that identifies unique adolescents.
#' @param SxVariable The column number for the variable that contain the codes specifying females and males.
#' @param AgeVariable The column number for the variable that contains the ages of the adolescents. This must be integer format.
#' @param InSchool The column number containing the indicator of whether an adolescent is in school or has left school. Can be either an ordered factor or numeric. If this is a factor, factor level 1 must be in-school. If it is a numeric variable, the lowest number must be the in-school value.
#' @param HoursWorked The column number containing the hours worked by each adolescent. Must be an ordered factor or numeric. The levels/values must be ascending for hours worked.
#' @param HoursCutOff The maximum hours worked by adolescents in-school. Must be the relevant factor level/number from HoursWorked.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.


FixHours <- function(Adolescents, AdolescentID = NULL, SxVariable = NULL, AgeVariable = NULL, InSchool = NULL, HoursWorked = NULL, HoursCutOff = NULL, UserSeed = NULL) {

  options(dplyr.summarise.inform=F)

  #####################################
  # quick reasonableness checks
  #####################################

  if (is.null(AdolescentID)) {
    stop("The column number containing the ID information in the Adolescents data frame must be supplied.")
  }

  if (is.null(SxVariable)) {
    stop("The column number containing the sex information in the Adolescents data frame must be supplied.")
  }

  if (is.null(AgeVariable)) {
    stop("The column number containing the age information in the Adolescents data frame must be supplied.")
  }

  if (is.null(InSchool)) {
    stop("The column number containing the information relating to whether an adolescent is still in school, or has left school, must be supplied.")
  }

  if (!(is.factor(InSchool)) & !(is.numeric(InSchool))) {
    stop("The school indicator variable must be a factor or be numeric.")
  }

  if (is.null(HoursWorked)) {
    stop("The column number containing the hours worked values must be supplied.")
  }

  if (!(is.factor(HoursWorked)) & !(is.numeric(HoursWorked))) {
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
                              rename(IntSex = !! SxVariable, IntAge = !! AgeVariable, InSchool = !! InSchool,
                                     IntHours = !! HoursWorked, IntID = !! AdolescentID) %>%
                              mutate(IntSex = as.character(IntSex),
                                     IntAge = as.integer(IntAge),
                                     IntHours = as.integer(IntHours),
                                     InSchool = as.integer(InSchool)))
                                    # IntHours = as.ordered(IntHours)))

  # get the original variable names

  ChildrenIDColName <- sym(names(Adolescents[AdolescentID]))
  ChildrenAgeColName <- sym(names(Adolescents[AgeVariable]))
  ChildrenSexColName <- sym(names(Adolescents[SxVariable]))
  ChildrenStatusColName <- sym(names(Adolescents[InSchool]))
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

  # get the unmatched lower hours
  UsedShorterHours <- FixedInSchool %>%
    group_by(IntHours) %>%
    summarise(Used = n())


  # random sort the UnusedShorterHours ahead of using head functions
  # using the -index is creating an empty data frame
  LongerHoursUnused <- LongerHoursUnused %>%
    slice_sample(n = nrow(LongerHoursUnused), replace = FALSE)
#
#
  for (x in 1:nrow(UsedShorterHours)) {

      HoursLevel <- as.numeric(UsedShorterHours[x,1])
      NumberToChange <- as.numeric(UsedShorterHours[x,2])

      # cat("The hours category is", HoursLevel, "and the count is", NumberToChange, "\n")

      # sample NumberToChange with that hours level from the incorrect InWork data frame
      SampleOfNotInSchool <- MismatchedWorking %>%
        filter(IntHours == HoursLevel) %>%
        slice_sample(n = NumberToChange, replace = FALSE) %>%
        select(-IntHours)

      # cat("The number of sampled rows is", nrow(SampleOfNotInSchool), "\n")

    # take the head of the unused longer hours
      SampledLongerHours <- LongerHoursUnused %>%
      slice_head(n = nrow(SampleOfNotInSchool))

      LongerHoursUnused <- LongerHoursUnused %>%
        slice_tail(n = (nrow(LongerHoursUnused) - nrow(SampledLongerHours)))


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

  if (is.factor(Adolescents[,InSchool]) == TRUE) {

 #   cat("School identifier is a factor")

    InSchoolLabels <- levels(Adolescents[,InSchool])

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
