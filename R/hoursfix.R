#' Reallocates the working hours between people in school and people not in school.
#' This function reallocates working hours so that adolescents in school work fewer hours than adolescents still in school.
#' The re-allocation is performed initially for the adolescents still in school. This ensures that the shorter hours worked have a higher probability of being allocated to adolescents who are very unlikely to have longer hours worked. The approach is designed to prevent longer working hours, for example full-time hours, being allocated to adolescents who are still studying.
#' The data frame must be restricted to only those whose hours can be reallocated.
#' @export
#' @param adolescents A data frame containing all adolescents who have working hours.
#' @param adlidcol The column number for the unique value that identifies unique adolescents.
#' @param statuscol The column number containing the indicator of whether an adolescent is in school or has left school. Can be either an ordered factor or numeric. If this is a factor, factor level 1 must be in-school. If it is a numeric variable, the lowest number must be the in-school value. This is output as an ordered factor.
#' @param hourscol The column number containing the hours worked by each adolescent. Must be an ordered factor or numeric. The levels/values must be ascending for hours worked. This is output as an ordered factor.
#' @param hoursmax The maximum hours worked by adolescents in-school. Must be the relevant factor level/number from HoursWorked.
#' @param grpcol The column number containing any grouping variable to be used. If this is used, the changes to the working hours will be performed using grouped data. Within-group totals for the working hours categories will be retained.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return A data of observations, with working hours reallocated so that an adolesent's working hours is compatible with schooling.
#'
#' @examples
#' AdolescentWork <- hoursfix(WorkingAdolescents, adlidcol = 3, statuscol = 6, hourscol = 5, hoursmax = 3, UserSeed = 4)


hoursfix <- function(adolescents, adlidcol = NULL, statuscol= NULL, hourscol= NULL, hoursmax = NULL, grpcol = NULL,
                     UserSeed = NULL) {

  options(dplyr.summarise.inform=F)

  #####################################
  # quick reasonableness checks
  #####################################

  if (is.null(adlidcol)) {
    stop("The column number containing the ID information in the adolescents data frame must be supplied.")
  }


  if (is.null(statuscol)) {
    stop("The column number containing the information relating to whether an adolescent is still in school, or has left school, must be supplied.")
  }

  if (!(is.factor(statuscol)) & !(is.numeric(statuscol))) {
    stop("The school indicator variable must be a factor or be numeric.")
  }

  if (is.null(hourscol)) {
    stop("The column number containing the hours worked values must be supplied.")
  }

  if (!(is.ordered(hourscol)) & !(is.numeric(hourscol))) {
    stop("Hours worked must be a factor or be numeric.")
  }

  if (is.null(hoursmax)) {
    stop("The maximum value for the number of hours worked, for adolescents still in school, must be supplied.")
  }

  # perform this by sex if the grouping variable is null

  if(is.null(grpcol) == TRUE) {

  #####################################
  #####################################
  # rename variables so don't need to use quosures inside code
  #####################################
  #####################################

  Children <- as.data.frame(adolescents %>%
                              rename(InSchool= !! statuscol,
                                     IntHours = !! hourscol, IntID = !! adlidcol) %>%
                              mutate(IntHours = as.integer(IntHours),
                                     InSchool = as.integer(InSchool)))

  childrentest <- as.data.frame(adolescents %>%
                                  rename(InSchool= !! statuscol,
                                         IntHours = !! hourscol, IntID = !! adlidcol))

  # get the original variable names

  ChildrenIDColName <- sym(names(adolescents[adlidcol]))
  ChildrenStatusColName <- sym(names(adolescents[statuscol]))
  ChildrenHoursColName <- sym(names(adolescents[hourscol]))

  #####################################
  #####################################
  # end column names
  #####################################
  #####################################

  # check if hourscol is an ordered factor or numeric

  if (is.ordered(childrentest$IntHours) == FALSE &
      is.numeric(childrentest$IntHours) == FALSE) {

    stop("Hours worked must be an ordered factor or numeric.")
  }




  #####################################
  #####################################
  # split out the correctly assigned hours for school status
  #####################################
  #####################################

  CorrectShorterHours <- Children %>%
    filter(as.integer(IntHours) <= hoursmax,
           InSchool == 1
          # as.integer(InSchool) == 1)
    )

  CorrectLongerHours <- Children %>%
    filter(as.integer(IntHours) > hoursmax,
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

  # print(table(MismatchedHours$IntHours))

  # split out the two school statuses

  MismatchedInSchool <- MismatchedHours %>%
    filter(InSchool == 1) %>%
    dplyr::select(-IntHours)

  LongerHoursUnused <- MismatchedHours %>%
    filter(IntID %in% MismatchedInSchool$IntID) %>%
    dplyr::select(IntHours)

  MismatchedWorking <- MismatchedHours %>%
    filter(InSchool == 2)

 # cat("There are", nrow(MismatchedWorking), "out of school adolescents with shorter hours", "\n")


  # just use the damn counts
  RemainingShorterHours <- MismatchedHours %>%
    filter(as.integer(IntHours) <= hoursmax) %>%
    dplyr::select(IntHours)


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

  # cat("There are ", nrow(LongerHoursUnused), "available to swap in", "\n")
#
  for (x in 1:nrow(UsedShorterHours)) {

      HoursLevel <- as.numeric(UsedShorterHours[x,1])
      NumberToChange <- as.numeric(UsedShorterHours[x,2])

       # cat("The hours category is", HoursLevel, "and the count is", NumberToChange, "\n")

      # sample NumberToChange with that hours level from the incorrect InWork data frame
      SampleOfNotInSchool <- MismatchedWorking %>%
        filter(IntHours == HoursLevel) %>%
        slice_sample(n = NumberToChange, replace = FALSE) %>%
        dplyr::select(-IntHours)

       # cat("The number of sampled rows is", nrow(SampleOfNotInSchool), "\n")

    # take the head of the unused longer hours
      SampledLongerHours <- LongerHoursUnused %>%
      slice_head(n = nrow(SampleOfNotInSchool))

      LongerHoursUnused <- LongerHoursUnused %>%
        slice_tail(n = (nrow(LongerHoursUnused) - nrow(SampledLongerHours)))

      # cat("There are ", nrow(LongerHoursUnused), "available to swap in after matching", "\n")

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

  if (is.factor(adolescents[,statuscol]) == TRUE) {

#   cat("School identifier is a factor")

   InSchoolLabels <- levels(adolescents[,statuscol])

   OutputDataFrame <- OutputDataFrame %>%
     mutate(InSchool= factor(InSchool, labels = c(InSchoolLabels), order = TRUE))

   #close factor test for school variable
 }

  if (is.ordered(adolescents[,hourscol]) == TRUE) {

  #  cat("Hours worked is a factor")

    HoursLabels <- levels(adolescents[,hourscol])

    OutputDataFrame <- OutputDataFrame %>%
      mutate(IntHours = factor(IntHours, labels = c(HoursLabels), order = TRUE))

    #close factor test for hours worked variable
  }


  OutputDataFrame <- OutputDataFrame %>%
    rename(!!ChildrenIDColName := IntID,
           !!ChildrenStatusColName := InSchool,
           !!ChildrenHoursColName := IntHours)


  return(OutputDataFrame)


  } else {

    #####################################
    #####################################
    # rename variables so don't need to use quosures inside code
    # this now includes the grouping variable
    #####################################
    #####################################

    Children <- as.data.frame(adolescents %>%
                                rename(InSchool= !! statuscol,
                                       IntHours = !! hourscol, IntID = !! adlidcol,
                                       TheGroups = !! grpcol) %>%
                                mutate(IntHours = as.integer(IntHours),
                                       InSchool = as.integer(InSchool)))

    childrentest <- as.data.frame(adolescents %>%
                                    rename(InSchool= !! statuscol,
                                           IntHours = !! hourscol, IntID = !! adlidcol,
                                           TheGroups = !! grpcol))

    # get the original variable names

    ChildrenIDColName <- sym(names(adolescents[adlidcol]))
    ChildrenStatusColName <- sym(names(adolescents[statuscol]))
    ChildrenHoursColName <- sym(names(adolescents[hourscol]))
    ChildrenGroupsColName <- sym(names(adolescents[grpcol]))

    #####################################
    #####################################
    # end column names
    #####################################
    #####################################

    # check if hourscol is an ordered factor or numeric

    if (is.ordered(childrentest$IntHours) == FALSE &
        is.numeric(childrentest$IntHours) == FALSE) {

      stop("Hours worked must be an ordered factor or numeric.")
    }


    # the below code has to loop for each unique grouping value

    GroupSum <- Children %>%
      group_by(TheGroups) %>%
      summarise()

    OriginalChildren <- Children

    # now just go through the groups

    for (g in 1:nrow(GroupSum)) {

      # print(g)
      Children <- OriginalChildren %>%
        filter(TheGroups == as.character(GroupSum[g,1]))

    #####################################
    #####################################
    # split out the correctly assigned hours for school status
    #####################################
    #####################################

    CorrectShorterHours <- Children %>%
      filter(as.integer(IntHours) <= hoursmax,
             InSchool == 1
             # as.integer(InSchool) == 1)
      )

    CorrectLongerHours <- Children %>%
      filter(as.integer(IntHours) > hoursmax,
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

    # print(table(MismatchedHours$IntHours))

    # split out the two school statuses

    MismatchedInSchool <- MismatchedHours %>%
      filter(InSchool == 1) %>%
      dplyr::select(-IntHours)

    LongerHoursUnused <- MismatchedHours %>%
      filter(IntID %in% MismatchedInSchool$IntID) %>%
      dplyr::select(IntHours)

    MismatchedWorking <- MismatchedHours %>%
      filter(InSchool == 2)

    # cat("There are", nrow(MismatchedWorking), "out of school adolescents with shorter hours", "\n")


    # just use the damn counts
    RemainingShorterHours <- MismatchedHours %>%
      filter(as.integer(IntHours) <= hoursmax) %>%
      dplyr::select(IntHours)


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

    # cat("There are ", nrow(LongerHoursUnused), "available to swap in", "\n")
    #
    for (x in 1:nrow(UsedShorterHours)) {

      HoursLevel <- as.numeric(UsedShorterHours[x,1])
      NumberToChange <- as.numeric(UsedShorterHours[x,2])

      # cat("The hours category is", HoursLevel, "and the count is", NumberToChange, "\n")

      # sample NumberToChange with that hours level from the incorrect InWork data frame
      SampleOfNotInSchool <- MismatchedWorking %>%
        filter(IntHours == HoursLevel) %>%
        slice_sample(n = NumberToChange, replace = FALSE) %>%
        dplyr::select(-IntHours)

      # cat("The number of sampled rows is", nrow(SampleOfNotInSchool), "\n")

      # take the head of the unused longer hours
      SampledLongerHours <- LongerHoursUnused %>%
        slice_head(n = nrow(SampleOfNotInSchool))

      LongerHoursUnused <- LongerHoursUnused %>%
        slice_tail(n = (nrow(LongerHoursUnused) - nrow(SampledLongerHours)))

      # cat("There are ", nrow(LongerHoursUnused), "available to swap in after matching", "\n")

      FixedInWork <- bind_cols(SampleOfNotInSchool,SampledLongerHours)

      # cat("Length of FixedInWork is", nrow(FixedInWork), "\n")

      if(exists("WorkFixed") == TRUE) {

        #       cat("Enters this loop with", nrow(FixedInWork), "rows in the created data frame", "\n")

        WorkFixed <- bind_rows(WorkFixed, FixedInWork)

        # cat("After being constructed earlier, Work fixed is", nrow(WorkFixed), "\n")

      } else {

        WorkFixed <- FixedInWork


        # cat("Work fixed is", nrow(WorkFixed), "\n")

        # closes if loop for constructing adolescents with shorter hours
      }

      MismatchedWorking <- MismatchedWorking %>%
        filter(!(IntID %in% SampleOfNotInSchool$IntID))

      #    cat("MismatchedWorking contains", nrow(MismatchedWorking), "at this point", "\n")

      # closes loop through the hours
    }

    # cat("Correct hours is", nrow(CorrectHours), "Fixed in school is", nrow(FixedInSchool), "work fixed is", nrow(WorkFixed),
    #     "Mismatched working is", nrow(MismatchedWorking), "\n")

    if(exists("OutputDataFrame")) {

      # de-dup WorkFixed from those already in DF
      OutputDataFrame <- OutputDataFrame %>%
        filter(!(IntID %in% c(WorkFixed$IntID)))

      OutputDataFrame <- bind_rows(OutputDataFrame, CorrectHours, FixedInSchool, MismatchedWorking)

    } else {

      OutputDataFrame <- bind_rows(CorrectHours, FixedInSchool, MismatchedWorking)
    }

    # closes for (g in 1:nrow(GroupSum))
    }

    OutputDataFrame <- bind_rows(OutputDataFrame, WorkFixed)


    if (is.factor(adolescents[,statuscol]) == TRUE) {

      #   cat("School identifier is a factor")

      InSchoolLabels <- levels(adolescents[,statuscol])

      OutputDataFrame <- OutputDataFrame %>%
        mutate(InSchool= factor(InSchool, labels = c(InSchoolLabels), order = TRUE))

      #close factor test for school variable
    }

    if (is.ordered(adolescents[,hourscol]) == TRUE) {

      #  cat("Hours worked is a factor")

      HoursLabels <- levels(adolescents[,hourscol])

      OutputDataFrame <- OutputDataFrame %>%
        mutate(IntHours = factor(IntHours, labels = c(HoursLabels), order = TRUE))

      #close factor test for hours worked variable
    }


    OutputDataFrame <- OutputDataFrame %>%
      rename(!!ChildrenIDColName := IntID,
             !!ChildrenStatusColName := InSchool,
             !!ChildrenHoursColName := IntHours,
             !!ChildrenGroupsColName := TheGroups)


    return(OutputDataFrame)

    # closes if(is.null(grpcol) == TRUE)
  }

  #closes function
}
