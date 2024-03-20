#' Reallocates the working hours between people in school and people not in school.
#' This function reallocates working hours so that people in school work fewer hours than people not in school.
#' The re-allocation is performed initially for the adolescents still in school. This ensures that the shorter hours worked have a higher probability of being allocated to adolescents who are very unlikely to have longer hours worked. The approach is designed to prevent longer working hours, for example full-time hours, being allocated to adolescents who are still studying.
#' The data frame must be restricted to only those whose hours can be reallocated.
#' @export
#' @param people A data frame containing all adolescents who have working hours.
#' @param pplid The variable containing the unique value that identifies unique adolescents.
#' @param pplstat The variable containing the indicator of whether an adolescent is in school or has left school. Can be either an ordered factor or numeric. If this is a factor, factor level 2 must be in-school. If it is a numeric variable, the lowest number must be the in-school value. This is output as an ordered factor.
#' @param pplhours The variable containing the hours worked by each adolescent. Must be a factor or numeric. If this is a factor, it is assumed to be ordered. The levels/values must be ascending for hours worked. This is output as an ordered factor.
#' @param hoursmax The maximum hours worked by adolescents in-school. Must be the relevant factor level/number from HoursWorked.
#' @param grpdef The vector containing any grouping variable to be used. If this is used, the changes to the working hours will be performed using grouped data. Within-group totals for the working hours groups will be retained.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return A data of observations, with working hours reallocated so that people's working hours are compatible with schooling
#'
#' @examples
#' # no grouping variable
#' AdolescentWork <- hoursfix(WorkingAdolescents, adlidcol = 3, statuscol = 6, hourscol = 5, hoursmax = 3, userseed = 4)
#'
#' # grouping variable
#' # when a group is used
#' AdolescentWork2 <- hoursfix(WorkingAdolescents, adlidcol = 3, statuscol = 6, hourscol = 5,
#'                             hoursmax = 3, grpcol = 1, userseed = 4)


hoursfix <- function(people, pplid, pplstat, pplhours, hoursmax, grpdef, userseed = NULL) {
  
  options(dplyr.summarise.inform=F)
  
  #####################################
  # check for missing input information
  #####################################
  
  
  if (!pplid %in% names(people)) {
    stop("The ID variable in the people data frame does not exist.")
  }
  
  if (!pplstat %in% names(people)) {
    stop("The school status variable in the people data frame does not exist.")
  }
 
  if (!(all((grpdef) %in% names(people)))) {
    stop("All names in grpdef must exist in the people data frame.")
  }
  
  # check if pplhours is an ordered factor or numeric

  if (is.ordered(people[[pplhours]]) == FALSE &
      is.numeric(people[[pplhours]]) == FALSE) {

    stop("Hours worked must be an ordered factor or numeric.")
  }

  
  # get column names as symbols to use inside data frame subfunctions

  IDColName <- sym(names(people[pplid]))
  StatusColName <- sym(names(people[pplstat]))
  HoursColName <- sym(names(people[pplhours]))
  

  # need to do the name change here
  
  peopleRenamed <- people %>%
    rename(InSchool= !! pplstat,
           IntHours = !! pplhours,
           IntID = !! pplid) %>%
    mutate(IntHours = as.integer(IntHours),
           InSchool = as.integer(InSchool))
  
  # get the min and max values for the InSchool variable
  
  minInSchool = min(peopleRenamed$InSchool)
  maxInSchool = max(peopleRenamed$InSchool)
  
  cat("The minimum in school value is", minInSchool, "and the maximum in school value is", maxInSchool, "\n")

    #####################################
    #####################################
    
    
    # get the unique set of grouping factors
    PeopleUnique <- as.data.frame(people[,grpdef] %>%
                                    unique())
    

  if (!is.null(userseed)) {
    set.seed(userseed)
  }
  
  
  # loop through the unique rows
  for(i in 1:nrow(PeopleUnique)) {
    
    # delete previous versions of working data frame
    if(exists("WorkingDF")) {
      rm(WorkingDF)
    }
    
    CurrentDef = PeopleUnique[i, , drop=FALSE]
    
    suppressMessages(CurrentGroup <- left_join(CurrentDef, peopleRenamed, by = c(grpdef)))

    # get the number in each status in the variable of interest
    NumInEachStatus <- CurrentGroup %>%
      group_by(InSchool) %>%
      summarise(NumPerLevel = n())

    #need to skip the bit below if there is ONLY the "not in school" status
    if(nrow(NumInEachStatus) == 1) {
      
      # cat("Group with only one status is", "\n")
      # print(CurrentDef)
      
      NumToFix <- 0
      
    } else {
      
      # cat("Group is", "\n")
      # print(CurrentDef)
      
      # NEED TO FIX FROM THIS BIT
      CurrentGroup <- left_join(CurrentDef, peopleRenamed, by = c(grpdef))
      
        HoursTooHigh <- CurrentGroup %>%
          filter(IntHours > hoursmax & InSchool == maxInSchool)

        HoursCanSub <- CurrentGroup %>%
          filter(IntHours <= hoursmax & InSchool == minInSchool)
        
        # cat("HoursTooHigh has", nrow(HoursTooHigh), "and HoursCanSub has", nrow(HoursCanSub), "rows", "\n")
        

        if(nrow(HoursTooHigh) > 0 & nrow(HoursCanSub) > 0) {
        
          if(nrow(HoursTooHigh) > nrow(HoursCanSub)) {

            HoursTooHigh <- HoursTooHigh %>%
              slice_sample(n = nrow(HoursCanSub))
          }

          if(nrow(HoursCanSub) > nrow(HoursTooHigh)) {

            HoursCanSub <- HoursCanSub %>%
              slice_sample(n = nrow(HoursTooHigh))
          }

          # length of both DFs is now the same
        
          # swap the school status

          # literally swap the hours worked in HoursTooHigh and HoursCanSub by row

          HoursTooHighStatus <- HoursTooHigh %>%
            select(IntHours)

          HoursCanSubFixed <- HoursCanSub %>%
            select(-IntHours) %>%
            bind_cols(HoursTooHighStatus)

          HoursCanSubStatus <- HoursCanSub %>%
            select(IntHours)

          HoursTooHighFixed <- HoursTooHigh %>%
            select(-IntHours) %>%
            bind_cols(HoursCanSubStatus)

          Fixed <- bind_rows(HoursCanSubFixed, HoursTooHighFixed)
          
            # get the people in the group that weren't amended

            UnAmended <- CurrentGroup %>%
              filter(!(IntID %in% c(Fixed$IntID)))

              # this needs to be a file that takes all the groups

              if(exists("OutputDataFrame")) {

                OutputDataFrame <- bind_rows(OutputDataFrame, UnAmended, Fixed)
              } else {

                OutputDataFrame <- bind_rows(UnAmended, Fixed)

                # closes  if(exists("OutputDataFrame"))
              }
            
            # closes if(nrow(HoursTooHigh) > 0 & nrow(HoursCanSub) > 0)
            
        } else {
          
          if(exists("OutputDataFrame")) {
            
            OutputDataFrame <- bind_rows(OutputDataFrame, CurrentGroup)
          } else {
            
            OutputDataFrame <- CurrentGroup
           
            # closes else to  if(exists("OutputDataFrame"))
          }
   
          # closes else to if(nrow(HoursTooHigh) > 0 & nrow(HoursCanSub) > 0)
        }
      # closes else to  if(nrow(NumInEachStatus) == 1)
    }
    
    # closes else to if(nrow(NumInEachStatus) == 1)
  }
  
  # add in people who are omitted from the fix cycle

  missingPeople <- peopleRenamed %>%
    filter(!IntID %in% c(OutputDataFrame$IntID))

  missingPeopleRowCount <- nrow(missingPeople)
  
  if(missingPeopleRowCount > 0) {

    OutputDataFrame <- bind_rows(OutputDataFrame, missingPeople)
  }
  

  # fix output data frame factors
  # 
  if (is.factor(people[[pplstat]]) == TRUE) {

    #   cat("School identifier is a factor")

    InSchoolLabels <- levels(people[[pplstat]])

    OutputDataFrame <- OutputDataFrame %>%
      mutate(InSchool= factor(InSchool, labels = c(InSchoolLabels), order = TRUE))

    #close factor test for school variable
  }

  if (is.factor(people[[pplhours]]) == TRUE) {

    cat("Hours worked is a factor", "\n")

    HoursLabels <- levels(people[[pplhours]])

    OutputDataFrame <- OutputDataFrame %>%
      mutate(IntHours = factor(IntHours,
                               levels = c(1:length(HoursLabels)),
                               labels = c(HoursLabels), order = TRUE))

    #close factor test for hours worked variable
  }


  OutputDataFrame <- OutputDataFrame %>%
    rename(!!quo_name(pplstat) := InSchool,
           !!quo_name(pplid) := IntID,
           !!quo_name(pplhours) := IntHours)


  return(OutputDataFrame)
  
  
  
  
    
    # #####################################
    # #####################################
    # # rename variables so don't need to use quosures inside code
    # # this now includes the grouping variable
    # #####################################
    # #####################################
    # 
    # People <- as.data.frame(people %>%
    #                         rename(InSchool= !! statuscol,
    #                                    IntHours = !! hourscol,
    #                                    IntID = !! adlidcol) %>%
    #                             mutate(IntHours = as.integer(IntHours),
    #                                    InSchool = as.integer(InSchool)))
    # 
    # childrentest <- as.data.frame(people %>%
    #                                 rename(InSchool= !! statuscol,
    #                                        IntHours = !! hourscol, IntID = !! adlidcol))
    # 
    # # get the original variable names
    # 
    # ChildrenIDColName <- sym(names(people[adlidcol]))
    # ChildrenStatusColName <- sym(names(people[statuscol]))
    # ChildrenHoursColName <- sym(names(people[hourscol]))
    # 
    # #####################################
    # #####################################
    # # end column names
    # #####################################
    # #####################################
    # 
    # # check if hourscol is an ordered factor or numeric
    # 
    # if (is.ordered(childrentest$IntHours) == FALSE &
    #     is.numeric(childrentest$IntHours) == FALSE) {
    #   
    #   stop("Hours worked must be an ordered factor or numeric.")
    # }
    # 
    # 
    # # the below code has to loop for each unique grouping value
    # # now just go through the groups
    # 
    # for (g in 1:nrow(PeopleUnique)) {
    #   
    #   CurrentDef <- PeopleUnique[g,]
    #   
    #   # print(g)
    #   
    #   GroupInfo <- CurrentDef %>%
    #     mutate(across(where(is.factor), as.character))
    #   
    #   GroupInfo <- as.character(GroupInfo[1,])
    #   
    #   cat("Current def is", GroupInfo, "\n")
    #   
    #   GroupInfo <- CurrentDef %>%
    #     mutate(across(where(is.factor), as.character))
    #   
    #   
    #   GroupInfo <- as.character(GroupInfo[1,])
    #   
    #   
    #   CurrentGroup <- left_join(CurrentDef, Children, by = c(grpdef))
    # 
    #   HoursTooHigh <- CurrentGroup %>%
    #     filter(IntHours > hoursmax & InSchool == 2)
    #   
    #   HoursCanSub <- CurrentGroup %>%
    #     filter(IntHours <= hoursmax & InSchool == 1)
    #   
    #   # fix the length of these two so that they are the same
    #   
    #   if(nrow(HoursTooHigh) > nrow(HoursCanSub)) {
    #     
    #     HoursTooHigh <- HoursTooHigh %>%
    #       slice_sample(n = nrow(HoursCanSub))
    #   }
    #   
    #   if(nrow(HoursCanSub) > nrow(HoursTooHigh)) {
    #     
    #     HoursCanSub <- HoursCanSub %>%
    #       slice_sample(n = nrow(HoursTooHigh))
    #   }
    #   
    #   # length of both DFs is now the same
    #   
    #   # swap the school status
    # 
    #   # literally swap the hours worked in HoursTooHigh and HoursCanSub by row
    #   
    #   HoursTooHighStatus <- HoursTooHigh %>%
    #     select(IntHours) 
    #   
    #   HoursCanSubFixed <- HoursCanSub %>%
    #     select(-IntHours) %>%
    #     bind_cols(HoursTooHighStatus)
    #   
    #   
    #   HoursCanSubStatus <- HoursCanSub %>%
    #     select(IntHours) 
    #   
    #   HoursTooHighFixed <- HoursTooHigh %>%
    #     select(-IntHours) %>%
    #     bind_cols(HoursCanSubStatus)
    #   
    #   Fixed <- bind_rows(HoursCanSubFixed, HoursTooHighFixed) 
    # 
    #   # get the people in the group that weren't amended
    #   
    #   UnAmended <- CurrentGroup %>%
    #     filter(!(IntID %in% c(Fixed$IntID)))
    #   
    #   
    #   # this needs to be a file that takes all the groups
    #   
    #   if(exists("OutputDataFrame")) {
    #     
    #     OutputDataFrame <- bind_rows(OutputDataFrame, UnAmended, Fixed)
    #   } else {
    #     
    #     OutputDataFrame <- bind_rows(UnAmended, Fixed)
    #     
    #     # closes  if(exists("OutputDataFrame"))
    #   }
    # 
    #   # 
    #   # 
    #   # #####################################
    #   # #####################################
    #   # # split out the correctly assigned hours for school status
    #   # #####################################
    #   # #####################################
    #   # 
    #   # CorrectShorterHours <- Children %>%
    #   #   filter(as.integer(IntHours) <= hoursmax,
    #   #          InSchool == 2
    #   #          # as.integer(InSchool) == 1)
    #   #   )
    #   # 
    #   # CorrectLongerHours <- Children %>%
    #   #   filter(as.integer(IntHours) > hoursmax,
    #   #          InSchool == 1
    #   #          # as.integer(InSchool) == 2)
    #   #   )
    #   # 
    #   # # merge the two correct data frames
    #   # CorrectHours <- bind_rows(CorrectShorterHours, CorrectLongerHours)
    #   # 
    #   # #####################################
    #   # #####################################
    #   # # Work on the mismatches
    #   # #####################################
    #   # #####################################
    #   # 
    #   # MismatchedHours <- Children %>%
    #   #   filter(!(IntID %in% CorrectHours$IntID))
    #   # 
    #   # # print(table(MismatchedHours$IntHours))
    #   # 
    #   # # split out the two school statuses
    #   # 
    #   # MismatchedInSchool <- MismatchedHours %>%
    #   #   filter(InSchool == 2) %>%
    #   #   dplyr::select(-IntHours)
    #   # 
    #   # if(nrow(MismatchedInSchool) > 0) {
    #   #   
    #   #   
    #   #   LongerHoursUnused <- MismatchedHours %>%
    #   #     filter(IntID %in% MismatchedInSchool$IntID) %>%
    #   #     dplyr::select(IntHours)
    #   #   
    #   #   MismatchedWorking <- MismatchedHours %>%
    #   #     filter(InSchool == 1)
    #   #   
    #   #   # cat("There are", nrow(MismatchedWorking), "out of school people with shorter hours", "\n")
    #   #   # cat("There are", nrow(MismatchedInSchool), "school people with longer hours", "\n")
    #   #   
    #   #   
    #   #   # just use the damn counts
    #   #   RemainingShorterHours <- MismatchedHours %>%
    #   #     filter(as.integer(IntHours) <= hoursmax) %>%
    #   #     dplyr::select(IntHours)
    #   #   
    #   #   
    #   #   if((nrow(RemainingShorterHours) < nrow(MismatchedInSchool)) == TRUE) {
    #   #     
    #   #     stop("There are not enough shorter hours to assign to people in school.")
    #   #   }
    #   #   
    #   #   # seed must come before first sample is cut
    #   #   if (!is.null(userseed)) {
    #   #     set.seed(userseed)
    #   #   }
    #   #   
    #   #   # assign the shorter hours to the children in school
    #   #   NewShortHours <- RemainingShorterHours %>%
    #   #     slice_sample(n = nrow(MismatchedInSchool), replace = FALSE)
    #   #   
    #   #   FixedInSchool <- bind_cols(MismatchedInSchool, NewShortHours)
    #   #   
    #   #   # get the matched lower hours
    #   #   UsedShorterHours <- FixedInSchool %>%
    #   #     group_by(IntHours) %>%
    #   #     summarise(Used = n())
    #   #   
    #   #   # take the longer hours from the  participants, these
    #   #   # need to swap with the left-over shorter hours from adjusting the in-school adolescent hours
    #   #   # using the -index is creating an empty data frame
    #   #   LongerHoursUnused <- LongerHoursUnused %>%
    #   #     slice_sample(n = nrow(LongerHoursUnused), replace = FALSE)
    #   #   
    #   #   # cat("There are ", nrow(LongerHoursUnused), "available to swap in", "\n")
    #   #   #
    #   #   for (x in 1:nrow(UsedShorterHours)) {
    #   #     
    #   #     HoursLevel <- as.numeric(UsedShorterHours[x,1])
    #   #     NumberToChange <- as.numeric(UsedShorterHours[x,2])
    #   #     
    #   #     # cat("The hours category is", HoursLevel, "and the count is", NumberToChange, "\n")
    #   #     
    #   #     # sample NumberToChange with that hours level from the incorrect InWork data frame
    #   #     SampleOfNotInSchool <- MismatchedWorking %>%
    #   #       filter(IntHours == HoursLevel) %>%
    #   #       slice_sample(n = NumberToChange, replace = FALSE) %>%
    #   #       dplyr::select(-IntHours)
    #   #     
    #   #     # cat("The number of sampled rows is", nrow(SampleOfNotInSchool), "\n")
    #   #     
    #   #     # take the head of the unused longer hours
    #   #     SampledLongerHours <- LongerHoursUnused %>%
    #   #       slice_head(n = nrow(SampleOfNotInSchool))
    #   #     
    #   #     LongerHoursUnused <- LongerHoursUnused %>%
    #   #       slice_tail(n = (nrow(LongerHoursUnused) - nrow(SampledLongerHours)))
    #   #     
    #   #     # cat("There are ", nrow(LongerHoursUnused), "available to swap in after matching", "\n")
    #   #     
    #   #     FixedInWork <- bind_cols(SampleOfNotInSchool,SampledLongerHours)
    #   #     
    #   #     # cat("Length of FixedInWork is", nrow(FixedInWork), "\n")
    #   #     
    #   #     if(exists("WorkFixed") == TRUE) {
    #   #       
    #   #       #       cat("Enters this loop with", nrow(FixedInWork), "rows in the created data frame", "\n")
    #   #       
    #   #       WorkFixed <- bind_rows(WorkFixed, FixedInWork)
    #   #       
    #   #       # cat("After being constructed earlier, Work fixed is", nrow(WorkFixed), "\n")
    #   #       
    #   #     } else {
    #   #       
    #   #       WorkFixed <- FixedInWork
    #   #       
    #   #       
    #   #       # cat("Work fixed is", nrow(WorkFixed), "\n")
    #   #       
    #   #       # closes if loop for constructing people with shorter hours
    #   #     }
    #   #     
    #   #     MismatchedWorking <- MismatchedWorking %>%
    #   #       filter(!(IntID %in% SampleOfNotInSchool$IntID))
    #   #     
    #   #     #    cat("MismatchedWorking contains", nrow(MismatchedWorking), "at this point", "\n")
    #   #     
    #   #     # closes loop through the hours
    #   #   }
    #   #   
    #   #   # cat("Correct hours is", nrow(CorrectHours), "Fixed in school is", nrow(FixedInSchool), "work fixed is", nrow(WorkFixed),
    #   #   #     "Mismatched working is", nrow(MismatchedWorking), "\n")
    #   #   
    #   #   if(exists("OutputDataFrame")) {
    #   #     
    #   #     # de-dup WorkFixed from those already in DF
    #   #     OutputDataFrame <- OutputDataFrame %>%
    #   #       filter(!(IntID %in% c(WorkFixed$IntID)))
    #   #     
    #   #     OutputDataFrame <- bind_rows(OutputDataFrame, CorrectHours, FixedInSchool, MismatchedWorking)
    #   #     
    #   #   } else {
    #   #     
    #   #     OutputDataFrame <- bind_rows(CorrectHours, FixedInSchool, MismatchedWorking)
    #   #   }
    #   #   
    #   #   # closes if(nrow(MismatchedInSchool) > 0)
    #   # } else {
    #   #   
    #   #   if(exists("OutputDataFrame")) {
    #   #     
    #   #     # de-dup WorkFixed from those already in DF
    #   #     OutputDataFrame <- OutputDataFrame %>%
    #   #       filter(!(IntID %in% c(WorkFixed$IntID)))
    #   #     
    #   #     OutputDataFrame <- bind_rows(OutputDataFrame, Children)
    #   #     
    #   #   } else {
    #   #     
    #   #     OutputDataFrame <- Children
    #   #   }
    #   #   
    #   #   # closes else to if(nrow(MismatchedInSchool) > 0)
    #   # }
    #   
    #   # closes for (g in 1:nrow(PeopleUnique))
    # }
    # 
    # if (is.factor(people[,statuscol]) == TRUE) {
    #   
    #   #   cat("School identifier is a factor")
    #   
    #   InSchoolLabels <- levels(people[,statuscol])
    #   
    #   OutputDataFrame <- OutputDataFrame %>%
    #     mutate(InSchool= factor(InSchool, labels = c(InSchoolLabels), order = TRUE))
    #   
    #   #close factor test for school variable
    # }
    # 
    # if (is.ordered(people[,hourscol]) == TRUE) {
    #   
    #   cat("Hours worked is a factor 2", "\n")
    #   
    #   HoursLabels <- levels(people[,hourscol])
    #   
    #   OutputDataFrame <- OutputDataFrame %>%
    #     mutate(IntHours = factor(IntHours, 
    #                              levels = c(1:length(HoursLabels)),
    #                              labels = c(HoursLabels), order = TRUE))
    #   
    #   #close factor test for hours worked variable
    # }
    # 
    # 
    # OutputDataFrame <- OutputDataFrame %>%
    #   rename(!!ChildrenIDColName := IntID,
    #          !!ChildrenStatusColName := InSchool,
    #          !!ChildrenHoursColName := IntHours)
    # 
    # 
    # return(OutputDataFrame)

  #closes function
}
