#' Reallocates the working hours between people in education and people not in education.
#' This function reallocates working hours so that people in education work fewer hours than people not in education. Pre-cleaning so that only people inside the student age range is not required. The hours of work are reallocated so that shorter hours worked are prioritised to those in education. The variables provided in the grpdef vector define the marginal totals that must be retained.
#' @export
#' @param people A data frame containing individual people.
#' @param pplid The variable containing the unique identifier for each person, in the people data frame.
#' @param pplstat The variable containing the indicator of whether a person is in education, in the people data frame. This must consist of only two values, and can be either an ordered factor or numeric. If this is a factor, factor level 2 must be for those in education. If it is a numeric variable, the lowest number must be for those in education.
#' @param pplhours The variable containing the hours worked by each adolescent. Must be a factor or numeric. If this is a factor, it is assumed to be ordered. The levels/values must be ascending for hours worked.
#' @param hoursmax The maximum hours worked by people in education. Must be the relevant factor level/number from pplhours.
#' @param grpdef The vector containing any grouping variable to be used. If this is used, the changes to the working hours will be performed using grouped data. Marginal totals for the cross-tabulations of the grouping variables are retained.
#' @param userseed If specified, this will set the seed to the number provided. If not, the normal set.seed() function will be used.
#'
#' @return A data of observations, with working hours reallocated so that people's working hours are compatible with their education status.
#'
#' @examples
#' # no grouping variable
#' AdolescentWork <- hoursfix(WorkingAdolescents, adlidcol = 3, statuscol = 6, hourscol = 5, hoursmax = 3, userseed = 4)
#'
#' # grouping variable
#' # when a group is used
#' AdolescentWork2 <- hoursfix(WorkingAdolescents, adlidcol = 3, statuscol = 6, hourscol = 5,
#'                             hoursmax = 3, grpcol = 1, userseed = 4)


fixhours <- function(people, pplid, pplstat, pplhours, hoursmax, grpdef, userseed = NULL) {
  
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
  
  # cat("The minimum in school value is", minInSchool, "and the maximum in school value is", maxInSchool, "\n")

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
      
      cat("Group is", "\n")
      print(CurrentDef)
      
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

    # cat("Hours worked is a factor", "\n")

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

  #closes function
}
