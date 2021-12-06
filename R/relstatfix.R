#' Reallocates the working hours between people in school and people not in school.
#' This function reallocates working hours so that adolescents in school work fewer hours than adolescents still in school.
#' The re-allocation is performed initially for the adolescents still in school. This ensures that the shorter hours worked have a higher probability of being allocated to adolescents who are very unlikely to have longer hours worked. The approach is designed to prevent longer working hours, for example full-time hours, being allocated to adolescents who are still studying.
#' The data frame must be restricted to only those whose hours can be reallocated.
#' @export
#' @param adolescents A data frame containing all adolescents who have working hours.
#' @param adlidcol The column number for the unique value that identifies unique adolescents.
#' @param statuscol The column number containing the indicator of whether an adolescent is in school or has left school. Can be either an ordered factor or numeric. If this is a factor, factor level 2 must be in-school. If it is a numeric variable, the lowest number must be the in-school value. This is output as an ordered factor.
#' @param hourscol The column number containing the hours worked by each adolescent. Must be an ordered factor or numeric. The levels/values must be ascending for hours worked. This is output as an ordered factor.
#' @param hoursmax The maximum hours worked by adolescents in-school. Must be the relevant factor level/number from HoursWorked.
#' @param grpcol The column number containing any grouping variable to be used. If this is used, the changes to the working hours will be performed using grouped data. Within-group totals for the working hours categories will be retained.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return A data of observations, with working hours reallocated so that an adolesent's working hours is compatible with schooling
#'
#' @examples
#' # no grouping variable
#' AdolescentWork <- hoursfix(WorkingAdolescents, adlidcol = 3, statuscol = 6, hourscol = 5, hoursmax = 3, userseed = 4)
#'
#' # grouping variable
#' # when a group is used
#' AdolescentWork2 <- hoursfix(WorkingAdolescents, adlidcol = 3, statuscol = 6, hourscol = 5,
#'                             hoursmax = 3, grpcol = 1, userseed = 4)


relstatfix <- function(people, grpdef = NULL, pplidcol = NULL, pplagecol = NULL, pplstatcol = NULL, stfixval = NULL,
                       props, matchdef = NULL, rawcol = NULL, userseed = NULL) {

  options(dplyr.summarise.inform=F)
  
  # print(names(people))
  
  
  #####################################
  # quick reasonableness checks
  #####################################
  
  # str(people)
  
  if (!(all((grpdef) %in% names(people)))) {
    stop("The matching columns in grpdef must exist in the data frame.")
  }

  if (!(all((matchdef) %in% names(people))) | !(all((matchdef) %in% names(props)))) {
    stop("The matching columns in matchdef must exist in the data frame.")
  }
  

  # need to do the name change here
  
  peopleRenamed <- people %>%
    rename(StatusID = !! pplstatcol,
           PersonID = !! pplidcol)


  # get the unique set of grouping factors
  PeopleUnique <- people[,grpdef] %>%
    unique()

  # get column names as symbols to use inside data frame subfunctions
  
  AgeColName <- sym(names(people[pplagecol]))
  
  IDColName <- sym(names(people[pplidcol]))

  StatuscolName <- sym(names(people[pplstatcol]))
  
  PropscolName <- sym(names(props[rawcol]))
  
  # print(class(stfixval))
  
  if (!is.null(userseed)) {
    set.seed(userseed)
  }

  # loop through the unique rows
  for(i in 1:nrow(PeopleUnique)) {
    
    
    # delete previous versions of these data frames
    if(exists("UnderSample")) {
      rm(UnderSample)
    }
    
    if(exists("OverSample")) {
      rm(OverSample)
      }
    
    # fix for one grouping variable
    # see https://stackoverflow.com/a/69116009/1030648
    CurrentDef = PeopleUnique[i, , drop=FALSE]

    CurrentGroup <- left_join(CurrentDef, peopleRenamed, by = c(grpdef))
    cat("Group is", "\n")
    print(CurrentDef)

    
  # get the number in each status in the variable of interest
    NumInEachStatus <- CurrentGroup %>%
      group_by(StatusID) %>%
      summarise(NumPerLevel = n())
    
   # extract the number of people in the status that needs fixing
    NumToFix <- pull(NumInEachStatus %>%
      filter(StatusID == stfixval) %>%
      select(NumPerLevel))

    if(length(NumToFix) == 0 & is.integer(NumToFix)) {
      
      NumToFix <- 0
    }
    
    # cat("Number of rows in current group is", nrow(CurrentGroup), "and Num in category is", NumToFix, "\n")
    

    
    # skip over the groups when the number in the status is the same as the number in the group
    # there is no-one to swap

    if(nrow(CurrentGroup) > NumToFix & NumToFix > 0) {

    # get the people that are in this subgroup

    MatchingValues <- CurrentGroup %>%
      select(all_of(matchdef)) %>%
      unique()
    
    RelevantProps <- left_join(MatchingValues, props, by = c(matchdef)) 

    InitialCounts <- CurrentGroup %>%
      group_by(!!AgeColName) %>%
      # group_by(!!AgeColName, StatusID) %>%
      # filter(StatusID == stfixval) %>%
      summarise(TotalinStatus = n())
    
    StatOnlyCounts <- CurrentGroup %>%
      group_by(!!AgeColName, StatusID) %>%
      filter(StatusID == stfixval) %>%
      summarise(NuminDesStatus = n())
    
    # cat("Initial counts construction below", "\n")
    
    InitialCounts <- left_join(InitialCounts, StatOnlyCounts)
    
        # cat("Initial counts constructed", "\n")

    
    if(is.factor(expr(`$`(InitialCounts, !!StatuscolName))) == TRUE) {

      # cat("is.factor loop entered", "\n")
   
      CountComp <- left_join(RelevantProps, InitialCounts) %>%
        mutate(TotalinStatus = ifelse(is.na(TotalinStatus), 0, TotalinStatus),
               ExpectedCount = round(TotalinStatus * !!PropscolName, 0),
               NuminDesStatus = ifelse(is.na(NuminDesStatus), 0, NuminDesStatus))

      # cat("CountComp in factor loop constructed", "\n")
      
      CountComp$StatusID[is.na(CountComp$StatusID)] <- stfixval
      

    } else {

      CountComp <- left_join(RelevantProps, InitialCounts) %>%
        mutate(TotalinStatus = ifelse(is.na(TotalinStatus), 0, TotalinStatus),
               ExpectedCount = round(TotalinStatus * !!PropscolName, 0),
               StatusID = ifelse(is.na(StatusID), stfixval, StatusID),
               NuminDesStatus = ifelse(is.na(NuminDesStatus), 0, NuminDesStatus))

    }
    
    # cat("Joining RelevantProps and InitialCounts", "\n")
    
      CountComp <- left_join(RelevantProps, InitialCounts) %>%
        mutate(TotalinStatus = ifelse(is.na(TotalinStatus), 0, TotalinStatus),
               ExpectedCount = round(TotalinStatus * !!PropscolName, 0),
               NuminDesStatus = ifelse(is.na(NuminDesStatus), 0, NuminDesStatus))
      
     
      # cat("Join RelevantProps and InitialCounts complete", "\n")

    ActualCounts <- sum(CountComp$NuminDesStatus)
    CalculatedCounts <- sum(CountComp$ExpectedCount)
    
    if(is.na(CalculatedCounts)) {
      
      stop("Current group contains missing values", "\n")
    }

    cat("Actual counts are", ActualCounts, "and the calculated counts are", CalculatedCounts, "\n")
    
    cat("Calculated counts must be multipled by ", ActualCounts/CalculatedCounts, "\n")
    
    TheMultiplier <- ActualCounts/CalculatedCounts
    
    CountComp <- CountComp %>%
      mutate(OldExpected = round(TotalinStatus * !!PropscolName, 0),
             ExpectedCount = floor(TotalinStatus * !!PropscolName * TheMultiplier),
             Remainder = (TotalinStatus * !!PropscolName * TheMultiplier) %% 1)
    
    CountsDiff <- sum(CountComp$NuminDesStatus) - sum(CountComp$ExpectedCount)
    
    # using this method to ensure that the counts line up
   #  https://stackoverflow.com/a/3956184/1030648
    
    CountComp <- CountComp %>%
      arrange(desc(Remainder))
    
    # add 1 to the counts for n == CountsDiff
    # note that this could, theoretically, create an expected count > available people that age
    # need to test
    
    for(l in 1:nrow(CountComp)) {
      
      if(CountsDiff > 0) {
      
      if(CountComp$TotalinStatus[l] >= CountComp$ExpectedCount[l]+ 1) {
        
        CountComp$ExpectedCount[l] <- CountComp$ExpectedCount[l] + 1
      }
        
        CountsDiff <- CountsDiff - 1
        
        # closes if(CountsDiff > 0)
      }
      
      # closes for(l in 1:nrow(CountComp))
    } 

    # TODO need to create a fix if there are not enough values to increase by 1?
    # not sure that this can occur

    # problem is there are no people in the group, so skip over these
    
    if(ActualCounts > 0) {

    ###########################
   
    CountComp <- CountComp %>%
      mutate(DiffsNeeded = ExpectedCount - NuminDesStatus) 

    UndersCount <- CountComp %>%
      filter(DiffsNeeded < 0) %>%
      summarise(Rows = n()) %>%
      pull(Rows)
    
    OversCount <- CountComp %>%
      filter(DiffsNeeded > 0) %>%
      summarise(Rows = n()) %>%
      pull(Rows) 
    
    # cat("Unders is", UndersCount, "and overs is", OversCount, "\n")
    # 
 
    CheckNoZeros <- sum(abs(CountComp$DiffsNeeded)) 

    
    # skip process if there are no differences between desired and actual counts
    # in all ages
    # OR there are no records to sub for the problem ones 

    if(CheckNoZeros > 0 & !(UndersCount == 0) & !(OversCount == 0)) {

    CountUnders <- CountComp %>%
      filter(DiffsNeeded < 0)
    

    # get the people to swap
    
    for(j in 1:nrow(CountUnders)) {
      
      CurrentTooMany <- CountUnders[j,]
      
      # cat("Current too many is", CurrentTooMany$DiffsNeeded, "\n")
      
      
      CurrentSample <- left_join(CurrentTooMany, CurrentGroup) %>%
        filter(StatusID == stfixval) %>%
        slice_sample(n = abs(CurrentTooMany$DiffsNeeded))
      
      if(exists("UnderSample")) {
        UnderSample <- bind_rows(UnderSample, CurrentSample)
      } else {
        UnderSample <- CurrentSample
      }

    # closes  for(j in 1:nrow(CountUnders))
    }
    


    CountOvers <- CountComp %>%
      filter(DiffsNeeded  > 0)

    
    for(k in 1:nrow(CountOvers)) {
      
      CurrentTooFew <- CountOvers[k,]
      
      # cat("Current too few is", CurrentTooFew$DiffsNeeded, "\n")
      
      CurrentSample <- left_join(CurrentTooFew, CurrentGroup, by = c(matchdef)) %>%
        filter(!(StatusID.y == stfixval)) %>%
        mutate(StatusID = StatusID.y) %>%
        select(-c(StatusID.x, StatusID.y)) %>%
        slice_sample(n = abs(CurrentTooFew$DiffsNeeded))
        
        # return(CurrentSample)
      
      if(exists("OverSample")) {
        OverSample <- bind_rows(OverSample, CurrentSample)
      } else {
        OverSample <- CurrentSample
      }
      # closes  for(j in 1:nrow(CountOvers))
    }
    
    ##################################################
    # unders and overs may not be the same,
    # force them to be the same length - make the larger one the length of the smaller one if so
    ##################################################




    if(nrow(UnderSample) > nrow(OverSample)) {

      UnderSample <- UnderSample %>%
        slice_sample(n = nrow(OverSample))
    }

    if(nrow(OverSample) > nrow(UnderSample)) {

      OverSample <- OverSample %>%
        slice_sample(n = nrow(UnderSample))
    }
  
    # now swapping the ages
    # only need to link age to ID, will wash out in the original data
    # should just be a straight join and then swap
    # remove any ordering
    
    UnderSample <- UnderSample %>%
      slice_sample(n = nrow(UnderSample))
    
    cat("Undercount is", (nrow(UnderSample)), "\n")
    
    OverSample <- OverSample %>%
      slice_sample(n = nrow(OverSample))
    
    cat("Overcount is", nrow(OverSample), "\n")

    # literally swap the ages in UnderDF and OverDF by row

    UnderSampleAges <- UnderSample %>%
      select(!!AgeColName) 
    
    OverSampleFixed <- OverSample %>%
      select(-!!AgeColName) %>%
      bind_cols(UnderSampleAges)
    

    OverSampleAges <- OverSample %>%
      select(!!AgeColName) 
    
    UnderSampleFixed <- UnderSample %>%
      select(-!!AgeColName) %>%
      bind_cols(OverSampleAges)
    
    Fixed <- bind_rows(OverSampleFixed, UnderSampleFixed) %>%
      select(-(c(NuminDesStatus, ExpectedCount, DiffsNeeded)))
    

  # add them to the others that haven't been amended
    
    UnAmended <- CurrentGroup %>%
      filter(!(PersonID %in% c(Fixed$PersonID)))

    
    # this needs to be a file that takes all the groups
    
    if(exists("GroupFixed")) {

      GroupFixed <- bind_rows(GroupFixed, UnAmended, Fixed)
    } else {

      GroupFixed <- bind_rows(UnAmended, Fixed)
      
    # closes  if(exists("GroupFixed"))
    }
    
    # closes if(CheckNoZeros > 0 & !(UndersCount == 0) & !(OversCount == 0))
    } else {
      
      if(exists("GroupFixed")) {
        
        GroupFixed <- bind_rows(GroupFixed, CurrentGroup)
      } else {
        
        GroupFixed <- CurrentGroup
        
        
        # closes  if(exists("GroupFixed"))
      }
      
      # closes else to  if(CheckNoZeros > 0 & !(UndersCount == 0) & !(OversCount == 0))
    }
    
    # closes if(ActualCounts > 0)
    }
    
    # closes if(nrow(CurrentGroup) > NumToFix)
    } else {
      
      if(exists("GroupFixed")) {
        
        GroupFixed <- bind_rows(GroupFixed, CurrentGroup)
      } else {
        
        GroupFixed <- CurrentGroup


        # closes  if(exists("GroupFixed"))
      }
      
      # closes if(nrow(CurrentGroup) > NumToFix)
    }
    
        # closes for(i in 1:nrow(PeopleUnique))
  } 
  
  # fix the col names and remove the extra rows
  
  OutputDataFrame <- GroupFixed %>%
    rename(!!IDColName := PersonID,
           !!StatuscolName := StatusID) %>%
    select(-c(TotalinStatus, {{PropscolName}}, OldExpected, Remainder)) 

  return(OutputDataFrame)
  #closes function
}
