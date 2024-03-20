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


intergrp <- function(people, grpdef = NULL, pplagecol = NULL, pplpropcol = NULL, intfile, intmin = NULL, 
                     intmax = NULL, userseed = NULL) {

  options(dplyr.summarise.inform=F)
  
  # print(names(people))
  

  # get the unique set of grouping factors
  PeopleUnique <- people[,grpdef] %>%
    unique()

  # get column names as symbols to use inside data frame subfunctions

  AgeColName <- sym(names(people[pplagecol]))

  PropscolName <- sym(names(people[pplpropcol]))
  
  # do the rename
  peopleRenamed <- people %>%
    rename(OriginalAge = !! pplagecol,
           OriginalProp = !! pplpropcol)
  
  agesForInterp <- intfile %>%
    rename(MinInterpol = !! intmin,
           MaxInterpol = !! intmax)
  
  # get unique ages in the dataframe
  AgesUsed <- peopleRenamed %>%
    group_by(OriginalAge) %>%
    summarise() %>%
    unique()

  
  if (!is.null(userseed)) {
    set.seed(userseed)
  }
  
  # loop through the unique rows
  for(i in 1:nrow(PeopleUnique)) {
    
    CurrentDef <- PeopleUnique[i,]

    CurrentGroup <- left_join(CurrentDef, peopleRenamed)
    
    CurrentAgesForInt <- left_join(CurrentDef, agesForInterp)
    
    MinIntAge <- CurrentAgesForInt$MinInterpol
    MaxIntAge <- CurrentAgesForInt$MaxInterpol
    
    AgeRangeseq <- data.frame(Ages = c(seq(MinIntAge, MaxIntAge, by = 1)))
    
    # check if any age groups missing, insert 0 values if missing

   CurrentInterp <- data.frame(spline(CurrentGroup$OriginalAge,
                                       CurrentGroup$OriginalProp,
                                       xout = AgeRangeseq$Ages, method = "natural"))
   
   # doesn't contain the group value info
   # need to append the columns
   
   # create expansion of grouping
   ExpandedGroup <- CurrentDef %>%
     slice_sample(n = nrow(CurrentInterp), replace = TRUE)

   WithGroupInfo <- bind_cols(CurrentInterp, ExpandedGroup) %>%
     rename(Age = x, Estimated = y)

   if(exists("AllResults")) {
     
     AllResults <- bind_rows(AllResults, WithGroupInfo)
   } else{
     
     AllResults <- WithGroupInfo
   }

    # closes for(i in 1:nrow(PeopleUnique))
  } 
  
  # fix any results < 0 or > 1
  AllResults <- AllResults %>%
    mutate(Estimated = ifelse(Estimated < 0, 0, ifelse(Estimated > 1, 1, Estimated)))
 
  return(AllResults)
  #closes function
}
