#' Estimates values for ages when only age--band level data is available.
#' This function uses values provided for age-band data to interpolate the relevant values for ages.
#' 
#' The node ages for each age--band are defined by the user, along with the age--band values. The function will then impute the values within the age--band edges. If ages requiring estimates are less than the minimum node age, or greater than the maximum node age, the formula for the neighbouring edge will be extrapolated to construct the values.
#' 
#' While the function is designed to interpolate proportions, in practice it can interpolate any values. The limitation is that the function performs no rounding. Integer node values may produce non-integer estimates.
#' 
#' @export
#' @param people A data frame containing all grouping variables, the node ages for each group, and the associated node values.
#' @param pplagecol The people column number containing the variable containing the node ages.
#' @param pplpropcol The people column number containing the node values.
#' @param ageranges A data frame that defines the minimum and maximum ages for each group. This data frame must include all variables used to define the groups. 
#' @param endmincol The ageranges column number that contains the minimum age for each group.
#' @param endmaxcol The ageranges column number that contains the maximum age for each group.
#' @param groupdef The character vector containing the names the grouping variables.
#'
#' @return A data frame containing the estimated value for each relevant age within each group. There are four columns: the age, one column for each grouping variable, and the fitted values.
#' 
#' @examples
#' AdolescentWork2 <- hoursfix(WorkingAdolescents, adlidcol = 3, statuscol = 6, hourscol = 5,
#'                             hoursmax = 3, grpcol = 1)


interdiff <- function(people, pplagecol = NULL, pplpropcol = NULL, ageranges, endmincol = NULL, endmaxcol = NULL,
                      grpdef = NULL) {
  
  options(dplyr.summarise.inform=F)
  
  # print(names(people))
  
  
  # get the unique set of grouping factors
  PeopleUnique <- as.data.frame(people[,grpdef] %>%
    unique())

  
  # get column names as symbols to use inside data frame subfunctions
  
  AgeColName <- sym(names(people[pplagecol]))
  
  PropscolName <- sym(names(people[pplpropcol]))
  
  # do the rename
  peopleRenamed <- as.data.frame(people %>%
    rename(OriginalAge = !! pplagecol,
           OriginalProp = !! pplpropcol))
  
  WorkingAges <- as.data.frame(ageranges %>%
    rename(MinAge = !! endmincol,
           MaxAge = !! endmaxcol))

  # loop through the unique rows
  for(i in 1:nrow(PeopleUnique)) {
    
    CurrentDef <- PeopleUnique[i,]

    WorkingAgeMin <- as.numeric(left_join(CurrentDef, ageranges, by = c(grpdef)) %>%
                                  select(MinAge) %>%
                                  pull())
    
    WorkingAgeMax <- as.numeric(left_join(CurrentDef, ageranges, by = c(grpdef)) %>%
                                  select(MaxAge) %>%
                                  pull())
    
    PreviousMinAge <- WorkingAgeMax
    
    PreviousMaxAge <- WorkingAgeMin

    GroupInfo <- CurrentDef %>%
      mutate(across(where(is.factor), as.character))


    GroupInfo <- as.character(GroupInfo[1,])

    cat("Current def is", GroupInfo, "and minimum age is", WorkingAgeMin, "and maximum age is", WorkingAgeMax, "\n")


    GroupInfo <- CurrentDef %>%
      mutate(across(where(is.factor), as.character))


    GroupInfo <- as.character(GroupInfo[1,])


    CurrentGroup <- left_join(CurrentDef, peopleRenamed, by = c(grpdef))
    
    # need to sort CurrentGroup by age
    CurrentGroup <- CurrentGroup %>%
      arrange(OriginalAge)

    # do the calculations
    # has to be to nrow(-1) as there is no point to diff when we get to nrow()

    for(j in 1:(nrow(CurrentGroup)-1)) {

      startpt = CurrentGroup$OriginalAge[j]
      endpt = CurrentGroup$OriginalAge[(j+1)]

      # cat("1 Start point is", startpt, "and end point is", endpt, "\n")

      ptdiff <- endpt - startpt

      startval <- CurrentGroup$OriginalProp[j]
      endval = CurrentGroup$OriginalProp[(j+1)]

      # cat("2 Start value is", startval, "and end value is", endval, "\n")

      valdiff <- endval - startval

      # do the estimates for the group

      # print(chgpts)

      # get the relevant inflexion points

      mininflex <- startpt

      maxinflex <- endpt

      
      # cat("3 mininflex is", mininflex, "and maxinflex is", maxinflex, "\n")

      cat("4 Inflexion points are", mininflex, "and", maxinflex, "\n")

      #do the next bit by whole numbers
      minintval <- ceiling(mininflex)
      maxintval <- floor(maxinflex)

      # cat("5 Interpolation range is", minintval, "to", maxintval, "\n")

      sloperange <- maxinflex - mininflex

      # print(sloperange)

      slopeval <- valdiff/sloperange

      # cat("Slope is", slopeval, "\n")

      CurrentDF <- data.frame(Age = c(seq(from = minintval, to = maxintval, by =1)))

      CurrentDF <- CurrentDF %>%
        mutate(Fits = startval + (Age - mininflex) * slopeval)
      
      CurrentMinAge <- min(CurrentDF$Age)
      CurrentMaxAge <- max(CurrentDF$Age)
      
      if(CurrentMinAge < PreviousMinAge) {
        
        PreviousMinAge <- CurrentMinAge
        MinInflex <- mininflex
        MinSlopeVal <- slopeval
        MinStartVal <- startval
        
      }

      if(CurrentMaxAge > PreviousMaxAge) {
        
        PreviousMaxAge <-CurrentMaxAge
        MaxInflex <- mininflex
        MaxSlopeVal <- slopeval
        MaxStartVal <- startval
        
      }
      
      if(exists("GroupDF")) {

        GroupDF <-  bind_rows(GroupDF, CurrentDF)

      } else{

        GroupDF <- CurrentDF
      }

      # closes for(j in 1:nrow(CurrentGroup))
    }

 
    # get rid of any duplicate rows

    GroupDF <- GroupDF %>%
      unique()
    
    if(WorkingAgeMin < min(GroupDF$Age)) {
      
      cat("Entered min loop and WorkingAgeMin is", WorkingAgeMin,  "and min age is", min(GroupDF$Age) , "\n")

      AppendDF <- data.frame(Age = c(seq(from = WorkingAgeMin, to = (min(GroupDF$Age) -1 ), by = 1)))
      
      AppendDF <- AppendDF %>%
        mutate(Fits = MinStartVal + (Age - MinInflex) * MinSlopeVal)
      
      GroupDF <-  bind_rows(AppendDF, GroupDF)

      # closes if(WorkingAgeMin > min(GroupDF$Age))
    }
    
    if(WorkingAgeMax > max(GroupDF$Age)) {
      
      AppendDF <- data.frame(Age = c(seq(from = (max(GroupDF$Age) +1), to = WorkingAgeMax, by = 1)))
      
      AppendDF <- AppendDF %>%
        mutate(Fits = MaxStartVal + (Age - MaxInflex) * MaxSlopeVal)
      
      GroupDF <-  bind_rows(GroupDF, AppendDF)
      
      # closes if(WorkingAgeMax > max(GroupDF$Age))
    }

    # add in group information
    ExpandedGroup <- CurrentDef %>%
      slice_sample(n = nrow(GroupDF), replace = TRUE)

    WithGroupInfo <- bind_cols(GroupDF, ExpandedGroup)

 
    if(exists("AllResults")) {

      AllResults <- bind_rows(AllResults, WithGroupInfo)
    } else{

      AllResults <- WithGroupInfo
    }

    rm(GroupDF)
    
    # closes for(i in 1:nrow(PeopleUnique))
  }
  
    AllResults <- AllResults %>%
      rename(!!AgeColName := Age)

  return(AllResults)
# 
#   # fix any results < 0 or > 1
#   AllResults <- AllResults %>%
#     mutate(Fits = ifelse(Fits < 0, 0, ifelse(Fits > 1, 1, Fits))) %>%
#     rename(!!AgeColName := OriginalAge)
# 
  # return(AllResults)
  #closes function
}
