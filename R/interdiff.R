#' Estimates values for ages when only age group data is available.
#' This function uses values provided for age group to interpolate the relevant values for ages.
#' 
#' The node ages for each age--band are defined by the user, along with the age--band values. The function will then impute the values within the age--band edges. 
#' Zero values at both extremes must be included. For example, for the age group 20-24 years, the pplprop value is for pplage. if the first non-zero relationship probability is for the age group 20-24 years, and the previous age group is 15-19 years, pplprop==0 for pplage==19. 
#'For each age group, there must be a minimum and maximum age specified. This provides the interpolation range for each age group. For the anchoring 0 values, the minimum and maximum ages are the same. In this example, for pplage==19, endmin==19, and endmax==19.
#' If there is no zero for older ages, as the final node value occurs inside the age group, the function assumes that the last node-to-node should be used to extrapolate for the ages older than the oldest node value. For example, if the last node value is for 90 years of age, but the oldest age is 95 years, the function will assume the same slope for ages 91 through 95 years. 
#' The function can perform a separate interpolation for groups, for example, a separate interpolation can be performed for each sex. The function is flexible for the number of variables that can be used to define groups. If only one interpolation is required, the same grpdef value should be used for each row in the data frame.
#' 
#' While the function is designed to interpolate proportions, in practice it can interpolate any values. The limitation is that the function performs no rounding. Integer node values may produce non-integer estimates.
#' 
#' @export
#' @param nodes A data frame containing all grouping variables, the node ages for each group, and the associated node values.
#' @param pplage The variable containing the node ages.
#' @param pplprop The variable containing the node values.
#' @param endmin The variable that contains the minimum age for each group.
#' @param endmax The variable that contains the maximum age for each group.
#' @param groupdef A character vector containing the names of the grouping variables.
#'
#' @return A data frame containing the fitted values, by age within group.
#' 
#' @examples
#' AdolescentWork2 <- hoursfix(WorkingAdolescents, adlidcol = 3, statuscol = 6, hourscol = 5,
#'                             hoursmax = 3, grpcol = 1)


interdiff <- function(nodes, pplage, pplprop, endmin, endmax, grpdef) {
  
  options(dplyr.summarise.inform=F)
  
  # print(names(people))
  
  
  # Subset the node dataframe
  PeopleUnique <- as.data.frame(nodes[,grpdef] %>%
    unique())

  #####################################
  # check for missing input information
  #####################################
  
  if (!pplage %in% names(nodes)) {
    stop("Age variable in the nodes data frame is incorrect.")
  }
  
  if (!pplprop %in% names(nodes)) {
    stop("Node variable in the nodes data frame is incorrect.")
  }
  
  if (!endmin %in% names(nodes)) {
    stop("Minimum age variable name in the ageranges data frame is incorrect.")
  }
  
  if (!endmax %in% names(nodes)) {
    stop("Maximum age variable name in the ageranges data frame is incorrect.")
  }


  # get column names as symbols to use inside data frame subfunctions
  
  agevarname <- sym(names(nodes[pplage]))

  
  # #####################################
  # #####################################
  # # rename variables so don't need to use quosures inside code
  # #####################################
  # #####################################

  peopleRenamed <- as.data.frame(nodes %>%
    rename(OriginalAge = !! pplage,
           OriginalProp = !! pplprop,
           MinAge = !! endmin,
           MaxAge = !! endmax))
  

  
  # loop through the unique rows
  for(i in 1:nrow(PeopleUnique)) {
    

    # below returns a vector if there is only one grouping variable
    # CurrentDef <- PeopleUnique[i,]
    
    
    # fix for one grouping variable
    # see https://stackoverflow.com/a/69116009/1030648
    CurrentDef = PeopleUnique[i, , drop=FALSE]
    

    WorkingAgeMin <- min(as.numeric(left_join(CurrentDef, peopleRenamed, by = c(grpdef)) %>%
                                      select(MinAge) %>%
                                      pull(MinAge)))
    
    
    WorkingAgeMax <- max(as.numeric(left_join(CurrentDef, peopleRenamed, by = c(grpdef)) %>%
                                      select(MaxAge) %>%
                                      pull()))
    
    # cat("WorkingAgeMin is ", WorkingAgeMin, " and WorkingAgeMax is ", WorkingAgeMax, "\n")
    
    GroupInfo <- CurrentDef %>%
      mutate(across(where(is.factor), as.character))
    
    
    GroupInfo <- as.character(GroupInfo[1,])

    # cat("Current def is", GroupInfo, "and minimum age is", WorkingAgeMin, "and maximum age is", WorkingAgeMax, "\n")

    # create data frame with ages
    
    GroupInfo <- CurrentDef %>%
      mutate(across(where(is.factor), as.character))
    
    
    GroupInfo <- as.character(GroupInfo[1,])

    CurrentGroup <- left_join(CurrentDef, peopleRenamed, by = c(grpdef))
    
    # need to sort CurrentGroup by age
    CurrentGroup <- CurrentGroup %>%
      arrange(OriginalAge)


    # dataframe of defined probs constructed
    
    # count number of rows in the group, need to handle one-row groups separately to 
    # prevent an NA error
    NumRowsInCurrentGroup <- nrow(CurrentGroup)
    
    # work through the CurrentGroup dataframe
    if(NumRowsInCurrentGroup == 1) {
      
      CurrentDF <- data.frame(Age = c(seq(from = WorkingAgeMin, to = WorkingAgeMax, by =1)))

     # cat("The probability to apply is", CurrentGroup$OriginalProp[1], "\n")

      startpt <-  CurrentGroup$OriginalProp[1]
      
      CurrentDF$Fits <- rep(startpt, nrow(CurrentDF))
      
      # add in group information
      ExpandedGroup <- CurrentDef %>%
        slice_sample(n = nrow(CurrentDF), replace = TRUE)
      
      # cat("The number of rows of CurrentDF is", nrow(CurrentDF), "and the number of rows of ExpandedGroup is", nrow(ExpandedGroup), "\n")
      
      CurrentDF <- bind_cols(CurrentDF, ExpandedGroup)
      
      CurrentMinAge <- min(CurrentDF$Age)
      CurrentMaxAge <- max(CurrentDF$Age)
      
      
      if(exists("GroupDF")) {
        
        # cat("Appending GroupDF", "\n")
        # cat("GroupDF has", nrow(GroupDF), "rows and CurrentDF has", nrow(CurrentDF), "\n")
        
        GroupDF <-  bind_rows(GroupDF, CurrentDF)

      } else {
        
        GroupDF <- CurrentDF
        
        # cat("GroupDF has", nrow(GroupDF), "rows and CurrentDF has", nrow(CurrentDF), "\n")
        
        # closes else to if(exists("GroupDF")) {
      }

      
    } else {
    
    for(j in 1:(nrow(CurrentGroup)-1)) {
      
 
      startpt = CurrentGroup$OriginalAge[j]
      endpt = CurrentGroup$OriginalAge[(j+1)]
      
      # cat("1 Start point is", startpt, "and end point is", endpt, "and number of rows are", nrow(CurrentGroup), "\n")
      
      # cat("WorkingAgeMin is", CurrentGroup$OriginalAge[j], "\n")
      
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
      
      # cat("4 Inflexion points are", mininflex, "and", maxinflex, "\n")
      
      #do the next bit by whole numbers
      minintval <- ceiling(mininflex)
      maxintval <- floor(maxinflex)
      
      # cat("5 Interpolation range is", minintval, "to", maxintval, "\n")
      
      sloperange <- maxinflex - mininflex
      
      # print(sloperange)
      
      slopeval <- valdiff/sloperange
      
      # cat("Slope is", slopeval, "\n")
      
      if(j ==(nrow(CurrentGroup)-1) & WorkingAgeMax > maxinflex) {
        
        # cat("Current def is", GroupInfo, "and minimum age is", WorkingAgeMin, "and maximum age is", WorkingAgeMax, "\n")
        # cat("maxinflex is", maxinflex, "and oldest age is", WorkingAgeMax, "\n")
        CurrentDF <- data.frame(Age = c(seq(from = minintval, to = WorkingAgeMax, by =1)))
        
      } else {
      
      CurrentDF <- data.frame(Age = c(seq(from = minintval, to = maxintval, by =1)))
      
      # closes else to  if(j ==(nrow(CurrentGroup)-1))
      } 
      
 
      CurrentDF <- CurrentDF %>%
        mutate(Fits = startval + (Age - mininflex) * slopeval)
      
      ExpandedGroup <- CurrentDef %>%
        slice_sample(n = nrow(CurrentDF), replace = TRUE)
      
      # cat("The number of rows of CurrentDF is", nrow(CurrentDF), "and the number of rows of ExpandedGroup is", nrow(ExpandedGroup), "\n")
      
      CurrentDF <- bind_cols(CurrentDF, ExpandedGroup)

      CurrentMinAge <- min(CurrentDF$Age)
      CurrentMaxAge <- max(CurrentDF$Age)
      
 
      if(exists("GroupDF")) {

        # cat("Appending GroupDF", "\n")
        # cat("GroupDF has", nrow(GroupDF), "rows and CurrentDF has", nrow(CurrentDF), "\n")

        GroupDF <-  bind_rows(GroupDF, CurrentDF)

        # cat("GroupDF now has", nrow(GroupDF), "rows", "\n")

      } else {

        GroupDF <- CurrentDF

        # cat("GroupDF has", nrow(GroupDF), "rows and CurrentDF has", nrow(CurrentDF), "\n")

        # closes else to if(exists("GroupDF")) {
      }
 
      # cat("Current def is", GroupInfo, "\n")
      
      # closes  for(j in 1:(nrow(CurrentGroup)-1))
    }

      # cat("After nrow loop, current def is", GroupInfo, "\n")
      # 
      # str(CurrentDef)
      # 
      # cat("The number of rows of GroupDF is", nrow(GroupDF), "\n")
      
      
      # closes else for if(NumRowsInCurrentGroup == 1)
    } 
    
  
    # closes for(i in 1:nrow(PeopleUnique))

  }
  
  # remove any duplicates that occur due to ages, rather than half ages, being used as edges
  # cat("De-duplicates started, data frame size is ", nrow(GroupDF), "\n")

  # removal of mid-points not working

  GroupDF <- GroupDF %>%
    group_by(Age,across(all_of(grpdef))) %>%
    filter(row_number() == 1) %>%
    ungroup()
  
  cat("Fitted values were interpolated for", i, "groups \n")
 
  # closes function

  return(GroupDF)

}