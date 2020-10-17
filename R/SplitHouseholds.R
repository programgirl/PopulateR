#' Create a set of dataframes that split out household groups
#' This function creates n dataframes of separate household groups, where n is the number of households to split from the input dataframe.
#' A single variable is required, to identify the groups. This can take any type, for example factors are acceptable as grouping variables.
#' Each output dataframe will have a name derived from the grouping variable.
#'
#' @export
#' @param AggregateDF A data frame containing all the household groups.
#' @param GroupVariable The column number for the grouping variable.

SplitHouseholds <- function(AggregateDF, GroupVariable = NULL)
{

  options(dplyr.summarise.inform=F)


  #####################################################################
  #####################################################################
  # Test for missing GroupVariable, stop function if this situation exists
  #####################################################################
  #####################################################################

  if (is.null(GroupVariable)) {
    stop("The grouping variable must be supplied.")
  }

  #####################################################################
  #####################################################################
  # Do the split
  #####################################################################
  #####################################################################

  WorkingDF <- AggregateDF %>%
    rename(GroupID = !! GroupVariable) %>%
    mutate(GroupID = as.character(GroupID))

  NumberGroups <- WorkingDF %>%
    group_by(GroupID) %>%
    filter(row_number() == 1) %>%
    select(GroupID) #%>%
 #   mutate(GroupID = as.character(GroupID))

  # for(a in 1:nrow(NumberGroups)) {
  #
  #   CurrentGroup <- NumberGroups$GroupID[a]
  #
  #   CurrentHousehold <- WorkingDF %>%
  #     filter(as.character(GroupID)==CurrentGroup$GroupID)
  #
  #   if (exists("HouseholdList")) {
  #
  #
  #   }
  # }


  return(NumberGroups)

}
