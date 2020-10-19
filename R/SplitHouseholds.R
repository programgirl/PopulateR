#' Create a set of dataframes that split out household groups
#' This function creates n dataframes of separate household groups, where n is the number of households to split from the input dataframe.
#' A single variable is required, to identify the groups. This can take any type, for example factors are acceptable as grouping variables.
#' Each output dataframe will have a name derived from the grouping variable.
#' Do not assign the function to an object, simply call the function without assignment.
#'
#' @export
#' @param AggregateDF A data frame containing all the household groups.
#' @param GroupVariable The column number for the grouping variable. The variable values must not start with a number, as R objects cannot start with numbers. The funnction tests for the existance of a number at the start of any of these values and will break the function if one exists.

SplitHouseholds <- function(AggregateDF, GroupVariable = NULL) {

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
    mutate(GroupID = gsub("[[:space:]]", "", as.character(GroupID)))


  if (any(grepl("^[[:digit:]]+", WorkingDF$GroupID))) {

    stop("The grouping variable values must not start with a number.")

  }

  Groups <- split(WorkingDF, WorkingDF$GroupID)


 list2env(Groups, .GlobalEnv)


}
