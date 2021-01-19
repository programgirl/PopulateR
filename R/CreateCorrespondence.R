#' Create a dataframe that has a correlation between two ordered factors.
#' This function outputs the same data frame, except a correlation has been constructed between two ordered factors. Due to the data structures of the two ordered factors, the desired correlation may not be output. Kendall's tau is used to test the correlation, as implemented in the stats package. Thus, Kendall's tau-b is used automatically when ties exist in the data.
#' Do not assign the function to an object, simply call the function without assignment.
#'
#' @export
#' @param InputDataframe A data frame containing the ordered factors to correlate.
#' @param GroupVariable The column number for the grouping variable. The variable values must not start with a number, as R objects cannot start with numbers. The funnction tests for the existance of a number at the start of any of these values and will break the function if one exists. This must be an ordered factor.
#' @param ToFixVariable The column number for the variable that requires correlation with the GroupVariable. This must be an ordered factor.
#' @param CorrelationNeeded The desired correlation between the GroupVariable and the ToFixVariable. This can be positive or negative.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param pValueToStop The primary stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param NumIterations The maximum number of iterations used to construct the coupled data frame. This has a default value of 1000000, and is the stopping rule if the algorithm does not converge.

CreateCorrespondence <- function(InputDataframe, GroupVariable = NULL, ToFixVariable = NULL, CorrelationNeeded = NULL, UserSeed = NULL, pValueToStop=NULL, NumIterations=1000000) {

  options(dplyr.summarise.inform=F)


  #####################################################################
  #####################################################################
  # Test for missing GroupVariable, stop function if this situation exists
  #####################################################################
  #####################################################################

  if (is.null(GroupVariable)) {
    stop("The grouping variable must be supplied.")
  }

  if (is.ordered(GroupVariable) == FALSE) {
    stop("The grouping variable must be an ordered factor.")
  }

  if (is.null(ToFixVariable)) {
    stop("The variable to fix must be supplied.")
  }

  if (is.null(ToFixVariable)) {
    stop("The variable to fix must be supplied.")
  }

  if (is.null(CorrelationNeeded)) {
    stop("The required correlation must be supplied.")
  }


  #####################################################################
  #####################################################################
  # Set up the data frame to sort
  #####################################################################
  #####################################################################

  WorkingDF <- InputDataframe %>%
    rename(GroupID = !! GroupVariable,
           ResortedVariable = !! ToFixVariable)

  #####################################################################
  #####################################################################
  # perform initial test
  #####################################################################
  #####################################################################

 ExistingCorrelation <- cor(WorkingDF$GroupID, WorkingDF$ResortedVariable)

}
