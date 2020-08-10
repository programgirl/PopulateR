#' Create a data frame of children matched to schools
#' This function creates a data frame of children, with a school allocated to them, based on the characteristics of the child ages in a household (e.g. the presence of twins) and the proportions of children of each age within each school.
#' Two data frames are required. The Children data frame contains the children data, to which the Schools data will be applied.
#' No minimum or maximum child ages are required, as the function limits the ages within the age range across the schools Thus, pre-cleaning the Children data frame is not required.
#' The Schools data frame must be a summary in the form of counts by age within school. Each row is one age only. For example, if a school has children aged 5 to 9 years, there should be 5 rows. #' Co-educational, single-sex, or both types of schools can be included in the Schools data frame. If at least one same-sex school is included, similarly aged children of the opposite sex will be assigned to the equivalent school for the opposite sex, if present, otherwise they will be assigned to a co-educational school. Because of the semi-summary nature of the Schools data frame, this information must be repeated for each row.
#' Children within a family will be assigned to the same school, where the age and/or sex structure requires this. It is possible that some children may be matched to a school with no available slots for that age, resulting in an overcount of children at that age in the school. While the code attempts to prevent this, this outcome may still occur. In this situation, a message will be printed to the console, identifying the problem household. Testing has shown that use of a different set.seed() may remove the problem. Alternatively, a manual fix can be performed after using this function.

#' The function performs a reasonableness check for child ID, child age, and counts for the number of children of each age versus the number of available age slots across all schools..
#'
#' @export
#' @param Children A data frame containing observations limited to the children to be matched An age column is required. All children in this data frame will be matched to a parent/guardian.
#' @param ChildIDVariable The column number for the ID variable in the Children data frame.
#' @param ChildAgeVariable The column number for the Age variable in the Children data frame.
#' @param ChildSexVariable The column number for the sex indicator for children. This column is used to assign children to the appropriate school type (co-educational or single-sex). The expected values are "F" (female) or "M" (male).
#' @param HouseholdIDVariable The column number for the household variable in the Children data frame. This must be provided.
#' @param Schools A data frame containing the school observations.
#' @param SchoolNameVariable The column number for the variable in the Schools data frame that contains the name of each school.
#' @param SchoolAgeVariable The column number for the Age variable in the Schools data frame. Each student age within the school must be a separate row.
#' @param SchoolRollCount The number of places available for children at that school age, within the school.
#' @param SchoolCoEdStatus An indicator variable used to determine whether the school is co-educational or single-sex. The expected values are "C" (co-educational), "F" (female only), and "M" (male-only).

#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.

AddSchools <- function(Children, ChildIDVariable, ChildAgeVariable, ChildSexVariable, HouseholdIDVariable = NULL,
                       Schools, SchoolIDVariable, SchoolAgeVariable, SchoolRollCount, SchoolCoEdStatus, UserSeed=NULL)
  {

  options(dplyr.summarise.inform=F)


  #####################################################################
  #####################################################################
  # Set up variables for use
  #####################################################################
  #####################################################################

  ChildrenRenamed <- Children %>%
    rename(ChildID = !! ChildIDVariable, ChildAge = !! ChildAgeVariable, ChildSex = !! ChildSexVariable,
           HouseholdID = !! HouseholdIDVariable)


  SchoolsRenamed <- Schools %>%
    rename(SchoolID = !! SchoolIDVariable, SchoolAge = !! SchoolAgeVariable,
           ChildCounts = !! SchoolRollCount, SchoolType = !! SchoolCoEdStatus)


  if(nrow(Children) > sum(SchoolsRenamed$ChildCounts)) {
    stop("The total number of children exceeds the total number of the student roll count.")
  }

  ChildrenCountTest <- ChildrenRenamed %>%
    group_by(ChildAge) %>%
    summarise(AgeCount = n())

  SchoolsCountTest <- SchoolsRenamed %>%


  return(ChildrenCountTest)

}
