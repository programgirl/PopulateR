#' Create a set of dataframes that split out household groups
#' This function creates n dataframes of separate household groups, where n is the number of households to split from the input dataframe.
#' A single variable is required, to identify the groups.
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
  # Test for any problem ages, stop function if this situation exists
  #####################################################################
  #####################################################################

  ChildrenRenamed <- Children %>%
    rename(ChildID = !! ChildIDVariable, ChildAge = !! ChildAgeVariable, ChildSex = !! ChildSexVariable,
           HouseholdID = !! HouseholdIDVariable)


  SchoolsRenamed <- Schools %>%
    rename(SchoolID = !! SchoolIDVariable, SchoolAge = !! SchoolAgeVariable,
           ChildCounts = !! SchoolRollCount, SchoolType = !! SchoolCoEdStatus) %>%
    mutate_if(is.factor, as.character)

  ChildrenCountTest <- ChildrenRenamed %>%
    group_by(ChildAge) %>%
    summarise(AgeCount = n())

  SchoolsCountTest <- SchoolsRenamed %>%
    group_by(SchoolAge) %>%
    summarise(SchoolAgeCount = sum(ChildCounts))

  CountComparison <- full_join(ChildrenCountTest, SchoolsCountTest, by = c("ChildAge" = "SchoolAge")) %>%
    mutate(AgeCount = replace(AgeCount, is.na(AgeCount), 0),
           SchoolAgeCount = replace(SchoolAgeCount, is.na(SchoolAgeCount), 0),
           CountDiff = SchoolAgeCount - AgeCount) %>%
    filter(SchoolAgeCount != 0, AgeCount != 0)

  TooManyKids <- CountComparison %>%
    filter(CountDiff < 0) %>%
    select(ChildAge)

  # TooManyKids <- as_tibble(CountComparison$ChildAge) # testing if loop


  if (!(nrow(TooManyKids)==0)) {

    TooManyKids <- as.vector(TooManyKids)

    stop(paste("The number of children at these ages exceeds the available school roll places: ", shQuote(TooManyKids), collapse=", "))

  }

  MaxSchoolAge <- as.numeric(CountComparison[nrow(CountComparison), 1])

  cat("The minimum school age is", as.numeric(CountComparison[1,1]), "and the maximum school age is ", as.numeric(CountComparison[nrow(CountComparison), 1]), "\n")


  # restrict child and school data frames to these minimum and maximum ages
  # get age range
  AgeRestriction <- CountComparison %>%
    filter(CountDiff >= 0) %>%
    select(ChildAge)

  # apply to children

  ChildrenRenamed <- left_join(AgeRestriction, ChildrenRenamed, by = "ChildAge")

  SchoolsRenamed <- left_join(AgeRestriction, SchoolsRenamed, by = c("ChildAge" = "SchoolAge"))


  SchoolsCountColIndex <- as.numeric(which(colnames(SchoolsRenamed) == "ChildCounts"))


  #####################################################################
  #####################################################################
  # Create a separate vector of age counts for each school
  #####################################################################
  #####################################################################

  # convert the schools file from long to wide ahead of vector construction

  # SchoolsLong <- reshape(SchoolsRenamed, idvar = c("SchoolID", "SchoolType"), v.names = ("ChildAge"), ids = ("ChildCounts"), direction = "wide")
  #
  #   SchoolsWide <- SchoolsRenamed %>%
  #     select(SchoolID, ChildAge, ChildCounts, SchoolType) %>%
  #    tidyr::pivot_wider(names_from = ChildAge, values_from = ChildCounts)

  # get the number of households
  NumberHouseholds <- as.numeric(ChildrenRenamed %>%
                                   dplyr::summarise(Count = n_distinct(HouseholdID)) %>%
                                   pull(Count))

  # get list of household IDs
  HouseholdIDList <- as.data.frame(ChildrenRenamed %>%
                                     distinct(HouseholdID))

  # testing on household 1914 as this contains primary and secondary school children.
  # WorkingChildren <- ChildrenRenamed %>%
  #   filter(HouseholdID == HouseholdIDList[30,1])

  # single household test worked, expand to entire child data frame
  for (x in 1:NumberHouseholds) {

    WorkingChildren <- ChildrenRenamed %>%
      filter(HouseholdID == HouseholdIDList[x,1])


    # TODO use this to identify twins. Not yet implemented. Needs to work with while loop below.
    # get child age counts for each age in DF, if twins then count == 2 rather than 1
    #
    #   WorkingChildrenAgeCounts <- WorkingChildren %>%
    #     select(ChildAge) %>%
    #     group_by(ChildAge) %>%
    #     summarise(AgeCount = n())

    while (!(is.na(WorkingChildren$ChildID[1])) == TRUE) {

      # cat(WorkingChildren$ChildID[1], "\n")

      SchoolMatches <- left_join(WorkingChildren, SchoolsRenamed, by = "ChildAge") %>%
        filter(ChildCounts != 0)

      # identify schools that exist multiple times from the join
      # this is the selection for the most number of children in the household to one school

      NumberTimesSchoolSelected <- SchoolMatches %>%
        select(SchoolID, ChildAge) %>%
        group_by(SchoolID) %>%
        summarise(TimesSelected = n())

      # create subset of schools limited to those of the maximum number, and loop
      # extract out first set of schools to match

      MaxSchoolDuplicates <- max(NumberTimesSchoolSelected$TimesSelected)

      FirstSetSchools <- NumberTimesSchoolSelected %>%
        filter(TimesSelected == MaxSchoolDuplicates) %>%
        select(-TimesSelected)

      # create constrained data frame based off SchoolMatches
      RestrictedSchoolMatches <- left_join(FirstSetSchools, SchoolMatches, by = "SchoolID")

      FinalSchoolSelected <- RestrictedSchoolMatches %>%
        slice_max(ChildCounts, n = 1, with_ties = FALSE) %>%
        select(SchoolID)

      FinalSchoolMerged <- left_join(FinalSchoolSelected, SchoolMatches, by = "SchoolID") %>%
        select(ChildID, SchoolID)

      SchoolCountDecreases <- left_join(FinalSchoolSelected, SchoolMatches, by = "SchoolID") %>%
        group_by(SchoolID, ChildAge) %>%
        mutate(FinalCounts = ChildCounts - n()) %>%
        ungroup() %>%
        distinct() %>%
        select(-ChildCounts) %>%
        rename(ChildCounts = FinalCounts)


      for (a in 1:nrow(SchoolCountDecreases)) {

        SchoolRowIndex <- as.numeric(which((SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID[a]) &
                                             (SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge[a])))

        SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts[a]

      }


      # create child data frame with school joined

      CurrentMatchedChildren <- left_join(FinalSchoolMerged, WorkingChildren, by = "ChildID")

      if (exists("FinalMatchedChildren")) {

        FinalMatchedChildren <- bind_rows(FinalMatchedChildren, CurrentMatchedChildren)

      } else {

        FinalMatchedChildren <- CurrentMatchedChildren

        # closes if statement
      }

      WorkingChildren <- WorkingChildren %>%
        filter(!(ChildID %in%  FinalMatchedChildren$ChildID))

      #closes while loop
    }

    # closes x loop
  }

  # # split out the households
  # for (x in 1:NumberHouseholds) {
  #
  #   WorkingChildren <- ChildrenRenamed %>%
  #     filter(HouseholdID == HouseholdIDList[x,1])

  # get child age counts for each age in DF, if twins then count == 2 rather than 1

  # WorkingChildrenAgeCounts <- WorkingChildren %>%
  #   select(ChildAge) %>%
  #   group_by(ChildAge) %>%
  #   summarise(AgeCount = n())
  #
  #   if (nrow(WorkingChildren > 1)) {
  #
  #   # match in relevant schools
  #   for (y in 1:nrow(WorkingChildren)) {
  #
  #     SchoolMatches <- left_join(WorkingChildren, SchoolsRenamed, by = "ChildAge")
  #
  #     #closes for y statement
  #   }
  #
  #     # if nrow(Children) else statements below
  #   } else { # this is for households with only one child
  #
  #     #closes else statement
  #   }
  #
  #
  #   # closes for x statement
  # }



  return(FinalMatchedChildren)

}
