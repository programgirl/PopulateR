#' Create a data frame of children matched to schools
#' This function creates a data frame of children, with a school allocated to them, based on the characteristics of the child ages in a household (e.g. the presence of twins) and the proportions of children of each age within each school.
#' Two data frames are required. The Children data frame contains the children data, to which the Schools data will be applied.
#' No minimum or maximum child ages are required, as the function limits the ages within the age range across the schools Thus, pre-cleaning the Children data frame is not required.
#' The Schools data frame must be a summary in the form of counts by age within school. Each row is one age only. For example, if a school has children aged 5 to 9 years, there should be 5 rows. #' Co-educational, single-sex, or both types of schools can be included in the Schools data frame. If at least one same-sex school is included, similarly aged children of the opposite sex will be assigned to the equivalent school for the opposite sex, if present, otherwise they will be assigned to a co-educational school. Because of the semi-summary nature of the Schools data frame, this information must be repeated for each row.
#' Children within a family will be assigned to the same school, where the age and/or sex structure requires this, using the probability supplied by the user. Twins are the exception to this rule. Twins and other multiples will be allocated to the same school with probability 1, except when the structure of single-sex schools prevents this. If there are single-sex schools for each twin/multiple, and one child is randomly allocated to a single-sex school, then all those children will be assigned to a single-sex school assuming that is available.The allocation is robust for the number of children in a household who are the same age.
#' The function performs a reasonableness check for child ID, child age, and counts for the number of children of each age versus the number of available age slots across all schools.
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
#' @param ChildProb If one child is assigned to a same-sex school, the probability that another child in the household is also assigned to a same-sex school. If an eqivalent same-sex school is not available, the other child will be assigned to a co-ed school. The default value is 1, so that all children in the same household will be assigned to same-sex schools, or to co-educational schools. A probability of 0 means that, if one child is assigned to a same-sex school, all other children will be assigned to co-educational schools. The assignment is affected by the number of boy-only and girl-only schools, and the age distribution covered by these schools..
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.

ThirdTimesACharm <- function(Children, ChildIDVariable, ChildAgeVariable, ChildSexVariable, HouseholdIDVariable = NULL,
                                  Schools, SchoolIDVariable, SchoolAgeVariable, SchoolRollCount, SchoolCoEdStatus, ChildProb = 1, UserSeed=NULL)
{

  options(dplyr.summarise.inform=F)


  #####################################################################
  #####################################################################
  # Test for any problem ages, stop function if this situation exists
  #####################################################################
  #####################################################################

  ChildrenRenamed <- Children %>%
    rename(ChildID = !! ChildIDVariable, ChildAge = !! ChildAgeVariable, ChildType = !! ChildSexVariable,
           HouseholdID = !! HouseholdIDVariable)


  SchoolsRenamed <- Schools %>%
    rename(SchoolID = !! SchoolIDVariable, SchoolAge = !! SchoolAgeVariable,
           ChildCounts = !! SchoolRollCount, SchoolType = !! SchoolCoEdStatus) %>%
    mutate_if(is.factor, as.character) %>%
    select(SchoolID, SchoolAge, ChildCounts, SchoolType)

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

  # get rid of the tibbles
  ChildrenRenamed <- as.data.frame(ChildrenRenamed)
  SchoolsRenamed <- as.data.frame(SchoolsRenamed)

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

  # testing on household 1114 as this a mix of ages and sexes.
  # TODO need a for loop in here for 1:nrow(HouseholdIDList)

  # random number generator to see if there is a same sex assignment
  # seed must come before first sample is cut
  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  # set the probabilities for same-sex matching to schools
  # this creates the probability against which the rolled probability will be tested
  # hence the need for the range to be 0 through 1.


  for (x in 1:nrow(HouseholdIDList)) {

    WorkingChildren <- ChildrenRenamed %>%
    filter(HouseholdID %in% HouseholdIDList[x,1])

    #####################################################################
    #####################################################################
    # match single child households
    #####################################################################
    #####################################################################


  if (nrow(WorkingChildren) == 1) {

    Child <- WorkingChildren

    AvailableSchools <- SchoolsRenamed %>%
      filter(ChildAge == Child$ChildAge,
             SchoolType %in% c(Child$ChildType, "C"),
             ChildCounts > 0)

     SelectedSchool <- AvailableSchools %>%
       slice_sample(weight_by = ChildCounts, n = 1) %>%
       select(SchoolID, ChildAge, ChildCounts)

     SchoolMerged <- left_join(SelectedSchool, Child, by = "ChildAge")

     SchoolCountDecreases <- SchoolMerged %>%
       mutate(FinalCounts = ChildCounts - n()) %>%
       ungroup() %>%
       distinct() %>%
       select(-ChildCounts) %>%
       rename(ChildCounts = FinalCounts)

       SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID &
                                            SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge))

       SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts

       # add matched children to the output dataframe
         if (exists("FinalMatchedChildren")) {

         FinalMatchedChildren <- bind_rows(FinalMatchedChildren, SchoolMerged)

         } else {


           FinalMatchedChildren <- SchoolMerged

       # closes if statement for existance of FinalMatchedChildren
         }

       # remove matched children from the working dataframe (i.e. from those still to be matched)
       WorkingChildren <- WorkingChildren %>%
         filter(!(ChildID %in%  FinalMatchedChildren$ChildID))


  } else {

    #####################################################################
    #####################################################################
    # matching multiple-child households
    #####################################################################
    #####################################################################

 #   cat("The household with more than 1 child is", WorkingChildren$HouseholdID, "\n")

    # TODO look at whether twin probabilties need to be addressed
    # current assumption is that twins are assigned to the same school with P == 1

    # randomise order of children within the household
    # this removes any systematic bias in child ordering

    WorkingChildren <- WorkingChildren %>%
      slice(sample(1:n()))

    # get school list match for all children in the household
     SchoolMatches <- left_join(WorkingChildren, SchoolsRenamed, by = "ChildAge") %>%
      filter(ChildCounts > 0, SchoolType %in% c(ChildType, "C"))

    # get twin ages, will only be non-empty if twins present
    TwinsAge <- WorkingChildren %>%
      group_by(ChildAge) %>%
      summarise(Count = n()) %>%
      filter(Count >1) %>%
      pull(ChildAge)

    cat("Twin age/s is/are ", TwinsAge, "and Household ID is ", mean(WorkingChildren$HouseholdID), "\n")


    # note at this point, match on sex hasn't been made
    # start the matching

    # do a random draw from the list of schools, based on classroom size
    # must ensure that sex is matched
    FirstChild <- SchoolMatches %>%
      slice_sample(weight_by = ChildCounts, n = 1)

    # # make sure that the first child is in a correctly matched same-sex or co-ed school
    # while (!FirstChild$SchoolType %in% c(FirstChild$ChildType, "C")) {
    #
    #   FirstChild <- SchoolMatches %>%
    #     slice_sample(weight_by = ChildCounts, n = 1)
    # }

    RemainingChildren <- SchoolMatches %>%
      filter(!(ChildID == FirstChild$ChildID))

    HouseholdMatchedChildren <- FirstChild

    for (y in 1:nrow(RemainingChildren)) {

      NextChild <- RemainingChildren[y,]

      # check if twin

      if (mean(NextChild$ChildAge) %in% c(TwinsAge)) {
        cat("Household ID is ", mean(NextChild$HouseholdID), "Child ID is", mean(NextChild$ChildID), "Child age is ", mean(NextChild$ChildAge), "\n")
   #   if (NextChild$ChildAge %in% HouseholdMatchedChildren$ChildAge) {
      #
      #   # assign same school if possible
      #   # next to test if the already assigned school is same-sex and the other twin can go to that same-sex school
      #   # first, get the school already used
      #
      #   TwinSchoolAlreadyMatched <- HouseholdMatchedChildren %>%
      #     filter(ChildAge == NextChild$ChildAge) %>%
      #     select(SchoolID, ChildCounts, SchoolType)
      #
      #
      #   # assign same-sex twinScho
      #   SchoolMatched <- NextChild %>%
      #     filter(SchoolID )
      #
      #   SchoolToMatch <- NextChild %>%
      #     filter(ChildAge == NextChild$ChildAge)
      #
      #   #closes if for twins test
      }


      #closes for y loop that cycles through the remaining multiple children
    }

    #

    # for (y in 1:nrow(SchoolMatches)) {
    #
    # Child <- WorkingChildren %>%
    #   filter(row_number() == y)
    #
    # cat( "ChildID is ", Child$ChildID, "Household ID is ", Child$HouseholdID, "\n")
    #
    # # ends y loop through the multi-child working children subset
    # }

    # closes else when there are >1 child per family
  }

  # closes for x loop through household ID list
  }



  return(SchoolMatches)

}
