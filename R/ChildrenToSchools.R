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
#' @param ChildIDCol The column number for the ID variable in the Children data frame.
#' @param ChildAgeCol The column number for the Age variable in the Children data frame.
#' @param ChildSxCol The column number for the sex indicator for children. This column is used to assign children to the appropriate school type (co-educational or single-sex). The expected values are "F" (female) or "M" (male).
#' @param HouseholdIDCol The column number for the household variable in the Children data frame. This must be provided.
#' @param Schools A data frame containing the school observations.
#' @param SchoolIDCol The column number for the variable in the Schools data frame that contains the name of each school. While these IDs can be numeric, the data must be either character (preferred) or factor (will be converted to character).
#' @param SchoolAgeCol The column number for the Age variable in the Schools data frame. Each student age within the school must be a separate row.
#' @param SchoolRollCol The number of places available for children at that school age, within the school.
#' @param SchoolTypeCol An indicator variable used to determine whether the school is co-educational or single-sex. The expected values are "C" (co-educational), "F" (female only), and "M" (male-only).
#' @param ChildProb If one child is assigned to a same-sex school, the probability that another child in the household is also assigned to a same-sex school. If an eqivalent same-sex school is not available, the other child will be assigned to a co-ed school. The default value is 1, so that all children in the same household will be assigned to same-sex schools, or to co-educational schools. A probability of 0 means that, if one child is assigned to a same-sex school, all other children will be assigned to co-educational schools. The assignment is affected by the number of boy-only and girl-only schools, and the age distribution covered by these schools..
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.

ChildrenToSchools <- function(Children, ChildIDCol, ChildAgeCol, ChildSxCol, HouseholdIDCol = NULL,
                        Schools, SchoolIDCol, SchoolAgeCol, SchoolRollCol, SchoolTypeCol, ChildProb = 1, UserSeed=NULL)
{

  options(dplyr.summarise.inform=F)


  #####################################################################
  #####################################################################
  # Test for any problem ages, stop function if this situation exists
  #####################################################################
  #####################################################################

  ChildrenRenamed <- Children %>%
    rename(ChildID = !! ChildIDCol, ChildAge = !! ChildAgeCol, ChildType = !! ChildSxCol,
           HouseholdID = !! HouseholdIDCol)

  SchoolsRenamed <- Schools %>%
    rename(SchoolID = !! SchoolIDCol, SchoolAge = !! SchoolAgeCol,
           ChildCounts = !! SchoolRollCol, SchoolType = !! SchoolTypeCol) %>%
    mutate(SchoolID = as.character(SchoolID),
           SchoolType = as.character(SchoolType)) %>%
    #mutate(across(where(is.factor), as.character)) %>%
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


  # NOTE: this removes any school classrooms where NO children of that age exist in the data
  SchoolsRenamed <- left_join(AgeRestriction, SchoolsRenamed, by = c("ChildAge" = "SchoolAge"))

  SchoolsCountColIndex <- as.numeric(which(colnames(SchoolsRenamed) == "ChildCounts"))

  # get rid of the tibbles
  ChildrenRenamed <- as.data.frame(ChildrenRenamed)
  SchoolsRenamed <- as.data.frame(SchoolsRenamed)

  #####################################################################
  # Create household meta data data frame
  #####################################################################
  # get the number of households
  NumberHouseholds <- as.numeric(ChildrenRenamed %>%
                                   dplyr::summarise(Count = n_distinct(HouseholdID)) %>%
                                   pull(Count))

  # get list of household IDs
  HouseholdIDList <- as.data.frame(ChildrenRenamed %>%
                                     distinct(HouseholdID))

#####################################################################
# create counts by sex
# will be used to ensure that selected schools do not decrease
# the availability of
#####################################################################

  ChildrenSexAge <- ChildrenRenamed %>%
    group_by(ChildType, ChildAge) %>%
    summarise(AgeCount = n())

  SchoolSexAge <- SchoolsRenamed %>%
    group_by(SchoolType, ChildAge) %>%
    summarise(SchoolAgeCount = sum(ChildCounts))

  # singleton households, assign these last
  SingletonHouseholds <- ChildrenRenamed %>%
    group_by(HouseholdID) %>%
    summarise(ChildCount = n()) %>%
    filter(ChildCount == 1)

  MultipleChildrenHouseholds <- ChildrenRenamed %>%
    filter(!(HouseholdID %in% (SingletonHouseholds$HouseholdID)))

  # this is the first part of the code that requires randomness
  # so seed is applied here

  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  # work through the multiple child households first, if these exist

  if(nrow(MultipleChildrenHouseholds > 0)) {

  for(i in 1: nrow(MultipleChildrenHouseholds)) {


    # must delete PossibleSchools dataframe
    if(exists("PossibleSchools")) {
      rm(PossibleSchools)
    }

    if(exists("JoinToMerge")) {
      rm(JoinToMerge)
    }

    CurrentHousehold <- MultipleChildrenHouseholds$HouseholdID[i]

    # print(CurrentHousehold)

      # get the children in the household

      ChildrenInHousehold <- ChildrenRenamed %>%
        filter(HouseholdID == CurrentHousehold)

      # random roll to see if any children in same school, will prioritise the twins
      RandomRollVector <- runif(nrow(ChildrenInHousehold))

      # cat(RandomRollVector, "\n")

      # test number of children who should go to the same school
      NumberSameSchool <- data.frame(RandomRollVector) %>%
        filter(RandomRollVector > (1-ChildProb)) %>%
        summarise(SameSchool = n()) %>%
        pull(SameSchool)

      # cat(NumberSameSchool, "\n")

      # add children to same school

      if(NumberSameSchool > 1) {

        # need to identify the number of children that can go to the same school

        # get child ages vector
        ChildAges <- ChildrenInHousehold %>%
          group_by(ChildType, ChildAge) %>%
          summarise(CountsByAge = n())

        # locate schools that can take the maximum number of children from NumberSameSchool down
        for(j in 1:nrow(ChildAges)) {

          # cat("Current child age is", ChildAges$ChildAge[j], "and child sex is", ChildAges$ChildType[j],
          #     "and number children that age and sex is", ChildAges$CountsByAge[j], "\n")

          SchoolSubset <- SchoolsRenamed %>%
                          filter(ChildAge == ChildAges$ChildAge[j],
                                 SchoolType %in% c(ChildAges$ChildType[j], "C"),
                                 ChildCounts >= ChildAges$CountsByAge[j])

          if(exists("PossibleSchools")) {
            PossibleSchools <- bind_rows(PossibleSchools, SchoolSubset)
          } else {
            PossibleSchools <- SchoolSubset
          }

          # closes for j loop through selecting schools
        }

        # get the co-ed schools

        CoedSchoolsSelected <- PossibleSchools %>%
          filter(SchoolType == "C") %>%
          group_by(SchoolID) %>%
          summarise(across(ChildCounts, list(RollCountSum = sum, NumberTimes = ~n()))) %>%
          rename(RollCountSum = ChildCounts_RollCountSum, NumberTimes = ChildCounts_NumberTimes)

        SingleSexSchoolsSelected <- PossibleSchools %>%
          filter(!(SchoolType == "C")) %>%
          group_by(SchoolID) %>%
          summarise(across(ChildCounts, list(RollCountSum = sum, NumberTimes = ~n()))) %>%
          rename(RollCountSum = ChildCounts_RollCountSum, NumberTimes = ChildCounts_NumberTimes)

          # determine if the single sex schools have the equivalent age
        FemaleAgesToMatch <- PossibleSchools %>%
          filter(SchoolType == "F") %>%
          pull(ChildAge)

        MaleAgesToMatch <- PossibleSchools %>%
          filter(SchoolType == "M") %>%
          pull(ChildAge)

        MaleSchoolAvailable <- SchoolsRenamed %>%
          filter(SchoolID %in%  c(SingleSexSchoolsSelected$SchoolID),
                 SchoolType == "M",
                 ChildAge %in% c(FemaleAgesToMatch))

        FemaleSchoolAvailable <- SchoolsRenamed %>%
          filter(SchoolID %in%  c(SingleSexSchoolsSelected$SchoolID),
                 SchoolType == "F",
                 ChildAge %in% c(MaleAgesToMatch))

        if(nrow(MaleSchoolAvailable) > 0 & nrow(FemaleSchoolAvailable) > 0) {

          PulledSingleSexSchools <- bind_rows(MaleSchoolAvailable, FemaleSchoolAvailable) %>%
            mutate(SchoolType = ifelse(SchoolType == "M", "F", "M"))


          # merge the two sets of single-sex schools
          # on age




        if(CurrentHousehold == 1093) {
          return(PulledSingleSexSchools)
        }

          # closes test to see if there are matching same-sex schools
        }

        # if(CurrentHousehold == 672) {
        #   return(MaleSchoolAvailable)
        # }

          # AgeInSingleSexSchools <- PossibleSchools %>%
          #   filter(!(SchoolType == "C")) %>%
          #   pull(ChildAge)
          #
          # SingleSexAgesF <- PossibleSchools %>%
          #   filter(SchoolType == "F",
          #          ChildAge %in% c(AgeInSingleSexSchools))
          #
          # SingleSexAgesM <- PossibleSchools %>%
          #   filter(SchoolType == "M",
          #          ChildAge %in% c(AgeInSingleSexSchools))


        #  closes if number same school greater than one
      }

      # closes for multiple children households
  }

    # closes if multiple children household
  }

  # closes function
}
