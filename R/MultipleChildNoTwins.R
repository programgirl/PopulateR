MultipleChildNoTwins <- function(Children, ChildIDVariable, ChildAgeVariable, ChildSexVariable, HouseholdIDVariable = NULL,
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

  # identify the households containing twins
  # only matters for children of school age
  # children not of school age are screened out already

  LimitedToTwinsHouseholds <- ChildrenRenamed %>%
    group_by(HouseholdID, ChildAge) %>%
    summarise(CountsByAge = n(), .groups = "keep") %>%
    filter(CountsByAge >1)

  # add twin marker to HouseholdIDList
  HouseholdIDList <- HouseholdIDList %>%
    mutate(TwinMarker = ifelse(HouseholdID %in% LimitedToTwinsHouseholds$HouseholdID, "Y", "N"))

  # ensure that households containing twins are NOT in the bottom 10% of households

  TwinHouseholdSubset <- HouseholdIDList %>%
    filter(TwinMarker == "Y")

  NoTwinsHouseholdSubset <- HouseholdIDList %>%
    filter(TwinMarker == "N")

  # this is the first part of the code that requires randomness
  # so seed is applied here

  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  BottomNoTwins <- NoTwinsHouseholdSubset %>%
    slice_sample(n=(nrow(HouseholdIDList)*.1))

  TopNoTwins <- NoTwinsHouseholdSubset %>%
    filter(!(HouseholdID %in% BottomNoTwins$HouseholdID))

  InitialHouseholdRebind <- bind_rows(TwinHouseholdSubset, TopNoTwins)

  InitialHouseholdRebind <- InitialHouseholdRebind %>%
    slice_sample(n = nrow(InitialHouseholdRebind))

  HouseholdIDList <- bind_rows(InitialHouseholdRebind, BottomNoTwins)


  #####################################################################
  #####################################################################
  # end data preparation
  #####################################################################
  #####################################################################

  for (x in 1:148) {

    # TODO testing above for a subset of households, once working use the for below

    # for (x in 1:nrow(HouseholdIDList)) {

    WorkingChildren <- ChildrenRenamed %>%
      filter(HouseholdID %in% HouseholdIDList[x,1])

    # #####################################################################
    # #####################################################################
    # # match single child households
    # #####################################################################
    # #####################################################################
    #
    #
    if (nrow(WorkingChildren) > 2 & HouseholdIDList[x,2] == "N") {

      cat("Household is", WorkingChildren$HouseholdID, "\n")
      cat("Multi-child household with no twins", HouseholdIDList[x,1], "\n")

         # random sort the children
         WorkingChildren <- WorkingChildren %>%
           slice_sample(n = nrow(WorkingChildren))

         # assign the first child
         # this one is assigned independent of the others
         FirstChild <- WorkingChildren %>%
           slice_head(n=1)

         AvailableSchools <- SchoolsRenamed %>%
           filter(ChildAge == FirstChild$ChildAge,
                  SchoolType %in% c(FirstChild$ChildType, "C"),
                  ChildCounts > 0)

         SelectedSchool <- AvailableSchools %>%
           slice_sample(weight_by = ChildCounts, n = 1) %>%
           select(SchoolID, ChildAge, ChildCounts)

         SchoolMerged <- left_join(SelectedSchool, FirstChild, by = "ChildAge")

         SchoolCountDecreases <- SchoolMerged %>%
           mutate(FinalCounts = ChildCounts - 1) %>%
           ungroup() %>%
           distinct() %>%
           select(-ChildCounts) %>%
           rename(ChildCounts = FinalCounts)

         SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID &
                                              SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge))

         SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts

         SchoolList <- as.vector(SelectedSchool$SchoolID)

         if (exists("FinalMatchedChildren")) {

           FinalMatchedChildren <- bind_rows(FinalMatchedChildren, SchoolMerged)

         } else {


           FinalMatchedChildren <- SchoolMerged

           # closes if statement for existence of FinalMatchedChildren
         }

         WorkingChildren <- WorkingChildren %>%
           filter(WorkingChildren$ChildID != FirstChild$ChildID)

         # loop through the other children
         while (nrow(WorkingChildren) > 0) {

           CurrentChild <- WorkingChildren[1,]

           RandomRollResult <- runif(1, 0, 1)

            AvailableSchools <- SchoolsRenamed %>%
             filter(ChildAge == CurrentChild$ChildAge,
                    SchoolType %in% c(CurrentChild$ChildType, "C"),
                    ChildCounts > 0)

            if (RandomRollResult <= ChildProb) {

              SelectedSchool <- AvailableSchools %>%
                filter(SchoolID %in% SchoolList)

              # fix the problem if there the classroom count is 0
              if (is.na(SelectedSchool$SchoolType[1]) == TRUE) {

                SelectedSchool <- AvailableSchools %>%
                  slice_sample(weight_by = ChildCounts, n = 1) %>%
                  select(SchoolID, ChildAge, ChildCounts)

                # closes if fix for putting in a school replacement where no classroom space available
              }


            } else {
              # ensure that match is to a school not in the school list, as the child cannot be allocated to the same school as another one in the family

              AvailableSchools <- SchoolsRenamed %>%
                filter(ChildAge == CurrentChild$ChildAge,
                       SchoolType %in% c(CurrentChild$ChildType, "C"),
                       ChildCounts > 0,
                       !(SchoolID %in% c(SchoolList)))


              SelectedSchool <- AvailableSchools %>%
                slice_sample(weight_by = ChildCounts, n = 1) %>%
                select(SchoolID, ChildAge, ChildCounts)

              # closes if for school match using the rolled probability
            }

            SchoolMerged <- left_join(SelectedSchool, CurrentChild, by = "ChildAge")

            SchoolCountDecreases <- SchoolMerged %>%
              mutate(FinalCounts = ChildCounts - 1) %>%
              ungroup() %>%
              distinct() %>%
              select(-ChildCounts) %>%
              rename(ChildCounts = FinalCounts)

            SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID &
                                                 SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge))

            SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts

            SchoolList <- c(SchoolList, SchoolMerged$ID)

            FinalMatchedChildren <- bind_rows(FinalMatchedChildren, SchoolMerged)

            # remove the matched children from the working dataframe (i.e. from those still to be matched)
            WorkingChildren <- WorkingChildren %>%
              filter(!(ChildID %in%  FinalMatchedChildren$ChildID))


         } # close while loop that circles through the n-1 children in the multiple-child-and-no-twins household


    } # closes if loop for multi-child household that does not contain twins


  } # closes for loop through the households

  return(FinalMatchedChildren)


} # closes function
