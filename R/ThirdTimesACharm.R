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

   #  #####################################################################
   #  #####################################################################
   #  # match single child households
   #  #####################################################################
   #  #####################################################################
   #
   #
   #  if (nrow(WorkingChildren) == 1) {
   #
   # #   cat("Household is", WorkingChildren$HouseholdID, "\n")
   #
   #    Child <- WorkingChildren
   #
   #    AvailableSchools <- SchoolsRenamed %>%
   #      filter(ChildAge == Child$ChildAge,
   #             SchoolType %in% c(Child$ChildType, "C"),
   #             ChildCounts > 0)
   #
   #    SelectedSchool <- AvailableSchools %>%
   #      slice_sample(weight_by = ChildCounts, n = 1) %>%
   #      select(SchoolID, ChildAge, ChildCounts)
   #
   #    SchoolMerged <- left_join(SelectedSchool, Child, by = "ChildAge")
   #
   #    SchoolCountDecreases <- SchoolMerged %>%
   #      mutate(FinalCounts = ChildCounts - 1) %>%
   #      ungroup() %>%
   #      distinct() %>%
   #      select(-ChildCounts) %>%
   #      rename(ChildCounts = FinalCounts)
   #
   #    SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID &
   #                                         SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge))
   #
   #    SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts
   #
   #    # add matched children to the output dataframe
   #    if (exists("FinalMatchedChildren")) {
   #
   #      FinalMatchedChildren <- bind_rows(FinalMatchedChildren, SchoolMerged)
   #
   #    } else {
   #
   #
   #      FinalMatchedChildren <- SchoolMerged
   #
   #      # closes if statement for existence of FinalMatchedChildren
   #    }
   #
   #    # closes loop for single-child households
   #    # NOTE: NOT running a series of if-else as there is no evidence this is quicker
   #    # also, there are two elses, and both differ on one variable
   #    # which would mean an if else nested under an if else
   #    # more readable just to use separate if statements, too
   #    # finally, makes code testing a breeze as an entire if statement can be commented out
   #    # wonders if anyone other than me bothers to read the code comments
   #  }
   #
   #  #####################################################################
   #  #####################################################################
   #  # match multi-child households NO TWINS
   #  #####################################################################
   #  #####################################################################
   #
   #  if (nrow(WorkingChildren) > 1 & HouseholdIDList[x,2] == "N") {
   #
   #    # cat("Multi-child household with no twins", HouseholdIDList[x,1], "\n")
   #
   #    # random sort the children
   #    WorkingChildren <- WorkingChildren %>%
   #      slice_sample(n = nrow(WorkingChildren))
   #
   #    # assign the first child
   #    # this one is assigned independent of the others
   #    FirstChild <- WorkingChildren %>%
   #      slice_head(n=1)
   #
   #    AvailableSchools <- SchoolsRenamed %>%
   #      filter(ChildAge == FirstChild$ChildAge,
   #             SchoolType %in% c(FirstChild$ChildType, "C"),
   #             ChildCounts > 0)
   #
   #    SelectedSchool <- AvailableSchools %>%
   #      slice_sample(weight_by = ChildCounts, n = 1) %>%
   #      select(SchoolID, ChildAge, ChildCounts)
   #
   #    SchoolMerged <- left_join(SelectedSchool, FirstChild, by = "ChildAge")
   #
   #    SchoolCountDecreases <- SchoolMerged %>%
   #      mutate(FinalCounts = ChildCounts - 1) %>%
   #      ungroup() %>%
   #      distinct() %>%
   #      select(-ChildCounts) %>%
   #      rename(ChildCounts = FinalCounts)
   #
   #    SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID &
   #                                         SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge))
   #
   #    SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts
   #
   #    SchoolList <- as.vector(SelectedSchool$SchoolID)
   #
   #    if (exists("FinalMatchedChildren")) {
   #
   #      FinalMatchedChildren <- bind_rows(FinalMatchedChildren, SchoolMerged)
   #
   #    } else {
   #
   #
   #      FinalMatchedChildren <- SchoolMerged
   #
   #      # closes if statement for existence of FinalMatchedChildren
   #    }
   #
   #    WorkingChildren <- WorkingChildren %>%
   #      filter(WorkingChildren$ChildID != FirstChild$ChildID)
   #
   #    # loop through the other children
   #    for (y in 1:nrow(WorkingChildren)) {
   #
   #      CurrentChild <- WorkingChildren[y,]
   #
   #      RandomRollResult <- runif(1, 0, 1)
   #
   #       AvailableSchools <- SchoolsRenamed %>%
   #        filter(ChildAge == CurrentChild$ChildAge,
   #               SchoolType %in% c(CurrentChild$ChildType, "C"),
   #               ChildCounts > 0)
   #
   #       if (RandomRollResult <= ChildProb) {
   #
   #         SelectedSchool <- AvailableSchools %>%
   #           filter(SchoolID %in% SchoolList)
   #
   #         # fix the problem if there the classroom count is 0
   #         if (is.na(SelectedSchool$SchoolType[1]) == TRUE) {
   #
   #           SelectedSchool <- AvailableSchools %>%
   #             slice_sample(weight_by = ChildCounts, n = 1) %>%
   #             select(SchoolID, ChildAge, ChildCounts)
   #
   #           # closes if fix for putting in a school replacement where no classroom space available
   #         }
   #
   #
   #       } else {
   #         # ensure that match is to a school not in the school list, as the child cannot be allocated to the same school as another one in the family
   #
   #         AvailableSchools <- SchoolsRenamed %>%
   #           filter(ChildAge == CurrentChild$ChildAge,
   #                  SchoolType %in% c(CurrentChild$ChildType, "C"),
   #                  ChildCounts > 0,
   #                  !(SchoolID %in% c(SchoolList)))
   #
   #
   #         SelectedSchool <- AvailableSchools %>%
   #           slice_sample(weight_by = ChildCounts, n = 1) %>%
   #           select(SchoolID, ChildAge, ChildCounts)
   #
   #         # closes if for school match using the rolled probability
   #       }
   #
   #       SchoolMerged <- left_join(SelectedSchool, CurrentChild, by = "ChildAge")
   #
   #       SchoolCountDecreases <- SchoolMerged %>%
   #         mutate(FinalCounts = ChildCounts - 1) %>%
   #         ungroup() %>%
   #         distinct() %>%
   #         select(-ChildCounts) %>%
   #         rename(ChildCounts = FinalCounts)
   #
   #       SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID &
   #                                            SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge))
   #
   #       SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts
   #
   #       SchoolList <- c(SchoolList, SchoolMerged$ID)
   #
   #       FinalMatchedChildren <- bind_rows(FinalMatchedChildren, SchoolMerged)
   #
   #       # remove the matched children from the working dataframe (i.e. from those still to be matched)
   #       # WorkingChildren <- WorkingChildren %>%
   #       #   filter(!(ChildID %in%  FinalMatchedChildren$ChildID))
   #
   #      # close for loop that circles through the n-1 children in the multiple-child-and-no-twins household
   #    }
   #
   #    # closes if loop for multi-child household that does not contain twins
   #  }


    #####################################################################
    #####################################################################
    # match multi-child households WITH TWINS
    #####################################################################
    #####################################################################

    if (HouseholdIDList[x,2] == "Y") {

 #     cat("Multi-child household with twins", HouseholdIDList[x,1], "\n")
      # need a loop through the working children here
      # loop is: UNTIL THERE ARE NO WORKING CHILDREN
      # cannot loop through 1:nrow because of the way the children are being handled
      # twins represent two childrne removed, not one

      WorkingChildren <- WorkingChildren %>%
        slice_sample(n = nrow(WorkingChildren))

      # get the age of the twins
      TwinsAges <- WorkingChildren %>%
        group_by(ChildAge) %>%
        summarise(Twins = n()) %>%
        filter(Twins > 1)

      return(TwinsAges)

      # NOTE: could be more than one set of multiple births in the same household
      # loop through the TwinsAges subset
      # look to remove this and simply loop through the children
      # this means that the twins age structure will be reconstructed each time
      for (t in 1: nrow(TwinsAges)) {

        # cat("Multi-child household with twins", HouseholdIDList[x,1], "\n")

      TwinsSubset <- WorkingChildren %>%
        filter(ChildAge %in% TwinsAges$ChildAge)

      # get number of children for each sex

      TwinsPyramid <- TwinsSubset %>%
        group_by(ChildType) %>%
        summarise(TypeCount = n())

      # randomise order of twins
      TwinsSubset <- TwinsSubset %>%
        slice_sample(n=nrow(TwinsSubset))

      # draw first twin
      FirstTwin <- TwinsSubset %>%
          slice_head(n=1)

 #      cat("Household is ", FirstTwin$HouseholdID, "first child is", FirstTwin$ChildID , "Sex of first child is", FirstTwin$ChildType , "number of sexes are", nrow(TwinsPyramid), "\n")

      if (nrow(TwinsPyramid) == 1) {

        #####################################################################
        # twins are all the one sex
        #####################################################################

        # indicates that there is only one sex present for that twin age
        # all twins can be allocated to the same school, irrespective of whether the school is same-sex or co-ed

        AvailableSchools <- SchoolsRenamed %>%
          filter(ChildAge == FirstTwin$ChildAge,
                 SchoolType %in% c(FirstTwin$ChildType, "C"),
                 ChildCounts >= nrow(TwinsSubset))

        SelectedSchool <- AvailableSchools %>%
          slice_sample(weight_by = ChildCounts, n = 1) %>%
          select(SchoolID, ChildAge, ChildCounts)

      # create the data frame that contains all the twins in the household
        AllTwins <- TwinsSubset %>%
          filter(ChildAge == SelectedSchool$ChildAge)

        # add the same school to all the twins

        SchoolMerged <- inner_join(SelectedSchool, AllTwins, by = "ChildAge")

        # decrease school counts
        SchoolCountDecreases <- SchoolMerged %>%
          slice_head(n=1) %>%
          mutate(FinalCounts = ChildCounts - nrow(TwinsSubset)) %>%
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
        }



        # TODO remove the relevant rows from TwinsAges
        # TODO loop through any non-twins in the data

        # closes if loop for same-sex twins
      }

       #####################################################################
       # twins are both sexes
       #####################################################################


      if (nrow(TwinsPyramid) == 2) {

        cat("Household", FirstTwin$HouseholdID, "has opposite sex twins", "\n")

        AvailableSchools <- SchoolsRenamed %>%
          filter(ChildAge == FirstTwin$ChildAge,
                 SchoolType %in% c(FirstTwin$ChildType, "C"),
                 ChildCounts >= nrow(TwinsSubset))

        SelectedSchool <- AvailableSchools %>%
          slice_sample(weight_by = ChildCounts, n = 1) %>%
          select(SchoolID, ChildAge, ChildCounts, SchoolType)

        # attach other twins who can also be in that school
        # this will differ depending on whether the school or co-ed or same-sex

        # work with same-sex school
        if (SelectedSchool$SchoolType != "C") {

          AllTwinsSameSex <- TwinsSubset %>%
            filter(ChildAge == SelectedSchool$ChildAge,
                   ChildType == FirstTwin$ChildType)

          # work with all twins who are the same sex
          SchoolMerged <- inner_join(SelectedSchool, AllTwinsSameSex, by = "ChildAge")

          # decrease school counts
          SchoolCountDecreases <- SchoolMerged %>%
            slice_head(n=1) %>%
            mutate(FinalCounts = ChildCounts - nrow(TwinsSubset)) %>%
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
          }

          # fix for twins who are the opposite sex
          # extract them
          AllTwinsOppositeSex <- TwinsSubset %>%
            filter(ChildAge == SelectedSchool$ChildAge,
                   ChildType != FirstTwin$ChildType)

          if (!(is.na(AllTwinsOppositeSex$ChildID[1]))) {

          WorkingChildType <- AllTwinsOppositeSex %>%
            slice_head(n=1) %>%
            pull(ChildType)

           RandomRollResult <- runif(1, 0, 1)

          if (RandomRollResult <= ChildProb) {

            AvailableSchools <- SchoolsRenamed %>%
              filter(ChildAge == FirstTwin$ChildAge,
                     SchoolType == WorkingChildType,
                     ChildCounts >= nrow(AllTwinsOppositeSex))

            SelectedSchool <- AvailableSchools %>%
              slice_sample(weight_by = ChildCounts, n = 1) %>%
              select(SchoolID, ChildAge, ChildCounts, SchoolType)

            # but will go to co-ed if no same-sex school available
            if (is.na(AvailableSchools$SchoolType[1]) == TRUE) {

              AvailableSchools <- SchoolsRenamed %>%
                filter(ChildAge == FirstTwin$ChildAge,
                       SchoolType == "C",
                       ChildCounts >= nrow(AllTwinsOppositeSex))

              SelectedSchool <- AvailableSchools %>%
                slice_sample(weight_by = ChildCounts, n = 1) %>%
                select(SchoolID, ChildAge, ChildCounts, SchoolType)

              # put to co-ed if no same-sex school available
            }

            # if random roll not high enough, goes to co-ed school
          } else {

            AvailableSchools <- SchoolsRenamed %>%
              filter(ChildAge == FirstTwin$ChildAge,
                     SchoolType == "C",
                     ChildCounts >= nrow(AllTwinsOppositeSex))

            SelectedSchool <- AvailableSchools %>%
              slice_sample(weight_by = ChildCounts, n = 1) %>%
              select(SchoolID, ChildAge, ChildCounts, SchoolType)


            # but will go to same-sex if no co-ed available
            if (is.na(AvailableSchools$SchoolType[1]) == TRUE) {

              AvailableSchools <- SchoolsRenamed %>%
                filter(ChildAge == FirstTwin$ChildAge,
                       SchoolType == WorkingChildType,
                       ChildCounts >= nrow(AllTwinsOppositeSex))

              SelectedSchool <- AvailableSchools %>%
                slice_sample(weight_by = ChildCounts, n = 1) %>%
                select(SchoolID, ChildAge, ChildCounts, SchoolType)

              # closes loop for going to same-sex if no co-ed available
              }

          # closes if loop for random roll
          }

           SchoolMerged <- inner_join(SelectedSchool, AllTwinsSameSex, by = "ChildAge")

           # decrease school counts
           SchoolCountDecreases <- SchoolMerged %>%
             slice_head(n=1) %>%
             mutate(FinalCounts = ChildCounts - nrow(TwinsSubset)) %>%
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
           }

           # closes school assignment when opposite-sex twins exist
          }

           # closes loop for when first twin is assigned to a same-sex school
        }

        # work with co-ed schools
        if (SelectedSchool$SchoolType == "C") {

          AllTwins <- TwinsSubset %>%
            filter(ChildAge == SelectedSchool$ChildAge)


          # add the same school to all the twins

          SchoolMerged <- inner_join(SelectedSchool, AllTwins, by = "ChildAge")

          # decrease school counts
          SchoolCountDecreases <- SchoolMerged %>%
            slice_head(n=1) %>%
            mutate(FinalCounts = ChildCounts - nrow(TwinsSubset)) %>%
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
          }

          # ends if loop for opposite sex twins, where the school is co-educational
          }


         # closes loop for opposite-sex twins
        }

      # closes for t loop through twins
      }


      # subset the multiples at that age
      # OthersSameAge <- TwinsSubset %>%
      #   filter(!(ChildID %in% FirstTwin$ChildID),
      #          ChildAge == FirstTwin$ChildAge)
      #
      # remove these children from the working children data frame
      WorkingChildren <- WorkingChildren %>%
        filter(!(ChildID %in% TwinsSubset$ChildID))



      # something about here
      # TODO remove the relevant rows from TwinsAges

      # closes if loop for multi-child household that DOES CONTAIN twins
    }


    # closes for x loop that moves through the households
  }

  return()

  # closes function
}

      # # need to extract twin age by twin count
      # # there may be triplets etc, so school counts may be affected by this
      # # gets more complicated if there is a mixture of same-sex AND opposite-sex multiples of the same age
      #
      #
      #
      #
      #
      #
      # AvailableSchools <- SchoolsRenamed %>%
      #   filter(ChildAge == FirstChild$ChildAge,
      #          SchoolType %in% c(FirstChild$ChildType, "C"),
      #          ChildCounts > 0)
      #
      #
      #    if (exists("FinalMatchedChildren")) {
      #
      #      FinalMatchedChildren <- bind_rows(FinalMatchedChildren, AllTwins)
      #
      #    } else {
      #
      #      FinalMatchedChildren <- AllTwins
      #    }
      #
      # # closes if t loop through twins
      # }





      # # random sort the children
      # WorkingChildren <- WorkingChildren %>%
      #   slice_sample(n = nrow(WorkingChildren))
      #
      # # assign the first child
      # # this one is assigned independent of the others
      # FirstChild <- WorkingChildren %>%
      #   slice_head(n=1)
      #
      # AvailableSchools <- SchoolsRenamed %>%
      #   filter(ChildAge == FirstChild$ChildAge,
      #          SchoolType %in% c(FirstChild$ChildType, "C"),
      #          ChildCounts > 0)
      #
      # SelectedSchool <- AvailableSchools %>%
      #   slice_sample(weight_by = ChildCounts, n = 1) %>%
      #   select(SchoolID, ChildAge, ChildCounts)
      #
      # SchoolMerged <- left_join(SelectedSchool, FirstChild, by = "ChildAge")
      #
      # SchoolCountDecreases <- SchoolMerged %>%
      #   mutate(FinalCounts = ChildCounts - 1) %>%
      #   ungroup() %>%
      #   distinct() %>%
      #   select(-ChildCounts) %>%
      #   rename(ChildCounts = FinalCounts)
      #
      # SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID &
      #                                      SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge))
      #
      # SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts
      #
      # SchoolList <- as.vector(SelectedSchool$SchoolID)
      #
      # if (exists("FinalMatchedChildren")) {
      #
      #   FinalMatchedChildren <- bind_rows(FinalMatchedChildren, SchoolMerged)
      #
      # } else {
      #
      #
      #   FinalMatchedChildren <- SchoolMerged
      #
      #   # closes if statement for existence of FinalMatchedChildren
      # }
      #
      # WorkingChildren <- WorkingChildren %>%
      #   filter(WorkingChildren$ChildID != FirstChild$ChildID)
      #
      # # loop through the other children
      # for (y in 1:nrow(WorkingChildren)) {
      #
      #   CurrentChild <- WorkingChildren[y,]
      #
      #   RandomRollResult <- runif(1, 0, 1)
      #
      #   AvailableSchools <- SchoolsRenamed %>%
      #     filter(ChildAge == CurrentChild$ChildAge,
      #            SchoolType %in% c(CurrentChild$ChildType, "C"),
      #            ChildCounts > 0)
      #
      #   if (RandomRollResult <= ChildProb) {
      #
      #     SelectedSchool <- AvailableSchools %>%
      #       filter(SchoolID %in% SchoolList)
      #
      #     # fix the problem if there the classroom count is 0
      #     if (is.na(SelectedSchool$SchoolType[1]) == TRUE) {
      #
      #       SelectedSchool <- AvailableSchools %>%
      #         slice_sample(weight_by = ChildCounts, n = 1) %>%
      #         select(SchoolID, ChildAge, ChildCounts)
      #
      #       # closes if fix for putting in a school replacement where no classroom space available
      #     }
      #
      #
      #   } else {
      #     # ensure that match is to a school not in the school list, as the child cannot be allocated to the same school as another one in the family
      #
      #     AvailableSchools <- SchoolsRenamed %>%
      #       filter(ChildAge == CurrentChild$ChildAge,
      #              SchoolType %in% c(CurrentChild$ChildType, "C"),
      #              ChildCounts > 0,
      #              !(SchoolID %in% c(SchoolList)))
      #
      #
      #     SelectedSchool <- AvailableSchools %>%
      #       slice_sample(weight_by = ChildCounts, n = 1) %>%
      #       select(SchoolID, ChildAge, ChildCounts)
      #
      #     # closes if for school match using the rolled probability
      #   }
      #
      #   SchoolMerged <- left_join(SelectedSchool, CurrentChild, by = "ChildAge")
      #
      #   SchoolCountDecreases <- SchoolMerged %>%
      #     mutate(FinalCounts = ChildCounts - 1) %>%
      #     ungroup() %>%
      #     distinct() %>%
      #     select(-ChildCounts) %>%
      #     rename(ChildCounts = FinalCounts)
      #
      #   SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID &
      #                                        SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge))
      #
      #   SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts
      #
      #   SchoolList <- c(SchoolList, SchoolMerged$ID)
      #
      #   FinalMatchedChildren <- bind_rows(FinalMatchedChildren, SchoolMerged)
      #
      #   # remove the matched children from the working dataframe (i.e. from those still to be matched)
      #   # WorkingChildren <- WorkingChildren %>%
      #   #   filter(!(ChildID %in%  FinalMatchedChildren$ChildID))
      #
      #   # close for loop that circles through the n-1 children in the multiple-child-and-no-twins household
      # }


   #
  #     #####################################################################
  #     # matching households containing twins
  #     #####################################################################
  #
  #     if (length(TwinsAge) > 0) {
  #
  #       cat("There are twins in the ", WorkingChildren$HouseholdID, "household", "\n")
  #
  #
  #       # randomise order of children within the household
  #       # this removes any systematic bias in child ordering
  #       # must be DONE ahead of separating twins
  #       # otherwise it introduces selection bias in households with twins
  #
  #       FirstChild <- WorkingChildren %>%
  #         slice_sample(n = 1)
  #
  #       NotFirstChild <- WorkingChildren %>%
  #         filter(WorkingChildren$ChildID != FirstChild$ChildID )
  #
  #       # is this selected child a twin?
  #       # code will be different depending on whether a twin or not
  #
  #       if (FirstChild$ChildAge %in% TwinsAge) {
  #
  #         # cat("First child", FirstChild$ChildID, "in household", FirstChild$HouseholdID, "is a twin", "\n")
  #
  #         # subset the twins
  #         TheTwins <- WorkingChildren %>%
  #           filter(ChildAge == FirstChild$ChildAge)
  #
  #         NotTwins <- WorkingChildren %>%
  #           filter(!(ChildID %in% TheTwins$ChildID))
  #
  #         # initial school match
  #         # there may be opposite-sex twins
  #         # so retain the count > 0 at this point
  #         TwinSchoolMatches <- left_join(TheTwins, SchoolsRenamed, by = "ChildAge") %>%
  #           filter(ChildCounts > 0, SchoolType %in% c(ChildType, "C"))
  #
  #         # randomly select one school match
  #         # again, match done ahead of classroom minimum size to reduce selection bias as probability is used for selection
  #         # whether this selection is okay is done in a loop below
  #         TheSchoolMatch <- TwinSchoolMatches %>%
  #           slice_sample(n=1)
  #
  #         # loop to ensure that
  #         # 1. if same sex, there is at least the same number as the number of twins classroom places
  #         # 2. if opposite sex and co-ed school is chosen for first, then all others go to the co-ed school
  #         # thus approach with  co-ed school is the same as for the same-sex twins
  #         # 3. if opposite sex and same-sex school is chosen, then check for same-sex school for other child/ren
  #         # else assign to co-ed.
  #         # 4. for households with triplets etc there may be a combination of same-sex and opposite-sex children
  #
  #         if (TheSchoolMatch$SchoolType %in% c("F", "M")) {
  #
  #           # working with the same-sex schools
  #           # need to fix this below.
  #           # TODO same sex school selection should only match if the twins are the same sex.
  #           # currently the code is not doing that
  #
  #           cat("The school type is", TheSchoolMatch$SchoolType, "for the twins in household", TheSchoolMatch$HouseholdID, "\n")
  #
  #           # limits the data to only same-sex school matches
  #           # if other twin is a different sex then they won't be included in this data frame
  #           SameSexSchoolTwins <- TwinSchoolMatches %>%
  #             filter(SchoolID == TheSchoolMatch$SchoolID)
  #
  #           # if loop is needed as split between same-sex and co-ed schools into separate data frames
  #           # only needs to be done once
  #           # and these would be reconstructed for each loop of the while loop
  #
  #           if (SameSexSchoolTwins$ChildCounts < nrow(SameSexSchoolTwins)) {
  #
  #             SameSexSchools <- TwinSchoolMatches %>%
  #               filter(SchoolType %in% c("M", "F"), TwinSchoolMatches$ChildCounts > nrow(SameSexSchoolTwins)) %>%
  #               slice_sample(n=1)
  #
  #             TwinMatch <- SameSexSchools
  #
  #             # if there isn't a sufficiently large same-sex school available
  #             # put the same-sex twins into a co-ed
  #             if (is.na(SameSexSchools$SchoolType[1]) == TRUE) {
  #
  #               TwinsSchoolMerged <- TwinSchoolMatches %>%
  #                 filter(SchoolType == "C", TwinSchoolMatches$ChildCounts > nrow(SameSexSchoolTwins)) %>%
  #                 slice_sample(n=1)
  #
  #               TwinMatch <- SameSexSchools
  #
  #               # closes if switch to co-ed schools if there are no same-sex schools that have classroom places for the twins
  #
  #             }
  #
  #
  #             # close if loop where twins are all assigned to the one same-sex school or all to the one co-ed school
  #           }
  #
  #           # add in bit here so that opposite sex twins are either all to same-sex schools or all to co-ed schools
  #           # check if any twins are unallocated
  #           NumberTwinsRemaining <- nrow(TheTwins) - nrow(TwinMatch)
  #
  #           # return(TwinsRemaining)
  #
  #           # this will only be an opposite-sex twin where the other sex has been allocated to a same-sex school
  #
  #           if (!(is_empty(TwinsRemaining)) == TRUE) {
  #
  #             SameSexEquivalent <- TwinSchoolMatches %>%
  #               filter(!(ChildID %in% TwinMatch$ChildID), SchoolType != "C", ChildCounts < nrow(TwinsRemaining)) %>%
  #               slice_sample(n=1)
  #
  #             # there may be no equivalent same-sex school OR
  #             # there may be no classroom places available
  #             # put to co-ed school instead if either situation occurred
  #             if (is.na(SameSexEquivalent$SchoolType[1]) == TRUE) {
  #
  #               CoedEquivalent <- TwinSchoolMatches %>%
  #                 filter(!(ChildID %in% TwinMatch$ChildID), SchoolType == "C", ChildCounts < nrow(TwinsRemaining)) %>%
  #                 slice_sample(n=1)
  #
  #               # ends if test for putting twins into coed equivalent
  #             }
  #
  #
  #
  #
  #
  #
  #             # closes if for allocation when twins are opposite sex and NOT in a co-ed school
  #           }
  #
  #
  #         } else {
  #
  #           # now working with ONLY the co-ed schools
  #
  #           SameSexSchoolTwins <- TwinSchoolMatches %>%
  #             filter(SchoolID == TheSchoolMatch$SchoolID)
  #
  #
  #           # closes if loop for school selection for twins
  #         }
  #
  #       } else {
  #
  #         cat("First child", FirstChild$ChildID, "in household", FirstChild$HouseholdID, "is NOT a twin", "\n")
  #
  #         # closes differential if for treating twin first selected versus
  #         # first selected is not a twin
  #       }
  #
  #       #  # get school list match for all children in the household
  #       #   SchoolMatches <- left_join(WorkingChildren, SchoolsRenamed, by = "ChildAge") %>%
  #       #    filter(ChildCounts > 0, SchoolType %in% c(ChildType, "C"))
  #       #
  #
  #     } else {
  #
  #       # cat("There are NO twins in the ", WorkingChildren$HouseholdID, "household", "\n")
  #
  #       #####################################################################
  #       # matching households that do not contain twins
  #       #####################################################################
  #
  #
  #       # closes if test for separating households with twins
  #     }
  #
  #
  #     # closes if test for whether household contains 1 child or multiple children
  #   }
  #
  #















































#
#   # only need this info is more than one child is the same sex AND same age
#   # the data frame below holds only the children in the household that are the same age AND same sex
#   # not sure this is required, commented out
#  # TwinsSameSex <- WorkingChildren %>%
#  #   group_by(ChildType, ChildAge) %>%
#  #   summarise(Count = n()) %>%
#  #   filter(Count >1)
#
#
#  # note at this point, match on sex hasn't been made
#  # start the matching
#
#  } else {
#
#    cat("There are no twins in the ", WorkingChildren$HouseholdID, "household", "\n")
#
#  #####################################################################
#  # Match one ("first") child
#  #####################################################################
#
#  # do a random draw from the list of schools, based on classroom size
#  FirstChild <- SchoolMatches %>%
#    slice_sample(weight_by = ChildCounts, n = 1)
#
#  cat("First child age is", FirstChild$ChildAge, "and household is", FirstChild$HouseholdID ,"and first child is", FirstChild$ChildID, "\n")
#
#  # decrement school count by the first child
#  # it is important to do this now
#  # so that we know if a classroom swap is needed for any same-sex twin
#  # don't need a loop as the code just related to the first child
#  # looping through the household occurs below, for the remaining children
#
#  SchoolCountDecrease <- FirstChild %>%
#    select(ChildCounts, SchoolID, ChildAge) %>%
#    mutate(FinalCounts = ChildCounts - 1) %>%
#    select(-ChildCounts) %>%
#    rename(ChildCounts = FinalCounts)
#
#  # test which schools are decremented
#  cat("School is ", SchoolCountDecrease$SchoolID, "and child age is ", SchoolCountDecrease$ChildAge, "and new count is ", SchoolCountDecrease$ChildCounts, "\n")
#
#    SchoolRowIndex <- as.numeric(which((SchoolsRenamed$SchoolID==SchoolCountDecrease$SchoolID) &
#                                         (SchoolsRenamed$ChildAge==SchoolCountDecrease$ChildAge)))
#
#    SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecrease$ChildCounts
#
#
#  #####################################################################
#  # Remove the first matched child from the unmatched children
#  #####################################################################
#
#  RemainingChildren <- SchoolMatches %>%
#    filter(!(ChildID == FirstChild$ChildID))
#
#  HouseholdMatchedChildren <- FirstChild
#
#  #####################################################################
#  # Match the remaining children
#  #####################################################################
#
#  # need to remove the existing matches
#  # cannot do this earlier as the random child/school match requires the school roll data
#  RemainingChildren <- RemainingChildren %>%
#    mutate(TempChildAge = ChildAge) %>%
#    select(-c(colnames(SchoolsRenamed))) %>%
#    mutate(ChildAge = TempChildAge) %>%
#    distinct() %>%
#    select(-TempChildAge)
#
#
#  for (y in 1:nrow(RemainingChildren)) {
#
#    NextChild <- RemainingChildren[y,]
#
#    cat("Next child is ", NextChild$ChildID, " in household", NextChild$HouseholdID, "\n")
#   # check if twin
#
#    if (NextChild$ChildAge %in% HouseholdMatchedChildren$ChildAge) {
#      # cat("Household ID is ", mean(NextChild$HouseholdID), "Child ID is", mean(NextChild$ChildID), "Child age is ", mean(NextChild$ChildAge), "\n")
#
#      NextChildType <- NextChild %>%
#        pull(ChildType)
#
#      MatchingData <- HouseholdMatchedChildren %>%
#        filter(ChildAge == NextChild$ChildAge)
#
#      # check for same-sex match as this has priority
#
#
#  #    cat("Next twin is", NextChild$ChildID[1], "in household", NextChild$HouseholdID, "\n")
#
#        # cat("Next Child ID is ", NextChild$ChildID, "and Child type is ", NextChildType, "\n")
#
#        # if a twin has already been allocated, this child needs to go to the same school
#        # if previous twin has been assigned to same-sex school and this child is the other sex
#        # then this twin needs to go to an equivalent same-sex school
#        # if no equivalent same-sex school then an equivalent co-ed school
#        # there may be more than one equivalent same-sex school
#        # otherwise random draw, EXCLUDING SAME SEX SCHOOLS
#
#        if (NextChildType %in% c(HouseholdMatchedChildren$ChildType)) {
#
#          # if same sex
#          # cat("NextChildID ", NextChild[["ChildID"]][1], " is the same sex as the first child", "\n")
#
#
#
#          # cat("Household ID is ", mean(NextChild$HouseholdID), "Child ID is", mean(NextChild$ChildID), "Child age is ", mean(NextChild$ChildAge), "\n")
#
#
#        } else {
#
#          # if opposite sex
#
#
# #   if (NextChild$ChildAge %in% HouseholdMatchedChildren$ChildAge) {
#    #
#    #   # assign same school if possible
#    #   # next to test if the already assigned school is same-sex and the other twin can go to that same-sex school
#    #   # first, get the school already used
#    #
#    #   TwinSchoolAlreadyMatched <- HouseholdMatchedChildren %>%
#    #     filter(ChildAge == NextChild$ChildAge) %>%
#    #     select(SchoolID, ChildCounts, SchoolType)
#    #
#    #
#    #   # assign same-sex twinScho
#    #   SchoolMatched <- NextChild %>%
#    #     filter(SchoolID )
#    #
#    #   SchoolToMatch <- NextChild %>%
#    #     filter(ChildAge == NextChild$ChildAge)
#
#          #closes test if test that divides the treatment of same-sex and opposite-sex twins
#
#        }
#    #
#    #   #closes if for twins test
#    }
#
#
#     #   #closes for y loop that cycles through the remaining multiple children
#     # }
#     #
#     # # closes new if twins list
#     # }
#
#     #
#
#     # for (y in 1:nrow(SchoolMatches)) {
#     #
#     # Child <- WorkingChildren %>%
#     #   filter(row_number() == y)
#     #
#     # cat( "ChildID is ", Child$ChildID, "Household ID is ", Child$HouseholdID, "\n")
#     #
#     # # ends y loop through the multi-child working children subset
#     # }
#
#     # closes else when there are >1 child per family
#   }
#
#   # closes for x loop through household ID list
#   }
#
#
#
#   return(MatchingData)
#
# }
