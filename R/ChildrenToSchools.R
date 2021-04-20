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
#' @param SchoolIDVariable The column number for the variable in the Schools data frame that contains the name of each school.
#' @param SchoolAgeVariable The column number for the Age variable in the Schools data frame. Each student age within the school must be a separate row.
#' @param SchoolRollCount The number of places available for children at that school age, within the school.
#' @param SchoolCoEdStatus An indicator variable used to determine whether the school is co-educational or single-sex. The expected values are "C" (co-educational), "F" (female only), and "M" (male-only).
#' @param ChildProb If one child is assigned to a same-sex school, the probability that another child in the household is also assigned to a same-sex school. If an eqivalent same-sex school is not available, the other child will be assigned to a co-ed school. The default value is 1, so that all children in the same household will be assigned to same-sex schools, or to co-educational schools. A probability of 0 means that, if one child is assigned to a same-sex school, all other children will be assigned to co-educational schools. The assignment is affected by the number of boy-only and girl-only schools, and the age distribution covered by these schools..
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.

ChildrenToSchools <- function(Children, ChildIDVariable, ChildAgeVariable, ChildSexVariable, HouseholdIDVariable = NULL,
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

      for (x in 1:nrow(HouseholdIDList)) {

    WorkingChildren <- ChildrenRenamed %>%
      filter(HouseholdID %in% HouseholdIDList[x,1])

    #####################################################################
    #####################################################################
    # match single child households
    #####################################################################
    #####################################################################


    if (nrow(WorkingChildren) == 1) {

      #   cat("Household is", WorkingChildren$HouseholdID, "\n")

      Child <- WorkingChildren

      # cat("Child is", Child$ChildID, "\n")

      AvailableSchools <- SchoolsRenamed %>%
        filter(ChildAge == Child$ChildAge,
               SchoolType %in% c(Child$ChildType, "C"),
               ChildCounts > 0)

      SelectedSchool <- AvailableSchools %>%
        slice_sample(weight_by = ChildCounts, n = 1) %>%
        select(SchoolID, ChildAge, ChildCounts)

      SchoolMerged <- left_join(SelectedSchool, Child, by = "ChildAge")

      SchoolCountDecreases <- SchoolMerged %>%
        mutate(FinalCounts = ChildCounts - 1) %>%
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

      }

      # closes loop through single child households
    }

    #####################################################################
    #####################################################################
    # match multiple child households, no twins
    #####################################################################
    #####################################################################

    if (nrow(WorkingChildren) > 1 & HouseholdIDList[x,2] == "N") {

      # cat("Household is", WorkingChildren$HouseholdID, "\n")
      # cat("Multi-child household with no twins", HouseholdIDList[x,1], "\n")

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

    #####################################################################
    #####################################################################
    # match multi-child households WITH TWINS
    #####################################################################
    #####################################################################

    if (HouseholdIDList[x,2] == "Y") {

      SchoolList <- c()

      # cat("Multi-child household with twins", HouseholdIDList[x,1], "\n")

      WorkingChildren <- WorkingChildren %>%
        slice_sample(n = nrow(WorkingChildren))
      #
      #     # cat("HouseholdID is", WorkingChildren$HouseholdID[1], "and the school list contains", paste0(SchoolList), "\n")
      #
      #     # loop through the children in the household
      #     # the number of children will decrease more than one for twins
      #     # so the loop must be WHILE there are children, not from 1 to the number of children
      while (nrow(WorkingChildren) > 0) {

        # cat("The first row of the working children file is", WorkingChildren$ChildAge[1], "\n")

        # get the age of the twins
        TwinsAges <- WorkingChildren %>%
          group_by(ChildAge) %>%
          summarise(Twins = n()) %>%
          filter(Twins > 1)

        # cat("The number of twins ages are", nrow(TwinsAges), "\n")


        # note: there will be a time when there are NOT twins
        # so need an if statement here to check if there are still unassigned twins in the household

        # if (nrow(TwinsAges) > 0 && WorkingChildren$ChildAge == TwinsAges$ChildAge[1]) {

        if (nrow(TwinsAges) > 0 && TwinsAges$ChildAge[1] %in% WorkingChildren$ChildAge) {

          # cat("The first row of the twins ages file is", TwinsAges$ChildAge[1], "\n")
          # cat("The first row of the working children file is", WorkingChildren$ChildAge[1], "\n")
          #
          #         cat("Multi-child household with twins", HouseholdIDList[x,1], "and twin age is", TwinsAges$ChildAge[1], "\n")
          # grab the first row, which is the twins' age to use

          TwinsSubset <- WorkingChildren %>%
            filter(ChildAge == TwinsAges$ChildAge[1])

          # cat("The length of the twins subset is", length(TwinsAges), "\n")
          #
          # cat("Multi-child household with twins", HouseholdIDList[x,1], "has", nrow(TwinsSubset), "twins at age", TwinsAges$ChildAge[1], "\n")

          # now working with ONLY the twins that are in that subset
          # NOTE: they may be either all one sex or both sexes
          # For triplets etc, both same-sex AND opposite may exist for the age

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

          # cat("Household is ", FirstTwin$HouseholdID, "first child is", FirstTwin$ChildID , "Sex of first child is", FirstTwin$ChildType , "number of sexes are", nrow(TwinsPyramid), "\n")

          #####################################################################
          # twins are all the one sex
          #####################################################################
          # indicates that there is only one sex present for that twin age
          # all twins can be allocated to the same school, irrespective of whether the school is same-sex or co-ed
          if (nrow(TwinsPyramid) == 1) {

            # get the counts for that age/sex

            NumberOfTwins <- WorkingChildren %>%
              filter(ChildAge == FirstTwin$ChildAge) %>%
              summarise(NumberThatAge = n()) %>%
              pull(NumberThatAge)

            # cat("In household", FirstTwin$HouseholdID, "there are", NumberOfTwins, "twins aged", FirstTwin$ChildAge, "who are", FirstTwin$ChildType, "\n")

            # get the available schools for the first twin

             AvailableSchools <- SchoolsRenamed %>%
              filter(ChildAge == FirstTwin$ChildAge,
                     SchoolType %in% c(FirstTwin$ChildType, "C"),
                     ChildCounts >= NumberOfTwins)

            if (length(SchoolList) == 0) {

              SelectedSchool <- AvailableSchools %>%
                slice_sample(weight_by = ChildCounts, n = 1)

            } else {

              RandomRollResult <- runif(1, 0, 1)

              # there are four possible combinations of random roll and school list
              # have done these separately so I don't get multiple nested if loops

              # significant random roll and a school match exists
              if (RandomRollResult <= ChildProb && any(AvailableSchools$SchoolID %in% SchoolList)) {

                SelectedSchool <- AvailableSchools %>%
                  filter(SchoolID %in% SchoolList) %>%
                  slice_sample(weight_by = ChildCounts, n = 1)

              }

              # non-significant random roll and a school match exists
              if (RandomRollResult > ChildProb && any(AvailableSchools$SchoolID %in% SchoolList)) {

                SelectedSchool <- AvailableSchools %>%
                  filter(!(SchoolID %in% SchoolList)) %>%
                  slice_sample(weight_by = ChildCounts, n = 1)

              }

              # just need the one, it only matters if the school DOES not exist in the schools list
              # can therefore ignore the random roll result
              # as no school matching problem exists
              if (!(any(AvailableSchools$SchoolID %in% SchoolList))) {

                SelectedSchool <- AvailableSchools %>%
                  slice_sample(weight_by = ChildCounts, n = 1)

              }

            } #closes SchoolList actions


            # add the selected school to the twins

            CurrentTwins <- WorkingChildren %>%
              filter(ChildAge == FirstTwin$ChildAge)

            SchoolMerged <- right_join(SelectedSchool, CurrentTwins, by = "ChildAge")

            # cat("School just added is", SchoolMerged$SchoolID[1], "\n")

            SchoolCountDecreases <- SchoolMerged %>%
              slice_head(n=1) %>%
              mutate(FinalCounts = ChildCounts - nrow(CurrentTwins)) %>%
              ungroup() %>%
              distinct() %>%
              select(-ChildCounts) %>%
              rename(ChildCounts = FinalCounts)

            SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID &
                                                 SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge))

            SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts

            if(exists("SchoolList")) {

              SchoolList <- c(SchoolList, SchoolCountDecreases$SchoolID)

            } else {

              SchoolList <- SchoolCountDecreases$SchoolID

              # cat(SchoolCountDecreases$SchoolID, "has been added to the School List", "\n")

              # cat("Household is",  FirstTwin$HouseholdID, "and schoolID is", SchoolCountDecreases$SchoolID, "\n")
              # cat("The SchoolList now contains", paste0(SchoolList), "\n")

              # closes school list loop
            }


            # put the children who have been assigned to schools into the output file

            if (exists("FinalMatchedChildren")) {

              FinalMatchedChildren <- bind_rows(FinalMatchedChildren, SchoolMerged)

            } else {


              FinalMatchedChildren <- SchoolMerged

              # closes if statement for existence of FinalMatchedChildren
            }

            # remove the twins from the working children
            WorkingChildren <- WorkingChildren %>%
              filter(!(ChildID %in%  FinalMatchedChildren$ChildID))


            # closes test for whether all twins are the same sex
          }

          #####################################################################
          # twins are opposite sexes
          #####################################################################
          # indicates that there is only one sex present for that twin age
          # all twins can be allocated to the same school, irrespective of whether the school is same-sex or co-ed

          if (nrow(TwinsPyramid) > 1) {

            SameSexTwins <- WorkingChildren %>%
              filter(ChildAge == FirstTwin$ChildAge,
                     ChildType == FirstTwin$ChildType)

            OppositeSexTwins <- WorkingChildren %>%
              filter(ChildAge == FirstTwin$ChildAge,
                     ChildType != FirstTwin$ChildType)


            # cat("Number of same sex twins is", nrow(SameSexTwins), "\n")
            #
            # cat("Number of opposite sex twins is", nrow(OppositeSexTwins), "in Household", OppositeSexTwins$HouseholdID[1], "\n")

            #####################################################################
            # same sexes, could be triplets, etc
            #####################################################################

            AvailableSchools <- SchoolsRenamed %>%
              filter(ChildAge == FirstTwin$ChildAge,
                     SchoolType %in% c(FirstTwin$ChildType, "C"),
                     ChildCounts >= nrow(SameSexTwins))

            if (length(SchoolList) == 0) {

              SelectedSchool <- AvailableSchools %>%
                slice_sample(weight_by = ChildCounts, n = 1)

            } else {

              RandomRollResult <- runif(1, 0, 1)

              # there are four possible combinations of random roll and school list
              # have done these separately so I don't get multiple nested if loops

              # significant random roll and a school match exists
              if (RandomRollResult <= ChildProb && any(AvailableSchools$SchoolID %in% SchoolList)) {

                SelectedSchool <- AvailableSchools %>%
                  filter(SchoolID %in% SchoolList) %>%
                  slice_sample(weight_by = ChildCounts, n = 1)

              }

              # non-significant random roll and a school match exists
              if (RandomRollResult > ChildProb && any(AvailableSchools$SchoolID %in% SchoolList)) {

                SelectedSchool <- AvailableSchools %>%
                  filter(!(SchoolID %in% SchoolList)) %>%
                  slice_sample(weight_by = ChildCounts, n = 1)

              }

              # just need the one, it only matters if the school DOES not exist in the schools list
              # can therefore ignore the random roll result
              # as no school matching problem exists
               if (!(any(AvailableSchools$SchoolID %in% SchoolList))) {

                SelectedSchool <- AvailableSchools %>%
                  slice_sample(weight_by = ChildCounts, n = 1)

              }

            } #closes SchoolList actions


            # add the selected school to the twins

            SchoolMerged <- right_join(SelectedSchool, SameSexTwins, by = "ChildAge")

            # cat("School just added is", SchoolMerged$SchoolID[1], "\n")

            SchoolCountDecreases <- SchoolMerged %>%
              slice_head(n=1) %>%
              mutate(FinalCounts = ChildCounts - nrow(SameSexTwins)) %>%
              ungroup() %>%
              distinct() %>%
              select(-ChildCounts) %>%
              rename(ChildCounts = FinalCounts)

            SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID &
                                                 SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge))

            SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts

            if(exists("SchoolList")) {

              SchoolList <- c(SchoolList, SchoolCountDecreases$SchoolID)

            } else {

              SchoolList <- SchoolCountDecreases$SchoolID

              cat(SchoolCountDecreases$SchoolID, "has been added to the School List", "\n")

              # cat("Household is",  FirstTwin$HouseholdID, "and schoolID is", SchoolCountDecreases$SchoolID, "\n")
              # cat("The SchoolList now contains", paste0(SchoolList), "\n")

              # closes school list loop
            }


            # put the children who have been assigned to schools into the output file

            if (exists("FinalMatchedChildren")) {

              FinalMatchedChildren <- bind_rows(FinalMatchedChildren, SchoolMerged)

            } else {


              FinalMatchedChildren <- SchoolMerged

              # closes if statement for existence of FinalMatchedChildren
            }

            # remove the twins from the working children
            WorkingChildren <- WorkingChildren %>%
              filter(!(ChildID %in%  FinalMatchedChildren$ChildID))


            #####################################################################
            # different sexes, could be triplets, etc
            #####################################################################

            OppositeSexTwinsType <- OppositeSexTwins %>%
              slice_head(n = 1)

            AvailableSchools <- SchoolsRenamed %>%
              filter(ChildAge == FirstTwin$ChildAge,
                     SchoolType %in% c(OppositeSexTwinsType$ChildType, "C"),
                     ChildCounts >= nrow(OppositeSexTwins))

            if (length(SchoolList) == 0) {

              SelectedSchool <- AvailableSchools %>%
                slice_sample(weight_by = ChildCounts, n = 1)

            } else {

              RandomRollResult <- runif(1, 0, 1)

              # there are four possible combinations of random roll and school list
              # have done these separately so I don't get multiple nested if loops

              # significant random roll and a school match exists
              if (RandomRollResult <= ChildProb && any(AvailableSchools$SchoolID %in% SchoolList)) {

                SelectedSchool <- AvailableSchools %>%
                  filter(SchoolID %in% SchoolList) %>%
                  slice_sample(weight_by = ChildCounts, n = 1)

              }

              # non-significant random roll and a school match exists
              if (RandomRollResult > ChildProb && any(AvailableSchools$SchoolID %in% SchoolList)) {

                SelectedSchool <- AvailableSchools %>%
                  filter(!(SchoolID %in% SchoolList)) %>%
                  slice_sample(weight_by = ChildCounts, n = 1)

              }

              # just need the one, it only matters if the school DOES not exist in the schools list
              # can therefore ignore the random roll result
              # as no school matching problem exists
               if (!(any(AvailableSchools$SchoolID %in% SchoolList))) {

                SelectedSchool <- AvailableSchools %>%
                  slice_sample(weight_by = ChildCounts, n = 1)

              }

            } #closes SchoolList actions


            # add the selected school to the twins

            SchoolMerged <- right_join(SelectedSchool, OppositeSexTwins, by = "ChildAge")

            # cat("School just added is", SchoolMerged$SchoolID[1], "\n")

            SchoolCountDecreases <- SchoolMerged %>%
              slice_head(n=1) %>%
              mutate(FinalCounts = ChildCounts - nrow(OppositeSexTwins)) %>%
              ungroup() %>%
              distinct() %>%
              select(-ChildCounts) %>%
              rename(ChildCounts = FinalCounts)

            SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID &
                                                 SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge))

            SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts

            if(exists("SchoolList")) {

              SchoolList <- c(SchoolList, SchoolCountDecreases$SchoolID)

            } else {

              SchoolList <- SchoolCountDecreases$SchoolID

              # cat(SchoolCountDecreases$SchoolID, "has been added to the School List", "\n")
#
#               cat("Household is",  FirstTwin$HouseholdID, "and schoolID is", SchoolCountDecreases$SchoolID, "\n")
#               cat("The SchoolList now contains", paste0(SchoolList), "\n")

              # closes school list loop
            }


            # put the children who have been assigned to schools into the output file

            if (exists("FinalMatchedChildren")) {

              FinalMatchedChildren <- bind_rows(FinalMatchedChildren, SchoolMerged)

            } else {


              FinalMatchedChildren <- SchoolMerged

              # closes if statement for existence of FinalMatchedChildren
            }

            # remove the twins from the working children
            WorkingChildren <- WorkingChildren %>%
              filter(!(ChildID %in%  FinalMatchedChildren$ChildID))

          } # closes if loop for working through opposite-sex twin pyramid


        }  else {# closes if loop for allocating twins

          #####################################################################
          # twins are opposite sexes
          #####################################################################

          # cat("Entered not-twins loop", "\n")

          CurrentChild <- WorkingChildren %>%
            slice_head(n=1)

          AvailableSchools <- SchoolsRenamed %>%
            filter(ChildAge == CurrentChild$ChildAge,
                   SchoolType %in% c(CurrentChild$ChildType, "C"),
                   ChildCounts >= 1)

          if (length(SchoolList) == 0) {

            SelectedSchool <- AvailableSchools %>%
              slice_sample(weight_by = ChildCounts, n = 1)

          } else {

            RandomRollResult <- runif(1, 0, 1)

            # there are four possible combinations of random roll and school list
            # have done these separately so I don't get multiple nested if loops

            # significant random roll and a school match exists
            if (RandomRollResult <= ChildProb && any(AvailableSchools$SchoolID %in% SchoolList)) {

              SelectedSchool <- AvailableSchools %>%
                filter(SchoolID %in% SchoolList) %>%
                slice_sample(weight_by = ChildCounts, n = 1)

            }

            # non-significant random roll and a school match exists
            if (RandomRollResult > ChildProb && any(AvailableSchools$SchoolID %in% SchoolList)) {

              SelectedSchool <- AvailableSchools %>%
                filter(!(SchoolID %in% SchoolList)) %>%
                slice_sample(weight_by = ChildCounts, n = 1)

            }

            # just need the one, it only matters if the school DOES not exist in the schools list
            # can therefore ignore the random roll result
            # as no school matching problem exists

            if (!(any(AvailableSchools$SchoolID %in% SchoolList))) {
              SelectedSchool <- AvailableSchools %>%
                slice_sample(weight_by = ChildCounts, n = 1)

            }

          } #closes SchoolList actions


          # add the selected school to the twins

          SchoolMerged <- right_join(SelectedSchool, CurrentChild, by = "ChildAge")

          # cat("School just added is", SchoolMerged$SchoolID[1], "\n")

          SchoolCountDecreases <- SchoolMerged %>%
            slice_head(n=1) %>%
            mutate(FinalCounts = ChildCounts - 1) %>%
            ungroup() %>%
            distinct() %>%
            select(-ChildCounts) %>%
            rename(ChildCounts = FinalCounts)

          SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID &
                                               SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge))

          SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts

          if(exists("SchoolList")) {

            SchoolList <- c(SchoolList, SchoolCountDecreases$SchoolID)

          } else {

            SchoolList <- SchoolCountDecreases$SchoolID

            cat(SchoolCountDecreases$SchoolID, "has been added to the School List", "\n")

            cat("Household is",  FirstTwin$HouseholdID, "and schoolID is", SchoolCountDecreases$SchoolID, "\n")
            cat("The SchoolList now contains", paste0(SchoolList), "\n")

            # closes school list loop
          }


          # put the children who have been assigned to schools into the output file

          if (exists("FinalMatchedChildren")) {

            FinalMatchedChildren <- bind_rows(FinalMatchedChildren, SchoolMerged)

          } else {


            FinalMatchedChildren <- SchoolMerged

            # closes if statement for existence of FinalMatchedChildren
          }

          # remove the twins from the working children
          WorkingChildren <- WorkingChildren %>%
            filter(!(ChildID %in%  FinalMatchedChildren$ChildID))

        } # closes if loop for non-twins in twin households

      } # closes if loop going through the children in the household

    } # closes loop that identifies if the household contains twins


    # closes the loop through the households
  }

  FinalMatchedChildren <- FinalMatchedChildren %>%
    select(-SchoolType)

  return(FinalMatchedChildren)

  # closes function
}
