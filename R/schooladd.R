#' Create a data frame of people matched to schools
#' This function creates a data frame of people and matching schools. By default, all similarly-aged people in the same household, who are in school, will be matched to the same school. If one person is matched to a same-sex school, then all similarly aged people will also be matched to a same-sex school. This includes opposite-sex children.
#' Two data frames are required: one for the children and one for the schools.
#' A numeric or ordered factor for school status is required. The smallest value/level will be treated as the code for people not in school. If one value is used, everyone in the data frame will be allocated a school. Thus, pre-cleaning a data frame is not required. If everyone in the data frame is to be allocated a school, then the same value must be used for everyone.
#' The Schools data frame must be a summary in the form of counts by age within school. Each row is one age only. For example, if a school has children aged 5 to 9 years, there should be 5 rows. Any combination of co-educational and single-sex schools can be used.
#' The minimum and maximum school ages, and the counts by sex for each school, are printed to the console.
#'
#' @export
#' @param people A data frame containing the people to be paired with a parent/guardian.
#' @param pplidcol The column number for the ID variable in the people data frame.
#' @param pplagecol The column number for the age variable in the people data frame.
#' @param pplsxcol The column number for the sex indicator for people. This column is used to assign people to the appropriate school type (co-educational or single-sex). The expected values are "F" (female) or "M" (male).
#' @param pplstcol The column number for the school status. Only two numeric values/factor levels can be used. The smallest number/level is the code for people not in school.
#' @param hhidcol The column number for the household identifier variable in the people data frame.
#' @param schools A data frame containing the school observations.
#' @param schidcol The column number for the ID variable in the schools data frame. Values in this column are treated as character. If the IDs are factors, these will be converted to character.
#' @param schagecol The column number for the Age variable in the schools data frame.
#' @param schrollcol The number of places available for people at that school age, within the school.
#' @param schtypecol An indicator variable used to determine whether the school is co-educational or single-sex. The expected values are "C" (co-educational), "F" (female only), and "M" (male-only).
#' @param personprob If one person is assigned to a same-sex school, the probability that another person in the household is also assigned to a same-sex school. If an equivalent same-sex school is not available, the other person will be assigned to a co-ed school. The default value is 1, so that all similarly aged people will be assigned to their respective same-sex schools, or all will be to co-educational schools.
#' @param schmiss The value that will be given to those people not in school. If left blank, the default value is 0.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @return A single data frame.
#'
#' @examples
# library(dplyr)
# SchoolsAdded <- schooladd(IntoSchools, pplidcol = 3, pplagecol = 4, pplsxcol = 8, pplstcol = 6,  hhidcol = 7,
#                           SchoolsToUse, schidcol = 2, schagecol = 4, schrollcol = 5, schtypecol = 3,
#                           UserSeed = 4)

schooladd <- function(people, pplidcol, pplagecol, pplsxcol, pplstcol = NULL, hhidcol = NULL, schools,
                      schidcol, schagecol, schrollcol, schtypecol, personprob = 1, schmiss = 0,
                      UserSeed=NULL)
{

  options(dplyr.summarise.inform=F)

  # content check
  if (!(is.factor(pplstcol)) & !(is.numeric(pplstcol))) {
    stop("The school status variable must be a factor or be numeric.")
  }

  #####################################################################
  #####################################################################
  # Test for any problem ages, stop function if this situation exists
  #####################################################################
  #####################################################################

  peopleRenamed <- people %>%
    rename(personID = !! pplidcol, personAge = !! pplagecol, personType = !! pplsxcol,
           HouseholdID = !! hhidcol, SchStat = !! pplstcol)

  NotInSchool <- peopleRenamed

  # can only take two values for school status variable

  TestLevels <- peopleRenamed %>%
    select(SchStat) %>%
    group_by(SchStat) %>%
    summarise(Nums = n()) %>%
    pull(SchStat)

  if(length(TestLevels) > 2) {
    stop("The school status variable must contain a maximum of two values.")
  }

  schoolsRenamed <- schools %>%
    rename(SchoolID = !! schidcol, SchoolAge = !! schagecol,
           personCounts = !! schrollcol, SchoolType = !! schtypecol) %>%
    mutate(SchoolID = as.character(SchoolID),
           SchoolType = as.character(SchoolType)) %>%
    #mutate(across(where(is.factor), as.character)) %>%
    select(SchoolID, SchoolAge, personCounts, SchoolType)

  OriginalschoolsCounts <- schoolsRenamed

  ###############################################
  ###############################################
  # get the col names
  ###############################################
  ###############################################

  pplidcolName <- sym(names(people[pplidcol]))
  pplagecolName <- sym(names(people[pplagecol]))
  hhidcolName <- sym(names(people[hhidcol]))
  pplsxcolName <- sym(names(people[pplsxcol]))
  schidcolName <- sym(names(schools[schidcol]))

  ###############################################
  ###############################################
  # quick test of compatibility of counts
  ###############################################
  ###############################################


  schoolsCountTest <- schoolsRenamed %>%
    group_by(SchoolAge) %>%
    summarise(SchoolAgeCount = sum(personCounts))

  ###############################################
  # test if there is only one factor level, i.e. all kids to assign
  ###############################################

  # print(length(TestLevels))

  if(length(TestLevels) == 1) {

    peopleCountTest <- peopleRenamed %>%
      group_by(personAge) %>%
      summarise(AgeCount = n())

    CountComparison <- full_join(peopleCountTest, schoolsCountTest, by = c("personAge" = "SchoolAge")) %>%
      mutate(AgeCount = replace(AgeCount, is.na(AgeCount), 0),
             SchoolAgeCount = replace(SchoolAgeCount, is.na(SchoolAgeCount), 0),
             CountDiff = SchoolAgeCount - AgeCount) %>%
      filter(SchoolAgeCount != 0, AgeCount != 0)

    TooManyKids <- CountComparison %>%
      filter(CountDiff < 0) %>%
      select(personAge)

    # need to construct the reduced data frame on the basis of age at school

  } else {

    # cat("Entered multiple factor loop", "\n")
    # get min factor level to exclude

    MinFactorLevel <- min(as.integer(peopleRenamed$SchStat))

    # print(MinFactorLevel)

    NotFactor <- peopleRenamed %>%
      filter(as.integer(SchStat) == as.integer(MinFactorLevel))

    # cat("The notfactor data frame has this number of rows", nrow(NotFactor), "\n")

    # cat("The number of rows in original peoplerenamed is", nrow(peopleRenamed), "\n")

    peopleRenamed <- peopleRenamed %>%
      filter(!(personID %in% NotFactor$personID))

    # cat("The revised count after removing wrong factor level is", nrow(peopleRenamed), "\n")

    peopleCountTest <- peopleRenamed %>%
      group_by(personAge) %>%
      summarise(AgeCount = n())

    CountComparison <- full_join(peopleCountTest, schoolsCountTest, by = c("personAge" = "SchoolAge")) %>%
      mutate(AgeCount = replace(AgeCount, is.na(AgeCount), 0),
             SchoolAgeCount = replace(SchoolAgeCount, is.na(SchoolAgeCount), 0),
             CountDiff = SchoolAgeCount - AgeCount) %>%
      filter(SchoolAgeCount != 0, AgeCount != 0)

    TooManyKids <- CountComparison %>%
      filter(CountDiff < 0) %>%
      select(personAge)

    # closes else to if(length(TestLevels) == 1)
  }


  # test should now work for both situations
  if (!(nrow(TooManyKids)==0)) {

    TooManyKids <- as.vector(TooManyKids)

    stop(paste("The number of people at these ages exceeds the available school roll places: ", TooManyKids))

  }

  MaxSchoolAge <- as.numeric(CountComparison[nrow(CountComparison), 1])

  cat("The minimum school age is", as.numeric(CountComparison[1,1]), "and the maximum school age is ", as.numeric(CountComparison[nrow(CountComparison), 1]), "\n")


  # restrict person and school data frames to these minimum and maximum ages
  # is done for multiple factor levels too, just in case the school age range is incompatible
  # get age range
  AgeRestriction <- CountComparison %>%
    select(personAge)

  # apply to people

  peopleRenamed <- left_join(AgeRestriction, peopleRenamed, by = "personAge")

  # NOTE: this removes any school classrooms where NO people of that age exist in the data
  schoolsRenamed <- left_join(AgeRestriction, schoolsRenamed, by = c("personAge" = "SchoolAge"))

  schoolsCountColIndex <- as.numeric(which(colnames(schoolsRenamed) == "personCounts"))

  # get rid of the tibbles
  peopleRenamed <- as.data.frame(peopleRenamed)
  schoolsRenamed <- as.data.frame(schoolsRenamed)

  #####################################################################
  # Create household meta data data frame
  #####################################################################
  # get the number of households
  NumberHouseholds <- as.numeric(peopleRenamed %>%
                                   dplyr::summarise(Count = n_distinct(HouseholdID)) %>%
                                   pull(Count))

  # get list of household IDs
  HouseholdIDList <- as.data.frame(peopleRenamed %>%
                                     distinct(HouseholdID))

  #####################################################################
  # create counts by sex
  # will be used to ensure that selected schools do not decrease
  # the availability of
  #####################################################################

  peopleSexAge <- peopleRenamed %>%
    group_by(personType, personAge) %>%
    summarise(AgeCount = n())

  schoolsexAge <- schoolsRenamed %>%
    group_by(SchoolType, personAge) %>%
    summarise(SchoolAgeCount = sum(personCounts))

  # this is the first part of the code that requires randomness
  # so seed is applied here

  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  ListofHouseholds <- peopleRenamed %>%
    group_by(HouseholdID) %>%
    summarise(NumberKids = n())

  ###############################################
  ###############################################
  ###############################################
  # assignment of schools
  ###############################################
  ###############################################
  ###############################################

  for(i in 1:nrow(ListofHouseholds)) {


    if(exists("UnmatchedSingleSexToMerge")) {
      rm(UnmatchedSingleSexToMerge)
    }

    if(exists("MergedSingleSexToAdd")) {
      rm(MergedSingleSexToAdd)
    }

    if(exists("SingleSexMatchedschools")) {
      rm(SingleSexMatchedschools)
    }


    CurrentHousehold <- ListofHouseholds$HouseholdID[i]

     # cat("The current household is", CurrentHousehold, "\n")


    # get the people in the household

    peopleInHousehold <- peopleRenamed %>%
      filter(HouseholdID == CurrentHousehold)

    NumKidsRemaining <- nrow(peopleInHousehold)

    # only need to do this for households with more than one person
    if(NumKidsRemaining >1) {


      # random roll to see if any people in same school, will prioritise the twins
      RandomRollVector <- runif(nrow(peopleInHousehold)-1)

      # cat(RandomRollVector, "\n")

      # test number of people who should go to the same school
      NumberSameSchool <- data.frame(RandomRollVector) %>%
        filter(RandomRollVector > (1-personprob)) %>%
        summarise(SameSchool = n()) %>%
        pull(SameSchool)


      # fix the number of people that can go to the same school
      if(NumberSameSchool == 1 | NumberSameSchool == 0) {

        NumberSameSchool <- 2

        # } else if (NumberSameSchool == 0) {
        #
        #   NumberSameSchool <- 2

      } else {

        FinalRandomRoll <- runif(1)

        if(FinalRandomRoll > (1-personprob)) {

          NumberSameSchool <- NumberSameSchool +1

          # closes if(FinalRandomRoll > (1-personprob))
        }


        # closes  if(NumberSameSchool == 1 | NumberSameSchool == 0)
      }

      # closes if(NumKidsRemaining >1)
    } else {

      NumberSameSchool <- 1
    }


    # print(NumberSameSchool)

    # add people to same school
    # need to identify the number of people that can go to the same school

    # get person ages vector
    personAges <- as.data.frame(peopleInHousehold %>%
                                 group_by(personType, personAge) %>%
                                 summarise(CountsByAge = n()))


    # locate schools that can take the maximum number of people from NumberSameSchool down

    schoolsubset <- left_join(personAges, schoolsRenamed, by = "personAge") %>%
      mutate(IsMatch = ifelse(SchoolType == "C" | SchoolType == personType, "Y", "N")) %>%
      filter(personCounts > 0,
             IsMatch == "Y") %>%
      group_by(SchoolID, personAge, SchoolType, personCounts) %>%
      summarise(NumberKids = sum(CountsByAge)) %>%
      ungroup() %>%
      mutate(Remainingpeople = personCounts - NumberKids) %>%
      filter(Remainingpeople >= 0)

    NumberKidsPerSchool <- schoolsubset %>%
      group_by(SchoolID) %>%
      summarise(across(c(personCounts, NumberKids), sum))

    # cat("Number kids per school subset is", nrow(schoolsubset), "\n")

    # cat("Possibleschools IsMatch is below", "\n")

    # get the co-ed schools

    CoedschoolsSelected <- schoolsubset %>%
      filter(SchoolType == "C")

    # cat("Number kids per coed school row count is", nrow(CoedschoolsSelected), "\n")

    # work with the single-sex schools

    SingleSexschoolsSelected <- schoolsubset %>%
      filter(!(SchoolType =="C"))

    # cat("Number kids per not coed school row count is", nrow(SingleSexschoolsSelected), "\n")



    # determine if there are two single sex schools that have the equivalent age
    # in the schoolsSubset
    FemaleAgesToMatch <- schoolsubset %>%
      filter(SchoolType == "F")

    # this will only return a non-empty data frame if there is a male school
    # that exists in the schoolsSubset, ignoring age
    MaleschoolsToMatch <- schoolsubset %>%
      filter(SchoolType == "M")


    # will only return a data frame is there is a male school included in the schoolsubset
    # that covers the age range of any single-sex schools selected
    # may have no matched males - because either
    # 1. the single-sex schools age ranges do not align, or
    # 2. there is only one single-sex school available

    MatchedMales <- left_join(FemaleAgesToMatch, MaleschoolsToMatch, by = "personAge")

    if(!(is.na(MatchedMales$SchoolID.y[1]))) {

      # cat("There is an overlap in the single-sex school ages", "\n")

      SingleSexMatchedschools <- MatchedMales %>%
        select(SchoolID.x, SchoolID.y) %>%
        distinct() %>%
        tidyr::drop_na() %>%
        left_join(NumberKidsPerSchool, by = c("SchoolID.x" = "SchoolID")) %>%
        rename(personCounts.x = personCounts, NumberKids.x = NumberKids) %>%
        left_join(NumberKidsPerSchool, by = c("SchoolID.y" = "SchoolID"))  %>%
        rename(personCounts.y = personCounts, NumberKids.y = NumberKids) %>%
        mutate(personCounts = personCounts.x + personCounts.y,
               NumberKids = NumberKids.x + NumberKids.y,
               SchoolID = paste0("CombinedSchool", 1:nrow(.)))


      # cat("SingleSexMatchedschools okay for household", CurrentHousehold, "\n")

      MergedSingleSexToAdd <- SingleSexMatchedschools %>%
        select(SchoolID, personCounts, NumberKids) %>%
        mutate(SchoolType = "S")

      # cat("MergedSingleSexToAdd okay for household", CurrentHousehold, "\n")

      # note the NA value for the people remaining as this information is absent from the merger
      # of the single-sex schools
      AllschoolsFromWhichToChoose <- bind_rows(CoedschoolsSelected, MergedSingleSexToAdd)

      # need to also amend NumberKidsPerSchool as that is the sampling data frame
      # and has not been amended from the code above
      # have to remove the matched single sex schools and add the replacement combo school in
      # remove the school type from the MergedSingleSex
      ComboToAdd <- MergedSingleSexToAdd %>%
        select(-SchoolType)

      NumberKidsPerSchool <- NumberKidsPerSchool %>%
        filter(!(SchoolID %in% c(MatchedMales$SchoolID.x)),
               !(SchoolID %in% c(MatchedMales$SchoolID.y))) %>%
        bind_rows(ComboToAdd)

      # cat("AllschoolsFromWhichToChoose okay for household", CurrentHousehold, "\n")


      UnmatchedSingleSexToMerge <- SingleSexschoolsSelected %>%
        filter(!(SchoolID %in% c(SingleSexMatchedschools$SchoolID.x)),
               !(SchoolID %in% c(SingleSexMatchedschools$SchoolID.y)))

      if(!(is.na(UnmatchedSingleSexToMerge$NumberKids[1]))) {

        # cat("UnmatchedSingleSexToMerge loop entered for for household", CurrentHousehold, "\n")

        # print(str(Possibleschools))
        # cat("The rows in UnmatchedSingleSexToMerge are", nrow(UnmatchedSingleSexToMerge), "\n")
        # print(str(UnmatchedSingleSexToMerge))
        # cat("The key test value is", UnmatchedSingleSexToMerge$NumberTimes[1], "\n")

        PossibleschoolsIndicators <- schoolsubset %>%
          select(SchoolID, SchoolType) %>%
          filter(SchoolID %in% c(UnmatchedSingleSexToMerge$SchoolID))

        AbsentSingleSex <- schoolsubset %>%
          filter(SchoolID %in% c(UnmatchedSingleSexToMerge$SchoolID)) %>%
          group_by(SchoolID) %>%
          summarise(across(personCounts, list(RollCountSum = sum, NumberTimes = ~n()))) %>%
          rename(RollCountSum = personCounts_RollCountSum, NumberTimes = personCounts_NumberTimes) %>%
          left_join(PossibleschoolsIndicators, by = "SchoolID")


        AllschoolsFromWhichToChoose <- bind_rows(AllschoolsFromWhichToChoose, AbsentSingleSex)

        # closes  if(!(is.na(UnmatchedSingleSexToMerge$NumberKids[1])))
      }

      # closes if(!(is.na(UnmatchedSingleSexToMerge$SchoolID[1])))
    } else if ("F" %in% schoolsubset$SchoolType | "M" %in% schoolsubset$SchoolType){


      # need to do the same if no matching single sex schools for the age ranges

      # cat("Entered some same sex schools loop", "\n")

      SingleSexschools <- schoolsubset %>%
        filter(!(SchoolType == "C"))

      AllschoolsFromWhichToChoose <- bind_rows(CoedschoolsSelected, SingleSexschools)

      # closes else after closes if(!(is.na(UnmatchedSingleSexToMerge$SchoolID[1])))
    } else {

      # cat("Entered no-same-sex-schools loop", "\n")

      AllschoolsFromWhichToChoose <- CoedschoolsSelected

    }

    # select schools by probability
    # uses simple weight by roll count
    # cat("The random roll is", NumberSameSchool, "for household", CurrentHousehold, "\n")

    # select schools
    # select school combination by probability
    # but limit to those schools that accept the number of people that do not exceed the number
    # to be assigned to the same school

    # cat("The schools from which to choose are", "\n")
    # str(NumberKidsPerSchool)


    while(NumKidsRemaining > 0) {
      # first set of schools to choose

      # cat("Number kids per school is", NumberKidsPerSchool$NumberKids, "NumberSameSchool is",
      #     NumberSameSchool, "household ID is", CurrentHousehold, "Number kids remaining is",
      #     NumKidsRemaining, "\n")

      MaxpeopleCanTake <- (min(NumberSameSchool, max(NumberKidsPerSchool$NumberKids)))

      # extract these schools
      Multiplesschools <- NumberKidsPerSchool %>%
        filter(NumberKids >= MaxpeopleCanTake)


      # cat("The max people is", MaxpeopleCanTake, "\n")

      # cat("The number of people to the same school", NumberSameSchool, "\n")

      # random select from the Multipleschools
      # probability based on roll size

      # print(Multiplesschools$RollCountSum)

      # will always be at least one person that a school can take
      # need to loop through the number of people in the household so that
      # the school assignment is done for each one

      SchoolChosen <- Multiplesschools %>%
        slice_sample(weight_by = NumberKids, n=1)


      # now need to loop through all the people in the family

      # print(CurrentHousehold)

      # cat("And the school chosen is", "\n")
      # str(SchoolChosen)

      # cat("With possible schools")
      # str(AllschoolsFromWhichToChoose)

      # create a SchoolChosenDetail data frame when a same-sex combination is selected
      if(exists("SingleSexMatchedschools") == TRUE) {

        if(isTRUE(SchoolChosen$SchoolID %in% c(SingleSexMatchedschools$SchoolID))) {

          # cat("Uses the combo single sex school SchoolID", "\n")

          School1 <- SingleSexMatchedschools %>%
            filter(SchoolID == SchoolChosen$SchoolID) %>%
            select(SchoolID.x)

          SchoolDetail1 <- schoolsubset %>%
            filter(SchoolID == School1$SchoolID.x)

          School2 <- SingleSexMatchedschools %>%
            filter(SchoolID == SchoolChosen$SchoolID) %>%
            select(SchoolID.y)

          SchoolDetail2 <- schoolsubset %>%
            filter(SchoolID == School2$SchoolID.y)

          SchoolChosenDetail <- bind_rows(SchoolDetail1, SchoolDetail2)

          # close if(isTRUE(SchoolChosenDetail$SchoolID %in% c(SingleSexMatchedschools$SchoolID)))
        } else {

          # cat("Should have else here and SchoolID is", SchoolChosen$SchoolID, "\n")

          SchoolChosenDetail <- AllschoolsFromWhichToChoose %>%
            filter(SchoolID == SchoolChosen$SchoolID)

        }

      } else {

        # the school is chosen normally as the single-sex combo was not selected

        # cat("All schools from which to chose is", "\n")
        #
        # str(AllschoolsFromWhichToChoose)
        #
        # cat("No single sex school combo and the possible schools that will be merged are", "\n")
        #
        # str(schoolsubset)

        SchoolChosenDetail <- AllschoolsFromWhichToChoose %>%
          filter(SchoolID == SchoolChosen$SchoolID) #%>%
        #     left_join(schoolsubset, by = c("SchoolID", "SchoolType")) %>%
        # filter(personAge %in% c(personAges$personAge))

        # cat("The file SchoolChosenDetail inside loop is", "\n")
        # str(SchoolChosenDetail)

        # cat("Also entered this final loop", "\n")

        # cat("The school chosen detail is", "\n")
        #
        # print(str(SchoolChosenDetail))

        # close if(exists("SingleSexMatchedschools") == TRUE)
      }

      # cat("The people in the household are", "\n")
      # str(peopleInHousehold)
      #
      # cat("The file SchoolChosenDetail is", "\n")
      # str(SchoolChosenDetail)

      # cat("personSchoolMerge IsMatch below line 496", "\n")


      personSchoolMerge <- left_join(peopleInHousehold, SchoolChosenDetail, by = "personAge") %>%
        mutate(IsMatch = ifelse(SchoolType == "C" | SchoolType == personType | SchoolType == "S", "Y", "N")) %>%
        filter(IsMatch == "Y") %>%
        select(-IsMatch)


      # pick the kids that will go to this school, using the max number that can be assigned
      # loop is only entered if there are more school spots than people
      # due to the join, this can only happen if there are multiple people who are the same age
      # and permitted same-sex combinations

      if(nrow(personSchoolMerge) > MaxpeopleCanTake) {

        # cat("There are", MaxpeopleCanTake, "for", nrow(personSchoolMerge), "school slots", "\n")

        TimesToRepeat <- 1 + nrow(personSchoolMerge) - MaxpeopleCanTake

        AgesToLoop <- personSchoolMerge %>%
          pull(personAge)

        # cat("nrow(personSchoolMerge is", nrow(personSchoolMerge), " MaxpeopleCanTake is",  MaxpeopleCanTake, "\n")
        # cat("Line 565 lines to repeat are", AgesToLoop, "\n")

        for(k in 1:TimesToRepeat) {
          youngest = AgesToLoop[k]
          oldest = AgesToLoop[k + MaxpeopleCanTake - 1]

           # cat("Youngest is", youngest, "and oldest is", oldest, "\n")
          AgeDiffs = oldest - youngest

          # print(AgeDiffs)

          if(k == 1) {
            SmallestVector <- k
            SmallestGap <- AgeDiffs

          }

          if (AgeDiffs < SmallestGap) {

            # cat("Loop entered, smallest gap is", SmallestGap, "smallest vector is", SmallestVector, "\n")

            SmallestVector <- k
            SmallestGap <- AgeDiffs

          }

          # cat("Start row for subset is", SmallestVector, "and end row for subset is", (SmallestVector+(MaxpeopleCanTake-1)), "\n")

          # closes for(k in 1:TimesToRepeat)
        }


        personSchoolMerge <- personSchoolMerge %>%
          slice(SmallestVector:(SmallestVector+(MaxpeopleCanTake-1)))


        # closes if(nrow(personSchoolMerge) > MaxpeopleCanTake)
      }


      # merge the school into the household data

      # do the allocation
      if(exists("peopleFinalised")) {

        # cat("people Finalised exists", "\n")

        peopleFinalised <- bind_rows(peopleFinalised, personSchoolMerge)

      } else {

        peopleFinalised <- personSchoolMerge

        # get column index for injection
        # only need to grab this the once, when the file is constructed
        schoolsIDColIndex <- as.numeric(which(colnames(peopleFinalised) == "SchoolID"))

        schoolsTypeColIndex <- as.numeric(which(colnames(peopleFinalised) == "SchoolType"))

        # closes  if(exists(peopleFinalised))
      }

      # reduce the relevant roll count for the school
      # do straight injection of updated count
      # this will update each time the person/ren are allocated
      # so the numbers will always be accurate

      # NOTE: the usual approach is to remove the allocated school from the set available
      # so it does not matter that the role counts aren't updated for that household
      # as the school will not be used again
      # and where the school has to be added in, the updated roll count is used

      SchoolCountSummaries <- personSchoolMerge %>%
        group_by(SchoolID, personAge) %>%
        summarise(AllocatedCounts = n())

      # print(CurrentHousehold)
      # print(str(SchoolCountSummaries))

      for(l in 1:nrow(SchoolCountSummaries)) {

        SchoolRowIndex <- as.numeric(which(schoolsRenamed$SchoolID==SchoolCountSummaries$SchoolID[l] &
                                             schoolsRenamed$personAge==SchoolCountSummaries$personAge[l]))

        schoolsRenamed[SchoolRowIndex, schoolsCountColIndex] <- schoolsRenamed[SchoolRowIndex, schoolsCountColIndex] -
          SchoolCountSummaries$AllocatedCounts[l]

        schoolsRenamed <- schoolsRenamed %>%
          filter(personCounts > 0)

        # closes for(l in 1:nrow(SchoolCountSummaries))
      }

      # remove people who are matched
      peopleInHousehold <- peopleInHousehold %>%
        filter(!(personID %in% c(personSchoolMerge$personID)))

      # cat("The people in household data are", "\n")
      # str(peopleInHousehold)

      # update for whether loop continues
      # cat("The number of people allocated was", nrow(personSchoolMerge), "\n")

      NumKidsRemaining <- NumKidsRemaining - nrow(personSchoolMerge)

      # cat("The number of kids remaining is", NumKidsRemaining, "\n")

      # update the maximum number that can be allocated to the same school

      # stops the recalculation of this information if there are no more people to assign to schools
      # put the code block in the loop below

      if (NumKidsRemaining > 0) {

        NumberSameSchool <- max(NumberSameSchool - nrow(personSchoolMerge), 1)

        AllschoolsFromWhichToChoose <- AllschoolsFromWhichToChoose %>%
          filter(!(SchoolID == SchoolChosen$SchoolID),
                 personAge %in% c(peopleInHousehold$personAge))

        if(is.na(AllschoolsFromWhichToChoose$personAge[1])) {

          personAges <- as.data.frame(peopleInHousehold %>%
                                       group_by(personType, personAge) %>%
                                       summarise(CountsByAge = n()))

          # construct a non-empty data frame if the schools went to zero because of school
          # age ranges and age ranges of people in the household

          AllschoolsFromWhichToChoose <- left_join(personAges, schoolsRenamed, by = "personAge") %>%
            mutate(IsMatch = ifelse(SchoolType == "C" | SchoolType == personType, "Y", "N")) %>%
            filter(personCounts > 0,
                   IsMatch == "Y") %>%
            group_by(SchoolID, personAge, SchoolType, personCounts) %>%
            summarise(NumberKids = sum(CountsByAge)) %>%
            ungroup() %>%
            mutate(Remainingpeople = personCounts - NumberKids)

          # str(personAges)
          # str(schoolsRenamed)
          # str(AllschoolsFromWhichToChoose)

          # if school slots are small relative to school population
          # and there are single-sex schools and co-ed schools
          # can end up with situation where the only school slot available is at a single-sex school
          # for the opposite sex
          # so fix by doing a straight swap

          if(is.na(AllschoolsFromWhichToChoose$personAge[1])) {

            # cat(CurrentHousehold, "Entered this final fixit loop", "\n")


            # str(schoolsRenamed)
            # sample one school per remaining person
            IntschoolsFromWhichToChoose <- left_join(personAges, schoolsRenamed, by = "personAge") %>%
              group_by(SchoolID) %>%
              slice(rep(1:n(), first(personCounts))) %>%
              left_join(peopleInHousehold, by = c("personAge", "personType")) %>%
              select(personID, SchoolID, personAge, personType, SchoolType) %>%
              group_by(personID) %>%
              slice_sample(n = 1) %>%
              ungroup()

            # cat("AllschoolsFromWhichToChoose", "\n")
            # pick one school to swap in for remaining person
            peopleToSwap <- peopleFinalised %>%
              filter(SchoolType == "C") %>%
              right_join(IntschoolsFromWhichToChoose, by = "personAge") %>%
              filter(!(personType.x == personType.y)) %>%
              group_by(personID.y) %>%
              slice_sample(n = 1) %>%
              ungroup()

            # process:
            # 1. swap school IDs
            # 2. sub in the new school ID for the donor person/people, replacing the original
            # 2.A must also change sex allocation
            # 3. create the correctly formatted person merge file for the recipient people
            # 4. add in those recipient people
            # 5. decrease the counts for the schools that were the incorrect sex
            InjectionInformation <- peopleToSwap %>%
              select(personID.x, SchoolID.y, SchoolType.y)

            for(m in 1:nrow(InjectionInformation)) {

              personRowIndex <- as.numeric(which(peopleFinalised$personID==InjectionInformation$personID.x[m]))

              peopleFinalised[personRowIndex, schoolsIDColIndex] <- InjectionInformation$SchoolID.y[m]
              peopleFinalised[personRowIndex, schoolsTypeColIndex] <- InjectionInformation$SchoolType.y[m]

              # closes for(m in 1:nrow(InjectionInformation))
            }

            # replacement school injected, now fix the person/people in the current household


            # 3. create the correctly formatted person merge file for the recipient people
            # get people and add the extra columns for merge
            # need to mock up these columns as the schools dataset no longer contains
            # this schoolID/personage combo
            # only incorrect value is SchoolType

            personSchoolMerge <- peopleToSwap %>%
              select(personID.y, SchoolID.x) %>%
              rename(personID = personID.y,
                     SchoolID = SchoolID.x) %>%
              left_join(peopleInHousehold, by = "personID") %>%
              mutate(SchoolType = "Z", personCounts = 0, NumberKids = 0, Remainingpeople = 0)

            # 4. add in those recipient people
            peopleFinalised <- bind_rows(peopleFinalised, personSchoolMerge)

            # 5. decrease the counts for the schools that were the incorrect sex
            # cat("Fixing school count summaries", "\n")

            SchoolCountSummaries <- peopleToSwap %>%
              select(SchoolID.y, personAge) %>%
              rename(SchoolID = SchoolID.y) %>%
              group_by(SchoolID, personAge) %>%
              summarise(AllocatedCounts = n()) %>%
              left_join(schoolsRenamed, by = c("SchoolID", "personAge")) %>%
              mutate(Remainingpeople = personCounts - AllocatedCounts)


            for(l in 1:nrow(SchoolCountSummaries)) {
              SchoolRowIndex <- as.numeric(which(schoolsRenamed$SchoolID==SchoolCountSummaries$SchoolID[l] &
                                                   schoolsRenamed$personAge==SchoolCountSummaries$personAge[l]))

              # cat("School row index is", SchoolRowIndex, "and the school column index is", schoolsCountColIndex, "\n")

              schoolsRenamed[SchoolRowIndex, schoolsCountColIndex] <- schoolsRenamed[SchoolRowIndex, schoolsCountColIndex] -
                SchoolCountSummaries$AllocatedCounts[l]

              schoolsRenamed <- schoolsRenamed %>%
                filter(personCounts > 0)

              # closes for(l in 1:nrow(SchoolCountSummaries))
            }

            # remove people who are matched


            peopleInHousehold <- peopleInHousehold %>%
              filter(!(personID %in% c(personSchoolMerge$personID)))

            # AllschoolsFromWhichToChoose <- bind_rows(SchoolID = "0", "personAge" = 0, "SchoolType" = "0",
            #                                          "personCounts" = 0, "NumberKids" = 0,
            #                                          "Remainingpeople" = 0)
            #
            # NumKidsRemaining <- 0

            # cat("The number of people remaining in the household is", nrow(peopleInHousehold), "\n")

            # add a person to a school when the addition will be an overcount
            if(nrow(peopleInHousehold) > 0) {

              # cat("The number of people in the household is", list(peopleInHousehold$personID), "\n")

              # options are:
              # 1. school already used
              # 2. an equivalent same-sex school already used
              # 3. no school used and need to match new school

              # check to see if there is any school that matches the ones the others are in
              schoolsAlreadyUsed <- peopleFinalised %>%
                filter(HouseholdID == CurrentHousehold) %>%
                select(SchoolID) %>%
                unique() %>%
                left_join(OriginalschoolsCounts, by = "SchoolID") %>%
                filter(SchoolAge %in% peopleInHousehold$personAge,
                       personCounts > 0)

              # Option 3. no school used and need to match new school
              if(nrow(schoolsAlreadyUsed) == 0) {

                FinalSchoolMatch <- peopleInHousehold %>%
                  left_join(OriginalschoolsCounts, by = c("personAge" = "SchoolAge")) %>%
                  filter((SchoolType == "C" | SchoolType == personType) &
                           personCounts > 0)

                for (m in 1: nrow(peopleInHousehold)) {

                  SchoolThatMatched <- FinalSchoolMatch %>%
                    filter(personID == peopleInHousehold$personID[m]) %>%
                    slice_sample(weight_by = personCounts, n=1)

                  peopleFinalised <- bind_rows(peopleFinalised, SchoolThatMatched)

                  # closes for (m in 1: nrow(peopleInHousehold))
                }

                # closes if(nrow(schoolsAlreadyUsed) == 0)
              } else {

                # NOT TESTED

                # options remaining are
                # 1. school already used
                # 2. an equivalent same-sex school already used

                # get the age matches
                InterimSchoolMatches <- peopleFinalised %>%
                  filter(HouseholdID == CurrentHousehold) %>%
                  select(SchoolID) %>%
                  unique() %>%
                  filter(SchoolAge %in% peopleInHousehold$personAge,
                         personCounts > 0)

                # loop through the people matched to no school

                for (n in 1: nrow(peopleInHousehold)) {

                  Currentperson <- peopleInHousehold[n,]

                  SchoolMatchOptions <- Currentperson %>%
                    left_join(OriginalschoolsCounts, by = c("personAge" = "SchoolAge")) %>%
                    filter(SchoolID %in% c(InterimSchoolMatches$SchoolID))

                  #  # option
                  # 1. school already used, same-sex

                  if(Currentperson$personType %in% c(SchoolMatchOptions$SchoolType)) {

                    # remove matches that are opposite sex
                    SchoolMatchOptions <- SchoolMatchOptions %>%
                      filter(SchoolType == Currentperson$personType)

                    # random sample one of the schools and add person to the final dataframe

                    SchoolThatMatched <- SchoolMatchOptions %>%
                      slice_sample(weight_by = personCounts, n=1)

                    peopleFinalised <- bind_rows(peopleFinalised, SchoolThatMatched)

                    # closes if(SchoolMatchOptions %in% SchoolType == "C" | SchoolType == personType)

                  } else {

                    # no matching single-sex schools
                    # look at opposite-sex single-sex school

                    SchoolMatchOptionsSexes <- SchoolMatchOptions %>%
                      filter(!(SchoolType == Currentperson$personType) &
                               !(SchoolType == "C"))

                    # deal with the situation where there are schools of different types
                    if (nrow(SchoolMatchOptionsSexes) > 0) {

                      SchoolThatMatched <- SchoolMatchOptions %>%
                        slice_sample(weight_by = personCounts, n=1)

                      peopleFinalised <- bind_rows(peopleFinalised, SchoolThatMatched)

                      # there may be no single-sex schools that match the sex of the person
                      # which means only co-ed schools left

                      # closes if (nrow(SchoolMatchOptionsSexes) > 0)
                    } else {

                      # for the co-ed only schools
                      SchoolThatMatched <- SchoolMatchOptions %>%
                        filter(SchoolType == "C") %>%
                        slice_sample(weight_by = personCounts, n=1)

                      peopleFinalised <- bind_rows(peopleFinalised, SchoolThatMatched)

                      # closes else to if (nrow(SchoolMatchOptionsSexes) > 0)
                    }

                    # closes else to if(Currentperson$personType %in% c(SchoolMatchOptions$SchoolType))
                  }

                  # cat("The maximum people can take on line 952 is", MaxpeopleCanTake, "\n")
                  # cat("The min number same school is", min(NumberSameSchool), "The other is",
                  # max(AllschoolsFromWhichToChoose$NumberKids), "\n")

                  # closes for (n in 1: nrow(peopleInHousehold))
                }

                # closes else for if(nrow(schoolsAlreadyUsed) == 0)
              }


              # closes if(nrow(peopleInHousehold) > 0)
            }

            # closes INSIDE if(is.na(AllschoolsFromWhichToChoose$personAge[1]))
          }

          # close if(is.na(AllschoolsFromWhichToChoose$personAge[1]))
        }

        # print(CurrentHousehold)

        # str(AllschoolsFromWhichToChoose)

        NumberKidsPerSchool <- NumberKidsPerSchool %>%
          filter(SchoolID %in% c(AllschoolsFromWhichToChoose$SchoolID))

        # cat("The number same school is", NumberSameSchool, "\n")

        # something here about the random roll - number of kids already allocated versus max number kids can take
        # need to update random roll, taking into account of the number of kids already assigned

        # cat("The remaining schools from which to choose are", nrow(AllschoolsFromWhichToChoose), "\n")

        MaxpeopleCanTake <- max((min(NumberSameSchool, max(AllschoolsFromWhichToChoose$NumberKids))), 1)


        # closes if (NumKidsRemaining > 0)
      }


      # closes  while(NumKidsRemaining > 0)
    }


    # closes  if(NumberSameSchool > 1)
    # }


    # closes  for(i in 1:nrow(peopleRenamed))

  }


  #

  # add in the out-of-age people

  # need to test the school ID input - whether character or numeric

  print(is.numeric(pplstcol))

  if (is.numeric(pplstcol) == TRUE) {
    NotInSchool <- NotInSchool %>%
    filter(!(personID %in% c(peopleRenamed$personID))) %>%
    mutate(SchoolID = 0) %>%
    rename(!!pplidcolName := personID, !!pplagecolName := personAge,
           !!hhidcolName := HouseholdID, !!pplsxcolName := personType,
           !!schidcolName := SchoolID)

    OutputDataframe <- peopleFinalised %>%
      mutate(SchoolID = as.numeric(SchoolID)) %>%
      select(-c(SchoolType, personCounts, NumberKids, Remainingpeople)) %>%
      rename(!!pplidcolName := personID, !!pplagecolName := personAge,
             !!hhidcolName := HouseholdID, !!pplsxcolName := personType,
             !!schidcolName := SchoolID)

} else {
  cat("Entered factor loop", "\n")

  NotInSchool <- NotInSchool %>%
    filter(!(personID %in% c(peopleRenamed$personID))) %>%
    mutate(SchoolID = "0") %>%
    rename(!!pplidcolName := personID, !!pplagecolName := personAge,
           !!hhidcolName := HouseholdID, !!pplsxcolName := personType,
           !!schidcolName := SchoolID)


  OutputDataframe <- peopleFinalised %>%
    select(-c(SchoolType, personCounts, NumberKids, Remainingpeople)) %>%
    rename(!!pplidcolName := personID, !!pplagecolName := personAge,
           !!hhidcolName := HouseholdID, !!pplsxcolName := personType,
           !!schidcolName := SchoolID)

}



  OutputDataframe <- bind_rows(OutputDataframe, NotInSchool)

  cat("The school allocation by sex is", "\n")
  print(table(peopleFinalised$SchoolID, peopleFinalised$Sex))

  return(OutputDataframe)
  # closes function
}
