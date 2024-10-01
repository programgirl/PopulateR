#' Create a data frame of people matched to schools
#' This function creates a data frame of people and matching schools. By default, all similarly-aged students in the same household will be matched to the same school. If one student is matched to a same-sex school, then all similarly aged students will also be matched to a same-sex school. This includes opposite-sex children.
#' Two data frames are required: one for the people ("people) and one for the schools ("schools").
#' In the "people" data frame, a numeric or ordered factor for school status is required. The smallest value/level will be treated as the code for non-students. If one value is used, everyone in the data frame will be allocated a school. Thus, pre-cleaning a data frame is not required. 
#' The "schools" data frame must be a summary in the form of roll counts by age within school. Each row is one age only. For example, if a school has children aged 5 to 9 years, there should be 5 rows. Any combination of co-educational and single-sex schools can be used,  and the relevant value must be on each row of the \enquote*{schools} data frame. 
#' The minimum and maximum school ages, followed by the achieved counts by sex for each school, are printed to the console.
#'
#' @export
#' @param people A data frame containing individual people.
#' @param pplid The variable containing the unique identifier for each person, in the people data frame.
#' @param pplage The variable containing the ages, in the people data frame. 
#' @param pplsx The variable containing the codes for sex, in the people data frame. 
#' @param pplst The school status variable in the people data frame. Only two numeric values/factor levels can be used. The smallest number/level is the code for people not in school.
#' @param hhid The household identifier variable, in the people data frame.
#' @param schools A data frame containing the schools.
#' @param schid  The variable containing the unique identifier for each school, in the schools data frame.
#' @param schage The variable containing the ages, in the schools data frame. 
#' @param schroll The variable containing the number of places available for people at that school age, within the school.
#' @param schtype The variable that indicates whether the school is co-educational or single-sex. The expected value for a co-educational school is "C". The codes for female and male must be the same as in the people data frame.
#' @param schmiss The school identifier value that will be given to those people not in school. If left blank, the default value is 0. If the school IDs are numeric in the schools data frame, a numeric missing value must be supplied.
#' @param sameprob The probability that students from the same household will be at the same school, given age (and sex if there are same-sex schools). Results depend on the number of students in each household, and student ages, combined with the sizes of the school rolls. Value must be between 0 and 1. The default value is 1.
#' @param userseed If specified, this will set the seed to the number provided. If not,the normal set.seed() function will be used.
#' 
#' @return Two data frames, as a list. $Population contains the synthetic population with the schools added. $Schools contains the remaining roll counts for the schools.
#'
#' @examples
#' library(dplyr)
#' SchoolsAdded <- schooladd(IntoSchools, pplid = 3, pplage = 4, pplsx = 8,
#'                           pplst = 6,  hhid = 7, SchoolsToUse, schid = 2,
#'                           schage = 4, schroll = 5, schtype = 3,
#'                           userseed = 4)

pairschoolwt <- function(people, pplid, pplage, pplsx, pplst = NULL, hhid = NULL, schools, schid, schage, schroll, schtype, 
                       schmiss = 0, sameprob = 1, userseed=NULL)
{
  
  options(dplyr.summarise.inform=F)
  options(warn=1)
  
  statcolName <- sym(names(people[pplst]))
  schidcolName <- sym(names(schools[schid]))
  
  # content check
  if (!(is.factor(people[[statcolName]])) & !(is.numeric(people[[statcolName]]))) {
    stop("The school status variable must be a factor or be numeric.")
  }
  
  if(is.numeric(schools[[schidcolName]]) & !(is.numeric(schmiss))) {
    stop("The School ID for people not in school must be numeric. Type mismatch with School ID.")
  }
  
  if(!(between(sameprob, 0, 1))) {
    stop("The probability of being in the same school must be a value between 0 and 1.")
  }
  
  
  ###############################################
  ###############################################
  # get the col names
  ###############################################
  ###############################################
  
  # people data
  
  pplidcolName <- sym(names(people[pplid]))
  pplagecolName <- sym(names(people[pplage]))
  pplsexcolName <- sym(names(people[pplsx]))
  hhidcolName <- sym(names(people[hhid]))
  
  # school data
  
  schidcolName <- sym(names(schools[schid]))
  schagecolName <- sym(names(schools[schage]))
  schrollcolName <- sym(names(schools[schroll]))
  schtypecolName <- sym(names(schools[schtype]))
  
  #####################################################################
  #####################################################################
  # Test for any problem ages, stop function if this situation exists
  #####################################################################
  #####################################################################
  
  peopleRenamed <- people %>%
    rename(personID = !! pplidcolName, 
           personAge = !! pplagecolName, 
           personType = !! pplsexcolName,
           HouseholdID = !! hhidcolName, 
           schStat = !! statcolName)
  
  NotInSchool <- peopleRenamed
  
  # can only take two values for school status variable
  
  TestLevels <- peopleRenamed %>%
    select(schStat) %>%
    group_by(schStat) %>%
    summarise(Nums = n()) %>%
    pull(schStat)
  
  if(length(TestLevels) > 2) {
    stop("The school status variable must contain a maximum of two values.")
  }
  
  
  schoolsRenamed <- schools %>%
    rename(schoolID = !! schidcolName, 
           schoolAge = !! schagecolName,
           personCounts = !! schrollcolName, 
           schoolType = !! schtypecolName) %>%
    mutate(schoolID = as.character(schoolID),
           schoolType = as.character(schoolType),
           originalCounts = personCounts) %>%
    #mutate(across(where(is.factor), as.character)) %>%
    select(schoolID, schoolAge, personCounts, schoolType, originalCounts)
  
  OriginalschoolsCounts <- schoolsRenamed
  
  
  
  #####################################################################
  #####################################################################
  # Test if sex codes in the two data frames are the same
  #####################################################################
  #####################################################################
  
  peopleCodeTest <- peopleRenamed %>%
    select(personType) %>%
    unique()
  
  schoolsCodeTest <- schoolsRenamed %>%
    select(schoolType) %>%
    unique()
  
  # are they the same
  SexTest <- merge(peopleCodeTest, schoolsCodeTest, 
                   by.x = c("personType"),
                   by.y = c("schoolType"))
  
  SexTestRows <- as.numeric(nrow(SexTest))
  
  
  if(SexTestRows < 1) {
    
    stop("The sex variable codes in the people data frame do not match the codes in the schools data frame.")
  }
  
  
  ###############################################
  ###############################################
  # quick test of compatibility of counts
  ###############################################
  ###############################################
  
  
  schoolsCountTest <- schoolsRenamed %>%
    group_by(schoolAge) %>%
    summarise(schoolAgeCount = sum(personCounts))
  
  ###############################################
  # test if there is only one factor level, i.e. all kids to assign
  ###############################################
  
  # print(length(TestLevels))
  
  if(length(TestLevels) == 1) {
    
    peopleCountTest <- peopleRenamed %>%
      group_by(personAge) %>%
      summarise(AgeCount = n())
    
    CountComparison <- full_join(peopleCountTest, schoolsCountTest, by = c("personAge" = "schoolAge")) %>%
      mutate(AgeCount = replace(AgeCount, is.na(AgeCount), 0),
             schoolAgeCount = replace(schoolAgeCount, is.na(schoolAgeCount), 0),
             CountDiff = schoolAgeCount - AgeCount) %>%
      filter(schoolAgeCount != 0, AgeCount != 0)
    
    TooManyKids <- CountComparison %>%
      filter(CountDiff < 0) %>%
      select(personAge)
    
    # need to construct the reduced data frame on the basis of age at school
    
  } else {
    
    # cat("Entered multiple factor loop", "\n")
    # get min factor level to exclude
    
    MinFactorLevel <- min(as.integer(peopleRenamed$schStat))
    
    # print(MinFactorLevel)
    
    NotFactor <- peopleRenamed %>%
      filter(as.integer(schStat) == as.integer(MinFactorLevel))
    
    # cat("The notfactor data frame has this number of rows", nrow(NotFactor), "\n")
    
    # cat("The number of rows in original peoplerenamed is", nrow(peopleRenamed), "\n")
    
    peopleRenamed <- peopleRenamed %>%
      filter(!(personID %in% NotFactor$personID))
    
    # cat("The revised count after removing wrong factor level is", nrow(peopleRenamed), "\n")
    
    peopleCountTest <- peopleRenamed %>%
      group_by(personAge) %>%
      summarise(AgeCount = n())
    
    CountComparison <- full_join(peopleCountTest, schoolsCountTest, by = c("personAge" = "schoolAge")) %>%
      mutate(AgeCount = replace(AgeCount, is.na(AgeCount), 0),
             schoolAgeCount = replace(schoolAgeCount, is.na(schoolAgeCount), 0),
             CountDiff = schoolAgeCount - AgeCount) %>%
      filter(schoolAgeCount != 0, AgeCount != 0)
    
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
  
  MaxschoolAge <- as.numeric(CountComparison[nrow(CountComparison), 1])
  
  cat("The minimum school age is", as.numeric(CountComparison[1,1]), "and the maximum school age is", as.numeric(CountComparison[nrow(CountComparison), 1]), "\n")
  
  
  # restrict person and school data frames to these minimum and maximum ages
  # is done for multiple factor levels too, just in case the school age range is incompatible
  # get age range
  AgeRestriction <- CountComparison %>%
    select(personAge)
  
  # apply to people
  
  peopleRenamed <- left_join(AgeRestriction, peopleRenamed, by = "personAge")
  
  
  # NOTE: this removes any school classrooms where NO people of that age exist in the data
  schoolsRenamed <- left_join(AgeRestriction, schoolsRenamed, by = c("personAge" = "schoolAge"))
  
  # schoolsCountColIndex <- as.numeric(which(colnames(schoolsRenamed) == "personCounts"))
  
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
  # the availability of places
  #####################################################################
  
  peopleSexAge <- peopleRenamed %>%
    group_by(personType, personAge) %>%
    summarise(AgeCount = n())
  
  schoolsexAge <- schoolsRenamed %>%
    group_by(schoolType, personAge) %>%
    summarise(schoolAgeCount = sum(personCounts))
  
  ListofHouseholds <- peopleRenamed %>%
    group_by(HouseholdID) %>%
    summarise(numberKids = n())
  
  
  #####################################################################
  # sort by descending
  #####################################################################
  
  ListofHouseholds <- ListofHouseholds %>%
    arrange(desc(numberKids))
  
  #####################################################################
  # igraph function to match kids into different schools
  #####################################################################
  
  kidsAdd <- function(theDF) {
    # generate bipartite match
    theGraph <- igraph::graph_from_data_frame(theDF) %>%
      # igraph::set_vertex_attr(name = "type", value = names(igraph::V(.)) %in% theDF$personAge)
      igraph::set_vertex_attr(name = "type", value = names(igraph::V(.)) %in% theDF$personID)
    
    # max bipartite match
    maxbm <- na.omit(igraph::max_bipartite_match(theGraph)$matching)
    
    # retrieve match pattern and yield output
    # v1 <- maxbm[maxbm %in% theDF$personAge]
    v1 <- maxbm[maxbm %in% theDF$personID]
    v2 <- names(v1)
    # matchedSchools <- data.frame("personAge" = c(v1), schoolID = c(v2)
    data.frame(
      # personAge = `class<-`(v1, class(theDF$personAge)),
      personID = `class<-`(v1, class(theDF$personID)),
      schoolID = `class<-`(v2, class(theDF$schoolID))
    )
    
  }
  
  
  ###############################################
  ###############################################
  ###############################################
  # assignment of schools
  ###############################################
  ###############################################
  ###############################################
  
  # this is the first part of the code that requires randomness
  # so seed is applied here
  
  if (!is.null(userseed)) {
    set.seed(userseed)
  }
  
  for(i in 1:nrow(ListofHouseholds)) {
    
    
    CurrentHousehold <- ListofHouseholds$HouseholdID[i]
    
    # cat("Current household is", CurrentHousehold, "\n")
    
    
    # get the people in the household
    
    peopleInHousehold <- peopleRenamed %>%
      filter(HouseholdID == CurrentHousehold)
    
    NumKids <- as.numeric(nrow(peopleInHousehold))
    
    # cat("Number in household is", NumKids, "and number available in schools", sum(schoolsRenamed$personCounts), "\n")
    
    # add people to same school
    # need to identify the number of people that can go to the same school
    
    # get person ages dataframe
    personAges <- peopleInHousehold %>%
      group_by(personType, personAge) %>%
      summarise(CountsByAge = n())
    
    # need a test to see if more than one age
    personAgesMaxCount <- peopleInHousehold %>%
      group_by(personAge) %>%
      summarise(CountsByAge = n())
    
    
    
    
    
    
    
    
    
    
    
    
    ###############################################
    ###############################################
    # one-child households
    ###############################################
    ###############################################
    
    if(NumKids==1) {
      
      # cat("The number of kids is 1 and household is", CurrentHousehold, "\n")
      
      # locate schools that can take the maximum number of people from NumberSameSchool down
      
      # have to do some special work when there are single-sex schools as well as co-end
      
      schoolsubset <- left_join(peopleInHousehold, schoolsRenamed, by = "personAge",
                                relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
        mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
        filter(personCounts > 0,
               isMatch == "Y") %>%
        mutate(remainingPeople = personCounts - 1)
      
      
      if(nrow(schoolsubset) > 0) {

          schoolMatch <- schoolsubset %>%
            slice_sample(n=1, weight_by = personCounts, replace = FALSE) %>%
            select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))

      } else {
        
        # all schools have 0 roll slots
        
        cat("There were not enough school roll places available for household", CurrentHousehold, "extra roll place added \n")
        
        
        schoolMatch <- left_join(peopleInHousehold, schoolsRenamed, by = "personAge",
                                 relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
          mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
          filter(isMatch == "Y") %>%
          slice_sample(n=1, weight_by = originalCounts, replace = FALSE) %>%
          select(-c(personCounts, schoolType, originalCounts, isMatch))
        
        # closes else to if(nrow(schoolsubset) > 0) {
      }
      
      # decrease the school roll count by 1
      # just mutate the school roll count
      # need to ensure that the count never goes to zero otherwise may have the problem of child counts
      # exceeding available school roll places
      
      schoolInfo <- schoolMatch %>%
        ungroup() %>%
        select(schoolID, personAge)
      
      schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
        mutate(personCounts = personCounts - 1,
               personCounts = ifelse(personCounts < 0, 0, personCounts)) 
      
      schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
        select(schoolID, personAge, personCounts, schoolType, originalCounts)
      
      # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")
      
      schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
      
      
      if(exists("ChildSchoolMatches")) {
        
        ChildSchoolMatches <- bind_rows(ChildSchoolMatches, schoolMatch)
        
        # closes if(exists("ChildSchoolMatches")) {
        
      } else {
        
        ChildSchoolMatches <- schoolMatch
        
        # closes else to if(exists("ChildSchoolMatches")) {
      }
      
      # closes if(NumKids==1) {
      
      
      
    } else {
      
      
      
      
      
      ###############################################
      ###############################################
      # there is more than one kid in the household
      ###############################################
      ###############################################
      
      # cat("The household is", CurrentHousehold, "max person ages is", max(personAges$CountsByAge), "\n")
      
      
      schoolsubset <- left_join(personAges, schoolsRenamed, by = "personAge",
                                relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
        mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
        select(-CountsByAge) %>%
        filter(personCounts > 0,
               isMatch == "Y") %>%
        mutate(remainingPeople = personCounts - 1) %>%
        filter(remainingPeople > -1)
      
      # need to split the allocation based on sameprob
      
      if(sameprob > 0 & sameprob < 1) {
        
        probSelected <- runif(1, 0, 1)
        
        # cat("probSelected is", probSelected, "\n")
        
        
        if(probSelected <= sameprob) {
          
          probUsed <- 1
          
        } else {
          
          probUsed <- 0
        }
        
        # closes if(sameprob > 0 & sameprob < 1) {
      } else {
        
        probUsed <- 0
      }
      
      
      # cat("Probability used is", probUsed, "\n")
      
      
      if(probUsed == 1) {
        
        ###############################################
        ###############################################
        # kids go to the same school
        ###############################################
        ###############################################
        
        # cat("Household ID is", CurrentHousehold, "Children will be going to the same school \n")
        
        ###############################################
        ###############################################
        # start single sex schools code block
        ###############################################
        ###############################################
        
        
        schoolTypeCheck <- schoolsubset %>%
          group_by(schoolType) %>%
          distinct(schoolType)
        
        
        if(nrow(schoolTypeCheck) > 1) {
          
          
          # create a new variable that is a not-same sex versus same-sex marker
          
          sexedsubset <- schoolsubset %>%
            mutate(SexDual = ifelse(schoolType == "C", "O", "S"))
          
          # put the single sex schools into one subset
          # has count by age only, no school ID, no person ID
          
          SingleSexSubAges <- sexedsubset %>%
            filter(SexDual == "S") %>%
            group_by(personType, personAge) %>%
            slice_max(remainingPeople) %>%
            group_by(schoolID) %>%
            summarise(NumKidsSpots = n()) %>%
            ungroup()
          
          SingleSexSubRolls <- sexedsubset %>%
            filter(SexDual == "S") %>%
            group_by(schoolID) %>%
            summarise(RollSpots = sum(remainingPeople)) %>%
            ungroup()
          
          SingleSexSub <- left_join(SingleSexSubAges, SingleSexSubRolls, by = c("schoolID"))
          
          
          SingleSexNum <- as.numeric(sum(SingleSexSub$NumKidsSpots))
          SingleSexWeight <- as.numeric(sum(SingleSexSub$RollSpots))
          
          # cat("The kids in single sex schools can be", SingleSexNum, "with", SingleSexWeight, "roll places \n")
          
          # the opposite sex ones in another
          # has count by school ID, no ages
          OppSexSubAges <- sexedsubset %>%
            filter(SexDual == "O") %>%
            group_by(schoolID) %>%
            summarise(NumKidsSpots = n()) %>%
            ungroup()
          
          OppSexSubRolls <- sexedsubset %>%
            filter(SexDual == "O") %>%
            group_by(schoolID) %>%
            summarise(RollSpots = sum(remainingPeople)) %>%
            ungroup()
          
          OppSexSub <- left_join(OppSexSubAges, OppSexSubRolls, by = c("schoolID"))
          
          # select the opposite sex school with the largest number of kids and then the largest roll count
          
          # get the max number of kids in the same opposite sex school
          OppSexNum <- as.numeric(max(OppSexSub$NumKidsSpots))
          
          
          SelectedOppSexSchool <- OppSexSub %>%
            filter(NumKidsSpots == OppSexNum) %>%
            arrange(desc(RollSpots)) %>%
            slice_head(n=1)
          
          OppSexWeight <- as.numeric(SelectedOppSexSchool$RollSpots)
          
          # get the school with the max number of kids
          # also, random select if the max number is the same
          
          # cat("The kids in co-ed schools can be", OppSexNum, "with", OppSexWeight, "roll places \n")
          
          
          
          
          
          
          ###############################################
          # identify which loop to use: place single sex first, or opposite sex
          ###############################################
          
          if(SingleSexNum > OppSexNum) {
            
            Variant <- 1
            
          } else if (OppSexNum > SingleSexNum) {
            
            Variant <- 2
            
          } else if (SingleSexNum == OppSexNum) { # & SingleSexWeight >= OppSexWeight) {

            Variant <- sample(c(1, 2), size = 1, prob=c(SingleSexWeight, OppSexWeight))


          } else {
            
            stop("All variant conditions not defined")
          }
          
          if(Variant == 1) {
            
            # cat("Entered SS > OS loop \n")
            
            # get the single-sex schools
            # many-to-many needed to account for twin households
            
            InitialMatches <- schoolsubset %>%
              filter(schoolID %in% c(SingleSexSub$schoolID)) %>%
              select(-c(personCounts, originalCounts, remainingPeople)) %>%
              left_join(peopleInHousehold, by = c("personAge", "personType"), relationship = "many-to-many") %>%
              select(-c(schoolType, isMatch))
            
            # make sure ALL single sex options are assigned
            while(nrow(InitialMatches) > 0) {
              
              schoolInfo <- InitialMatches %>%
                ungroup() %>%
                select(schoolID, personAge, personType) %>%
                group_by(schoolID, personAge, personType) %>%
                summarise(KidsPerAge = n())
              
              numKidsBySchool <- schoolInfo %>%
                group_by(schoolID, personType) %>%
                summarise(numberKids = sum(KidsPerAge))
              
              HowManySexes <- as.numeric(nrow(numKidsBySchool %>%
                                                ungroup() %>%
                                                select(personType) %>%
                                                distinct(personType)))
              
              if(HowManySexes > 2) {
                stop("HMS > 2")
              }
              
              
              # select school with the greatest number of kids
              selectedSchool <- numKidsBySchool %>%
                ungroup() %>%
                slice_max(numberKids) %>%
                slice_sample(n=1) %>%
                pull(schoolID)
              
              # print(selectedSchool)
              
              # # add the children to the school
              InitialMatchesA <- InitialMatches %>%
                filter(schoolID == selectedSchool)
              
              # # get the kids counts by age for the selected school
              MatchingInfo1 <- schoolInfo %>%
                ungroup() %>%
                filter(schoolID == selectedSchool) %>%
                select(-personType)
              
              
              # update the selected schools counts and rejoin with the non-selected schools
              # to reconstruct schoolsRenamed with the correctly decremented counts
              
              schoolsSelected1 <- left_join(MatchingInfo1, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                mutate(personCounts = personCounts - KidsPerAge,
                       personCounts = ifelse(personCounts < 0, 0, personCounts)) %>%
                select(-KidsPerAge)
              
              schoolsNotSelected1 <- anti_join(schoolsRenamed, MatchingInfo1, by = c("schoolID", "personAge")) %>%
                select(schoolID, personAge, personCounts, schoolType, originalCounts)
              
              schoolsRenamed <- bind_rows(schoolsNotSelected1, schoolsSelected1)
              
              # add the kids to the children-matched-with-schools data frame
              
              if(exists("ChildSchoolMatches")) {
                
                ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatchesA)
                
                # closes if(exists("ChildSchoolMatches")) {
                
              } else {
                
                ChildSchoolMatches <- InitialMatchesA
                
                # closes else to if(exists("ChildSchoolMatches")) {
              }
              
              # remove the matched children from the household children data frame
              
              InitialMatches <- InitialMatches %>%
                filter(!personID %in% InitialMatchesA$personID)
              
              # closes while(nrow(InitialMatches) > 0) {
            }
            
            # handle children who are not going to a single-sex school
            
            remainingAges <- peopleInHousehold %>%
              filter(!personID %in% ChildSchoolMatches$personID)
            
            # add any remaining children to schools
            
            while(nrow(remainingAges) > 0) {

              # get the schools, only co-ed left
              
              schoolsubset <- left_join(remainingAges, schoolsRenamed, by = "personAge",
                                        relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
                mutate(isMatch = ifelse(schoolType == "C", "Y", "N"))  %>%
                filter(personCounts > 0,
                       isMatch == "Y") %>%
                mutate(remainingPeople = personCounts - 1) %>%
                filter(remainingPeople > -1)
              
              schoolSubAges <- schoolsubset %>%
                group_by(schoolID) %>%
                summarise(NumKidsSpots = n()) %>%
                ungroup()
              
              schoolSubRolls <- schoolsubset %>%
                group_by(schoolID) %>%
                summarise(RollSpots = sum(remainingPeople)) %>%
                ungroup()
              
              schoolSub <- left_join(schoolSubAges, schoolSubRolls, by = c("schoolID"))
              
              
              # select the opposite sex school with the largest number of kids and then the largest roll count
              # get the max number of kids in the same opposite sex school
              
              maxNumKids <- as.numeric(max(schoolSub$NumKidsSpots))
              
              
              selectedSchool <- schoolSub %>%
                filter(NumKidsSpots == maxNumKids) %>%
                arrange(desc(RollSpots)) %>%
                slice_head(n=1) %>%
                select(schoolID) %>%
                pull(schoolID)
              
              # put children matched to a school inside a temporary data frame
              
              MatchesB <- schoolsubset %>%
                filter(schoolID == selectedSchool) %>%
                select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))
              
              # get kids per age per school for decrementing the schoolsRenamed data frame
              
              schoolInfo <- MatchesB %>%
                ungroup() %>%
                select(schoolID, personAge) %>%
                group_by(schoolID, personAge) %>%
                summarise(KidsPerAge = n())
              
              # decrement the school(s) roll counts
              
              schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                mutate(personCounts = personCounts - KidsPerAge,
                       personCounts = ifelse(personCounts <= 0, 0, personCounts)) %>%
                select(-KidsPerAge)
              
              schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                select(schoolID, personAge, personCounts, schoolType, originalCounts)
              
              schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
              
              
              if(exists("ChildSchoolMatches")) {
                
                ChildSchoolMatches <- bind_rows(ChildSchoolMatches, MatchesB)
                
                # closes if(exists("ChildSchoolMatches")) {
                
              } else {
                
                ChildSchoolMatches <- bind_rows(MatchesB)
                
                # closes else to if(exists("ChildSchoolMatches")) {
              }
              
              remainingAges <- remainingAges %>%
                filter(!personID %in% MatchesB$personID)
              
              # closes while(nrow(remainingAges) > 0) {
            }
            
            
            # ends if(Variant == 1) {
          } else {
            
            # cat("Entered OS > SS \n")
            
            
            selectedSchoolID <- SelectedOppSexSchool %>%
              select(schoolID) %>%
              pull(schoolID)
            
            InitialMatches <- schoolsubset %>%
              filter(schoolID == selectedSchoolID) %>%
              ungroup() %>%
              select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople)) %>%
              left_join(peopleInHousehold, by = c("personAge", "personType"))
            
            schoolInfo <- InitialMatches %>%
              ungroup() %>%
              select(schoolID, personAge) %>%
              group_by(schoolID, personAge) %>%
              summarise(KidsPerAge = n())
            
            schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
              mutate(personCounts = personCounts - KidsPerAge,
                     personCounts = ifelse(personCounts < 0, 0, personCounts)) %>%
              select(-KidsPerAge)
            
            schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
              select(schoolID, personAge, personCounts, schoolType, originalCounts)
            
            schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
            
            remainingAges <- peopleInHousehold %>%
              filter(!personID %in% c(InitialMatches$personID))
            
            if(exists("ChildSchoolMatches")) {
              ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)
              # closes if(exists("ChildSchoolMatches")) {
            } else {
              ChildSchoolMatches <- InitialMatches
              # closes else to if(exists("ChildSchoolMatches")) {
            }
            
            
            while(nrow(remainingAges) > 0) {
              
              # add the schools to the remaining children
              
              schoolsubset <- left_join(remainingAges, schoolsRenamed, by = "personAge",
                                        relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
                mutate(isMatch = ifelse(schoolType == "C", "Y", "N"))  %>%
                filter(personCounts > 0,
                       isMatch == "Y") %>%
                mutate(remainingPeople = personCounts - 1) %>%
                filter(remainingPeople > -1)
              
              schoolSubAges <- schoolsubset %>%
                group_by(schoolID) %>%
                summarise(NumKidsSpots = n()) %>%
                ungroup()
              
              schoolSubRolls <- schoolsubset %>%
                group_by(schoolID) %>%
                summarise(RollSpots = sum(remainingPeople)) %>%
                ungroup()
              
              schoolSub <- left_join(schoolSubAges, schoolSubRolls, by = c("schoolID"))
              
              
              # select the school with the largest number of kids and then the largest roll count
              
              # get the max number of kids in the same opposite sex school
              
              maxNumKids <- as.numeric(max(schoolSub$NumKidsSpots))
              
              # get the school
              
              selectedSchoolID <- schoolSub %>%
                filter(NumKidsSpots == maxNumKids) %>%
                arrange(desc(RollSpots)) %>%
                slice_head(n=1) %>%
                pull(schoolID)
              
              # match the kids and update the school rolls
              
              MatchesB <- schoolsubset %>%
                filter(schoolID == selectedSchoolID) %>%
                select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))
              
              schoolInfo <- MatchesB %>%
                ungroup() %>%
                select(schoolID, personAge) %>%
                group_by(schoolID, personAge) %>%
                summarise(KidsPerAge = n())
              
              schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                mutate(personCounts = personCounts - KidsPerAge,
                       personCounts = ifelse(personCounts <= 0, 0, personCounts)) %>%
                select(-KidsPerAge)
              
              schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                select(schoolID, personAge, personCounts, schoolType, originalCounts)
              
              schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
              
              # add kids to updating children plus school data frame
              
              if(exists("ChildSchoolMatches")) {
                ChildSchoolMatches <- bind_rows(ChildSchoolMatches, MatchesB)
                # closes if(exists("ChildSchoolMatches")) {
              } else {
                ChildSchoolMatches <- bind_rows(MatchesB)
                # closes else to if(exists("ChildSchoolMatches")) {
              }
              
              # update remaining children data frame 
              
              remainingAges <- remainingAges %>%
                filter(!personID %in% c(MatchesB$personID))
              
              # closes while(nrow(remainingAges) > 0) {
            }
            
            # closes else to if(Variant == 1) {
          }
          
          
          # ends if(nrow(schoolTypeCheck) > 1) {
        } else {
          # there is only one type of school: F, C, or M  
          
          
          schoolSubAges <- schoolsubset %>%
            group_by(schoolID) %>%
            summarise(NumKidsSpots = n()) %>%
            ungroup()
          
          schoolSubRolls <- schoolsubset %>%
            group_by(schoolID) %>%
            summarise(RollSpots = sum(remainingPeople)) %>%
            ungroup()
          
          schoolSub <- left_join(schoolSubAges, schoolSubRolls, by = c("schoolID"))
          
          maxNumKids <- as.numeric(max(schoolSub$NumKidsSpots))
          
          selectedSchoolID <- schoolSub %>%
            filter(NumKidsSpots == maxNumKids) %>%
            arrange(desc(RollSpots)) %>%
            slice_head(n=1) %>%
            select(schoolID) %>%
            pull(schoolID)

          InitialMatches <- schoolsubset %>%
            filter(schoolID == selectedSchoolID) %>%
            ungroup() %>%
            select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople)) %>%
            left_join(peopleInHousehold, by = c("personAge", "personType"))
          
          schoolInfo <- InitialMatches %>%
            ungroup() %>%
            select(schoolID, personAge) %>%
            group_by(schoolID, personAge) %>%
            summarise(KidsPerAge = n())
          
          schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
            mutate(personCounts = personCounts - KidsPerAge,
                   personCounts = ifelse(personCounts < 0, 0, personCounts)) %>%
            select(-KidsPerAge)
          
          schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
            select(schoolID, personAge, personCounts, schoolType, originalCounts)
          
          schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
          
          remainingAges <- peopleInHousehold %>%
            filter(!personID %in% c(InitialMatches$personID))
          
          if(exists("ChildSchoolMatches")) {
            ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)
            # closes if(exists("ChildSchoolMatches")) {
          } else {
            ChildSchoolMatches <- InitialMatches
            # closes else to if(exists("ChildSchoolMatches")) {
          }
          
          remainingAges <- peopleInHousehold %>%
            filter(!personID %in% c(InitialMatches$personID))
          
          # handle the children yet to be placed into a school
          
          while(nrow(remainingAges) > 0) {
            
            
            stop("Test case with all kids going to the same type of school, one or more children remaining \n")
          }
          
          
          # get the school with the largest number of children - account for twins
          
          # ends else to if(nrow(schoolTypeCheck) > 1) {
        }
        
        # ends if(probUsed == 1) {
      } else {
        
        ####################################
        # no children go to the same school
        #####################################
        
        # link each child to one or more schools, retain only child ID, school ID, and roll count
        
        schoolsubset <- left_join(peopleInHousehold, schoolsRenamed, by = "personAge",
                                  relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
          mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
          filter(personCounts > 0,
                 isMatch == "Y") %>%
          mutate(remainingPeople = personCounts - 1) %>%
          filter(remainingPeople > -1) %>%
          select(personID, schoolID, remainingPeople)
        
        # add a unique school to each child ID
        
        matchedSchools <- kidsAdd(schoolsubset)
        
        # add the schools to the kids 
        
        InitialMatches <- left_join(matchedSchools, peopleInHousehold, by = "personID")
        
        schoolInfo <- InitialMatches %>%
          ungroup() %>%
          select(schoolID, personAge) %>%
          group_by(schoolID, personAge) %>%
          summarise(KidsPerAge = n())
        
        # decrement the school(s) roll counts
        
        schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
          mutate(personCounts = personCounts - KidsPerAge,
                 personCounts = ifelse(personCounts <= 0, 0, personCounts)) %>%
          select(-KidsPerAge)
        
        schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
          select(schoolID, personAge, personCounts, schoolType, originalCounts)
        
        schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
        
        
        if(exists("ChildSchoolMatches")) {
          
          ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)
          
          # closes if(exists("ChildSchoolMatches")) {
          
        } else {
          
          ChildSchoolMatches <- bind_rows(InitialMatches)
          
          # closes else to if(exists("ChildSchoolMatches")) {
        }
        
        remainingAges <- remainingAges %>%
          filter(!personID %in% InitialMatches$personID)
        
        if(nrow(remainingAges) > 0) {
          
          stop("There are children who should not be in the same school who must be in the same school")
        }
        
        # closes else to if(probUsed == 1) {
      }
      
      # closes else to if(NumKids==1) {
    }
    
    
    
    # closes  for(i in 1:nrow(ListofHouseholds)) {
  }
  
  NotInSchool <- NotInSchool %>%
    filter(!(personID %in% c(ChildSchoolMatches$personID))) %>%
    mutate(schoolID = as.character(0))
  
  allPeople <- bind_rows(NotInSchool, ChildSchoolMatches) 
  
  if(isTRUE(is.integer(schools[[schid]]))) {
    
    schoolsRenamed <- schoolsRenamed %>% 
      mutate(schoolID = as.integer(schoolID))
    
    allPeople <- allPeople %>%
      mutate(schoolID = as.integer(schoolID))
  }
  
  
  
  if(isTRUE(is.numeric(schools[[schid]]))) {
    
    schoolsRenamed <- schoolsRenamed %>% 
      mutate(schoolID = as.numeric(schoolID))
    
    allPeople <- allPeople %>%
      mutate(schoolID = as.numeric(schoolID))
  }
  
  
  
  if(isTRUE(is.factor(schools[[schid]]))) {
    
    schoolsRenamed <- schoolsRenamed %>% 
      mutate(schoolID = as.factor(schoolID))
    
    allPeople <- allPeople %>%
      mutate(schoolID = as.numeric(schoolID))
  }
  
  
  
  
  
  allPeople <- allPeople %>%
    rename(!!pplidcolName := personID,
           !!pplagecolName := personAge,
           !!pplsexcolName := personType,
           !!hhidcolName := HouseholdID,
           !!statcolName := schStat)
  
  schoolsRenamed <- schoolsRenamed %>%
    rename(spacesRemaining = personCounts) %>%
    mutate(spacesUsed = originalCounts - spacesRemaining) %>%
    rename(!!schidcolName := schoolID,
           !!schagecolName := personAge,
           !!schrollcolName := originalCounts,
           !!schtypecolName := schoolType)
  
  schoolsOutput <- suppressMessages(left_join(schools, schoolsRenamed))
  
  cat("The dataframes are $Population and $Schools", "\n")
  cat("$Population contains everyone in the people data frame, with school IDs added", "\n")
  cat("$Schools contains updated count data for the schools", "\n")
  
  
  Output <- list()
  Output$Population <- allPeople
  Output$Schools <- schoolsOutput
  return(Output)
  
  # closes function
}

