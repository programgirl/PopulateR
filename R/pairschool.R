#' Create a data frame of people matched to schools
#' This function creates a data frame of people and matching schools. By default, all similarly-aged people in the same household, who are in school, will be matched to the same school. If one person is matched to a same-sex school, then all similarly aged people will also be matched to a same-sex school. This includes opposite-sex children.
#' Two data frames are required: one for the children and one for the schools.
#' A numeric or ordered factor for school status is required. The smallest value/level will be treated as the code for people not in school. If one value is used, everyone in the data frame will be allocated a school. Thus, pre-cleaning a data frame is not required. If everyone in the data frame is to be allocated a school, then the same value must be used for everyone.
#' The Schools data frame must be a summary in the form of counts by age within school. Each row is one age only. For example, if a school has children aged 5 to 9 years, there should be 5 rows. Any combination of co-educational and single-sex schools can be used. School ID can be numeric or character. The function will return a data frame of the correct School ID type.
#' The minimum and maximum school ages, followed by the achieved counts by sex for each school, are printed to the console.
#'
#' @export
#' @param people A data frame containing the school children.
#' @param pplid The ID variable in the people data frame.
#' @param pplage The age variable in the people data frame.
#' @param pplsx The sex variable in the people data frame. This column is used to assign people to the appropriate school type (co-educational or single-sex). The expected values are "F" (female) or "M" (male).
#' @param pplst The school status variable in the people data frame. Only two numeric values/factor levels can be used. The smallest number/level is the code for people not in school.
#' @param hhid The column number for the household identifier variable in the people data frame.
#' @param schools A data frame containing the school observations.
#' @param schid The school ID variable in the school data frame.
#' @param schage The age variable in the school data frame.
#' @param schroll The variable containing the number of places available for people at that school age, within the school.
#' @param schtype An indicator variable used to determine whether the school is co-educational or single-sex. The expected values are "C" (co-educational), "F" (female only), and "M" (male-only).
#' @param personprob If one person is assigned to a same-sex school, the probability that another person in the household is also assigned to a same-sex school. If an equivalent same-sex school is not available, the other person will be assigned to a co-ed school. The default value is 1, so that all similarly aged people will be assigned to their respective same-sex schools, or all will be to co-educational schools.
#' @param schmiss The value that will be given to those people not in school. If left blank, the default value is 0. If the school IDs are numeric in the schools data frame, a numeric missing value must be supplied.
#' @param sameprob The probability that children from the same household will be at the same school, given age (and sex if there are same-sex schools). Results depend on the number of children in each household, and child ages, combined with the sizes of the school rolls. Value must be between 0 and 1. The default value is 1.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @return A single data frame with a school column.
#'
#' @examples
#' library(dplyr)
#' SchoolsAdded <- schooladd(IntoSchools, pplid = 3, pplage = 4, pplsx = 8,
#'                           pplst = 6,  hhid = 7, SchoolsToUse, schid = 2,
#'                           schage = 4, schroll = 5, schtype = 3,
#'                           userseed = 4)

pairschool <- function(people, pplid, pplage, pplsx, pplst = NULL, hhid = NULL, schools,
                      schid, schage, schroll, schtype, personprob = 1, schmiss = 0,
                      sameprob = 1, userseed=NULL)
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
      igraph::set_vertex_attr(name = "type", value = names(igraph::V(.)) %in% theDF$personAge)
    
    # max bipartite match
    maxbm <- na.omit(igraph::max_bipartite_match(theGraph)$matching)
    
    # retrieve match pattern and yield output
    v1 <- maxbm[maxbm %in% theDF$personAge]
    v2 <- names(v1)
    # matchedSchools <- data.frame("personAge" = c(v1), schoolID = c(v2)
    data.frame(
      personAge = `class<-`(v1, class(theDF$personAge)),
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
        
        schoolTypeCheck <-  schoolsubset %>%
          group_by(schoolType) %>%
          distinct(schoolType)


        # to max the number of school places available
        # single-sex schools are selected as a priority over co-ed schools
        
        if(nrow(schoolTypeCheck) > 1) {
          
          # select the same sex school(s)

          schoolMatch <- schoolsubset %>%
            filter(!(schoolType == "C")) %>%
            slice_max(personCounts) %>%
            select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))

          # if there are more than two same-sex schools
          # will happen if two or more same-sex schools have the same number of roll slots available
          
          if(nrow(schoolMatch) > 1) {

            schoolMatch <- schoolMatch %>%
              filter(!(schoolType == "C")) %>%
              slice_sample(n=1, weight_by = personCounts, replace = FALSE) %>%
              select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))
          }
          
          # closes if(nrow(schoolTypeCheck) > 1) {
        } else {
          # there is more than one school but they are all the same type

          
          schoolMatch <- schoolsubset %>%
            slice_sample(n=1, weight_by = personCounts, replace = FALSE) %>%
            select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))
        }
        

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
      
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  
    
    
    
    
    
    
  
    
    
    
    
    
    ###############################################
    ###############################################
    # there is more than one kid in the household but no twins
    ###############################################
    ###############################################
    
    if(NumKids > 1 & max(personAgesMaxCount$CountsByAge) == 1) {
      
      # cat("The current household is", CurrentHousehold, "\n")
      # cat("More than one child and no twins \n")
      # cat("The no-twin household is", CurrentHousehold, "max person ages is", max(personAges$CountsByAge), "\n")
      
      ###############################################
      ###############################################
      # households with more than one child
      # no duplicate ages
      ###############################################
      ###############################################
      
      
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
          # first tranche for kids in same schools
          # there are more same-sex schools than co-ed
          # no twins
          ###############################################
          
          if(SingleSexNum > OppSexNum) {
            
            # cat("Entered SS > OS loop \n")
            
            InitialMatches <- schoolsubset %>%
              filter(schoolID %in% c(SingleSexSub$schoolID)) %>%
              select(-c(personCounts, originalCounts, remainingPeople)) %>%
              left_join(peopleInHousehold, by = c("personAge", "personType")) %>%
              select(-c(schoolType, isMatch))

            # fix school counts
            
            schoolInfo <- InitialMatches %>%
              ungroup() %>%
              select(schoolID, personAge)
            
            schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
              mutate(personCounts = personCounts - 1,
                     personCounts = ifelse(personCounts < 0, 0, personCounts)) 
            
            schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
              select(schoolID, personAge, personCounts, schoolType, originalCounts)
            
            # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")
            
            schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
            
            remainingAges <- peopleInHousehold %>%
              filter(!personID %in% c(InitialMatches$personID))
            
            if(nrow(remainingAges) == 0) {

              if(exists("ChildSchoolMatches")) {
                
                ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)
                
                # closes if(exists("ChildSchoolMatches")) {
                
              } else {
                
                ChildSchoolMatches <- InitialMatches
                
                # closes else to if(exists("ChildSchoolMatches")) {
              }
              
              # closes if(nrow(remainingAges) == 0) {
            } 
            
            # closes if(SingleSexNum > OppSexNum) {
            
            ###############################################
            # there are more coed-sex schools than same-sex
            # no twins
            ###############################################
          } else if (OppSexNum > SingleSexNum) {
            
            # cat("Entered OS > SS loop \n")
            
            InitialMatches <- schoolsubset %>%
              filter(schoolID %in% c(SelectedOppSexSchool$schoolID)) %>%
              ungroup() %>%
              select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople, personType)) %>%
              left_join(peopleInHousehold, by = "personAge")

            schoolInfo <- InitialMatches %>%
              ungroup() %>%
              select(schoolID, personAge)
            
            
            schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
              mutate(personCounts = personCounts - 1,
                     personCounts = ifelse(personCounts < 0, 0, personCounts)) 
            
            schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
              select(schoolID, personAge, personCounts, schoolType, originalCounts)
            
            # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")
            
            schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)

            remainingAges <- peopleInHousehold %>%
              filter(!personID %in% c(InitialMatches$personID))
            
            if(nrow(remainingAges) == 0) {
              
              if(exists("ChildSchoolMatches")) {
                
                ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)
                
                # closes if(exists("ChildSchoolMatches")) {
                
              } else {
                
                ChildSchoolMatches <- InitialMatches
                
                # closes else to if(exists("ChildSchoolMatches")) {
              }
              
              # closes if(nrow(remainingAges) == 0) {
            } 
            
            
            
            
            ###############################################
            # there are the same number of coed-sex and same-sex schools
            # no twins
            ###############################################
          } else if (SingleSexNum == OppSexNum) {
            
            # cat("Entered SS == OS loop \n")
            
            if(SingleSexWeight >= OppSexWeight) {
              
              # cat("Entered SS > OS loop \n")
              
              InitialMatches <- schoolsubset %>%
                filter(schoolID %in% c(SingleSexSub$schoolID)) %>%
                select(-c(personCounts, originalCounts, remainingPeople)) %>%
                left_join(peopleInHousehold, by = c("personAge", "personType")) %>%
                select(-c(schoolType, isMatch))
              
              # cat("Initial matches file constructed \n")
              
              # fix school counts
              
              schoolInfo <- InitialMatches %>%
                ungroup() %>%
                select(schoolID, personAge)
              
              # cat("school info file constructed \n")
              
              schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                mutate(personCounts = personCounts - 1,
                       personCounts = ifelse(personCounts < 0, 0, personCounts)) 
              
              # cat("schoolsselected file constructed \n")
              
              schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                select(schoolID, personAge, personCounts, schoolType, originalCounts)

              
              # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")
              
              schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
              
              # cat("schools renamed file reconstructed \n")
              
              remainingAges <- peopleInHousehold %>%
                filter(!personID %in% c(InitialMatches$personID))
              
              # cat("remaining people file constructed \n")
              
              if(nrow(remainingAges) == 0) {
                
                if(exists("ChildSchoolMatches")) {
                  
                  ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)
                  
                  # closes if(exists("ChildSchoolMatches")) {
                  
                } else {
                  
                  ChildSchoolMatches <- InitialMatches
                  
                  # closes else to if(exists("ChildSchoolMatches")) {
                }
                
                # closes if(nrow(remainingAges) == 0) {
              }
              
              
              # closes if(SingleSexWeight >= OppSexWeight) {
            } else {
              
              # cat("Entered OS > SS loop \n")
              
              InitialMatches <- schoolsubset %>%
                filter(schoolID %in% c(SelectedOppSexSchool$schoolID)) %>%
                ungroup() %>%
                select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople, personType)) %>%
                left_join(peopleInHousehold, by = "personAge")
              

              schoolInfo <- InitialMatches %>%
                ungroup() %>%
                select(schoolID, personAge)
              
              
              schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                mutate(personCounts = personCounts - 1,
                       personCounts = ifelse(personCounts < 0, 0, personCounts)) 
              
              schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                select(schoolID, personAge, personCounts, schoolType, originalCounts)
              
              # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")
              
              schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)

              
              remainingAges <- peopleInHousehold %>%
                filter(!personID %in% c(InitialMatches$personID))
              
              if(nrow(remainingAges) == 0) {
                
                if(exists("ChildSchoolMatches")) {
                  
                  ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)
                  
                  # closes if(exists("ChildSchoolMatches")) {
                  
                } else {
                  
                  ChildSchoolMatches <- InitialMatches
                  
                  # closes else to if(exists("ChildSchoolMatches")) {
                }
                
                # closes if(nrow(remainingAges) == 0) {
              }
              
              # closes else to if(SingleSexWeight >= OppSexWeight) {
            }
            
            
            
          } else {
            
            stop("The same-sex loops haven't included all options, current household is", CurrentHousehold, "\n")
          }
          
          
          
          
          
          
          
          
          
      
          
          
    
          
          
          
          ###############################################
          # second tranche for kids in same schools
          # there are children remaining
          # no twins
          ###############################################
          
          if(nrow(remainingAges) > 0) {
            
            # cat("Children still remain to be allocated, starting second tranche, with same sex schools \n")
            
            # construct a new schools subset
            schoolsubset <- left_join(remainingAges, schoolsRenamed, by = "personAge", 
                                      relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
              mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
              filter(personCounts > 0,
                     isMatch == "Y") %>%
              mutate(remainingPeople = personCounts - 1) %>%
              filter(remainingPeople > -1)
              
            schoolTypeCheck <- schoolsubset %>%
              group_by(schoolType) %>%
              distinct(schoolType)
            
           
            # still more than one type of school in the schools data
            
            if(nrow(schoolTypeCheck) > 1) {
              
              
              # create a new variable that is a not-same sex versus same-sex marker
              
              sexedsubset <- schoolsubset %>%
                mutate(SexDual = ifelse(schoolType == "C", "O", "S"))
              
              # put the single sex schools into one subset
              # has count by age only, no school ID, no person ID
              
              SingleSexSubAges <- sexedsubset %>%
                filter(SexDual == "S") %>%
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
              
              if(SingleSexNum > OppSexNum) {
                
                # cat("Entered SS > OS loop \n")

                SecondMatches <- schoolsubset %>%
                  filter(schoolID %in% c(SingleSexSub$schoolID)) %>%
                  select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))
                
                # fix school counts
                
                schoolInfo <- SecondMatches %>%
                  ungroup() %>%
                  select(schoolID, personAge)
                
                schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                  mutate(personCounts = personCounts - 1,
                         personCounts = ifelse(personCounts < 0, 0, personCounts)) 
                
                schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                  select(schoolID, personAge, personCounts, schoolType, originalCounts)
                
                # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")
                
                schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
                
                multipleKids <- bind_rows(InitialMatches, SecondMatches)
                
                remainingAges2 <- peopleInHousehold %>%
                  filter(!personID %in% c(multipleKids$personID))
                
                if(nrow(remainingAges2) == 0) {
                  
                  if(exists("ChildSchoolMatches")) {
                    
                    ChildSchoolMatches <- bind_rows(ChildSchoolMatches, multipleKids)
                    
                    # closes if(exists("ChildSchoolMatches")) {
                    
                  } else {
                    
                    ChildSchoolMatches <- bind_rows(InitialMatches, multipleKids)
                    
                    # closes else to if(exists("ChildSchoolMatches")) {
                  }
                  
                  # closes if(nrow(remainingAges) == 0) {
                } 
                
                # closes if(SingleSexNum > OppSexNum) {
                
                ###############################################
                # there are more coed-sex schools than same-sex
                # no twins
                ###############################################
              } else if (OppSexNum > SingleSexNum) {
                
                # cat("Entered OS > SS loop \n")
                
                SecondMatches <- schoolsubset %>%
                  filter(schoolID %in% c(SelectedOppSexSchool$schoolID)) %>%
                  select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))

                schoolInfo <- SecondMatches %>%
                  ungroup() %>%
                  select(schoolID, personAge)
                
                
                schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                  mutate(personCounts = personCounts - 1,
                         personCounts = ifelse(personCounts < 0, 0, personCounts)) 
                
                schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                  select(schoolID, personAge, personCounts, schoolType, originalCounts)
                
                # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")
                
                schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
                
                multipleKids <- bind_rows(InitialMatches, SecondMatches)
                
               remainingAges2 <- peopleInHousehold %>%
                  filter(!personID %in% c(multipleKids$personID))
                
                if(nrow(remainingAges2) == 0) {
                  
                  if(exists("ChildSchoolMatches")) {
                    
                    ChildSchoolMatches <- bind_rows(ChildSchoolMatches, multipleKids)
                    
                    # closes if(exists("ChildSchoolMatches")) {
                    
                  } else {
                    
                    ChildSchoolMatches <- multipleKids
                    
                    # closes else to if(exists("ChildSchoolMatches")) {
                  }
                  
                  # closes if(nrow(remainingAges) == 0) {
                } 
                
                ###############################################
                # there are the same number of coed-sex and same-sex schools
                # no twins
                ###############################################
              } else if (SingleSexNum == OppSexNum) {
                
                # cat("Entered SS == OS loop \n")
                
                if(SingleSexWeight >= OppSexWeight) {
                  
                  # cat("Entered SS > OS loop \n")
                  
                  SecondMatches <- schoolsubset %>%
                    filter(schoolID %in% c(SingleSexSub$schoolID)) %>%
                    select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))
                  
                  # fix school counts

                  schoolInfo <- SecondMatches %>%
                    ungroup() %>%
                    select(schoolID, personAge)
                  
                  schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                    mutate(personCounts = personCounts - 1,
                           personCounts = ifelse(personCounts < 0, 0, personCounts)) 
                  
                  schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                    select(schoolID, personAge, personCounts, schoolType, originalCounts)
                  
                  # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")
                  
                  schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
                  
                  multipleKids <- bind_rows(InitialMatches, SecondMatches)
                  
                  remainingAges2 <- peopleInHousehold %>%
                    filter(!personID %in% c(multipleKids$personID))
                  
                  if(nrow(remainingAges2) == 0) {
                    
                    if(exists("ChildSchoolMatches")) {
                      
                      ChildSchoolMatches <- bind_rows(ChildSchoolMatches, multipleKids)
                      
                      # closes if(exists("ChildSchoolMatches")) {
                      
                    } else {
                      
                      ChildSchoolMatches <- multipleKids
                      
                      # closes else to if(exists("ChildSchoolMatches")) {
                    }
                    
                    # closes if(nrow(remainingAges) == 0) {
                  }
                  
                  
                  # closes if(SingleSexWeight >= OppSexWeight) {
                } else {
                  
                  # cat("Entered OS > SS loop \n")
                  
                  SecondMatches <- schoolsubset %>%
                    filter(schoolID %in% c(SelectedOppSexSchool$schoolID)) %>%
                    select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))
                  
                  schoolInfo <- SecondMatches %>%
                    ungroup() %>%
                    select(schoolID, personAge)
                  
                  
                  schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                    mutate(personCounts = personCounts - 1,
                           personCounts = ifelse(personCounts < 0, 0, personCounts)) 
                  
                  schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                    select(schoolID, personAge, personCounts, schoolType, originalCounts)
                  
                  # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")
                  
                  schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
                  
                  multipleKids <- bind_rows(InitialMatches, SecondMatches)
                  
                  remainingAges2 <- remainingAges %>%
                    filter(!personID %in% c(multipleKids$personID))
                  
                  if(nrow(remainingAges2) == 0) {
                    
                    if(exists("ChildSchoolMatches")) {
                      
                      ChildSchoolMatches <- bind_rows(ChildSchoolMatches, multipleKids)
                      
                      # closes if(exists("ChildSchoolMatches")) {
                      
                    } else {
                      
                      ChildSchoolMatches <- multipleKids
                      
                      # closes else to if(exists("ChildSchoolMatches")) {
                    }
                    
                    # closes if(nrow(remainingAges2) == 0) {
                  }
                  
                  # closes else to if(SingleSexWeight >= OppSexWeight) {
                }
                
                
                
              } else {
                
                stop("The same-sex loops haven't included all options, current household is", CurrentHousehold, "\n")
              }
            
            # closes if(nrow(schoolTypeCheck) > 1) { FOR SECOND TRANCHE
            } else {

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
                slice_head(n=1) 
              
              SecondMatches <- schoolsubset %>%
                filter(schoolID %in% c(selectedSchool$schoolID)) %>%
                select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))
              
              schoolsSelected <- left_join(SecondMatches, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                select(schoolID, personAge, personCounts, schoolType, originalCounts) %>%
                mutate(personCounts = personCounts - 1,
                       personCounts = ifelse(personCounts <= 0, 0, personCounts)) 
              
              schoolsNotSelected <- anti_join(schoolsRenamed, SecondMatches, by = c("schoolID", "personAge")) %>%
                select(schoolID, personAge, personCounts, schoolType, originalCounts) 
              
              schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
              
              multipleKids <- bind_rows(InitialMatches, SecondMatches)
              
              remainingAges2 <- peopleInHousehold %>%
                filter(!personID %in% c(multipleKids$personID))
              
              if(nrow(remainingAges2) == 0) {
                
                if(exists("ChildSchoolMatches")) {
                  
                  ChildSchoolMatches <- bind_rows(ChildSchoolMatches, multipleKids)
                  
                  # closes if(exists("ChildSchoolMatches")) {
                  
                } else {
                  
                  ChildSchoolMatches <- multipleKids
                  
                  # closes else to if(exists("ChildSchoolMatches")) {
                }
                
                # closes if(nrow(remainingAges) == 0) {
              }
              
              
              # closes else to if(nrow(schoolTypeCheck) > 1) { FOR SECOND TRANCHE
            }
            
            # closes if(nrow(remainingAges > 0)) {
          }


          

          
          
          
          
          
          
          
          
          
          
      
          if(nrow(remainingAges2) > 0) {
            
            
            
            stop("Need third tranche for kids in same-sex schools \n")
          }
          
          
          
          
          
          
          
               
         
          
          
          
          
          
           ###############################################
          ###############################################
          # finish single sex schools code block
          ###############################################
          ###############################################

          # closes if(nrow(schoolTypeCheck) > 1) { FOR FIRST TRANCHE
          } else {
          

          
          
            
            
            
          ###############################################
          ###############################################
          # start there are no same sex schools code block
          ###############################################
          ###############################################
            

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
            slice_head(n=1) 
          
          filteredSchoolSubset <- schoolsubset %>%
            filter(schoolID %in% c(selectedSchool$schoolID))
          
          schoolInfo <- filteredSchoolSubset %>%
            ungroup() %>%
            select(schoolID, personAge)
          
          InitialMatches <- schoolsubset %>%
            filter(schoolID %in% c(filteredSchoolSubset$schoolID)) %>%
            select(-c(personCounts, originalCounts, remainingPeople)) %>%
            left_join(peopleInHousehold, by = c("personAge", "personType")) %>%
            select(-c(schoolType, isMatch))
 
          
          schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
            select(schoolID, personAge, personCounts, schoolType, originalCounts) %>%
            mutate(personCounts = personCounts - 1,
                   personCounts = ifelse(personCounts <= 0, 0, personCounts)) 
          
          schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
            select(schoolID, personAge, personCounts, schoolType, originalCounts) 
          
          schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
          
          remainingAges <- peopleInHousehold %>%
            filter(!personID %in% c(InitialMatches$personID))
          

          if(nrow(remainingAges) == 0) {
            
            if(exists("ChildSchoolMatches")) {
              
              ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)
              
              
              
              # closes if(exists("ChildSchoolMatches")) {
              
            } else {
              
              ChildSchoolMatches <- InitialMatches
              
              # closes else to if(exists("ChildSchoolMatches")) {
            }
            
            # closes if(nrow(remainingAges) == 0) {
          }
          
           if (nrow(remainingAges) > 0) {
            
            # cat("entered else if \n")
            
            schoolsubset <- schoolsubset %>%
              filter(personAge %in% remainingAges$personAge)
            
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
              slice_head(n=1) 
            
            filteredSchoolSubset <- schoolsubset %>%
              filter(schoolID %in% c(selectedSchool$schoolID))
            
            SecondMatches <- left_join(filteredSchoolSubset, peopleInHousehold, by = "personAge") %>%
              select(-c(schoolType, personCounts, numberKids, remainingPeople))
            
            schoolsSelected <- left_join(SecondMatches, schoolsRenamed, by = c("schoolID", "personAge")) %>%
              select(schoolID, personAge, personCounts, schoolType, originalCounts) %>%
              mutate(personCounts = personCounts - 1,
                     personCounts = ifelse(personCounts <= 0, 0, personCounts)) 
            
            schoolsNotSelected <- anti_join(schoolsRenamed, SecondMatches, by = c("schoolID", "personAge")) %>%
              select(schoolID, personAge, personCounts, schoolType, originalCounts) 
            
            schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
            
            multipleKids <- bind_rows(InitialMatches, SecondMatches)
            
            remainingAges2 <- peopleInHousehold %>%
              filter(!personID %in% c(multipleKids$personID))
            
            if(nrow(remainingAges2) == 0) {
              
              if(exists("ChildSchoolMatches")) {
                
                ChildSchoolMatches <- bind_rows(ChildSchoolMatches, multipleKids)
                
                # closes if(exists("ChildSchoolMatches")) {
                
              } else {
                
                ChildSchoolMatches <- multipleKids
                
                # closes else to if(exists("ChildSchoolMatches")) {
              }
              
              # closes if(nrow(remainingAges2) == 0) {
            }
            
            #closes } else if (nrow(remainingAges) > 0) {
          } 
          
          if(nrow(remainingAges2) > 0) {
            
            stop("No twins, no single sex schools, need third tranche")
          }
          
          #TODO code block here if remainingAges2 > 0
          
          ###############################################
          ###############################################
          # end there are no same sex schools code block
          ###############################################
          ###############################################
          
          # closes else to if(nrow(schoolTypeCheck) > 1) {
        }
        
        
        # closes if(probUsed ==1) {
      } else {
        
        # cat("Household ID is", CurrentHousehold, "Children will be not be going to the same school \n")
       
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         
        ###############################################
        ###############################################
        # no kids go to the same school
        ###############################################
        ###############################################
        
        schoolsubset <- schoolsubset %>%
          ungroup() %>%
          select(personAge, schoolID, remainingPeople)
        
        matchedSchools <- kidsAdd(schoolsubset)
        
        InitialMatches <- left_join(matchedSchools, peopleInHousehold, by = "personAge") 
        
        if(nrow(InitialMatches) < nrow(personAges)) {
          
          # this now means there are no schools that meet the requirement for the other children not being at the same school
          # which means all the school roll counts are zero
          
          # cat("The number of matches is", nrow(InitialMatches), "and the number of people ages is", nrow(personAges), "\n")
          
          # find the child/ren to add
          remainingAges <- personAges %>%
            filter(!(personAge %in% c(InitialMatches$personAge)))
          
          # add to same school if possible
          
          OtherKids <- left_join(remainingAges, schoolsRenamed, by = "personAge",
                                 relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
            mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
            filter(isMatch == "Y") %>%
            group_by(personAge) %>%
            slice_sample(n=1, weight_by = originalCounts, replace = FALSE) %>%
            ungroup() %>%
            select(personAge, schoolID)
          
          # may not have spaces at the same school, if not, pull a new school
          
          if(nrow(OtherKids) == 0) {
            
            OtherKids <- left_join(remainingAges, schoolsRenamed, by = "personAge",
                                   relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
              mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
              filter(isMatch == "Y",
                     !(schoolID %in% c(InitialMatches$schoolID))) %>%
              group_by(personAge) %>%
              slice_sample(n=1, weight_by = originalCounts, replace = FALSE) %>%
              ungroup() %>%
              select(personAge, schoolID)
            
            # closes if(nrow(OtherKids) == 0) {
          }
          
          
          OtherKids <- left_join(OtherKids, peopleInHousehold, by = "personAge") 
          
          MultipleKids <- bind_rows(InitialMatches, OtherKids)
          
          # closes if(nrow(InitialMatches) < nrow(personAges)) {
        } else {
          
          MultipleKids <- InitialMatches
          
          # closes else to if(nrow(InitialMatches) < nrow(personAges)) {
        }
        
        
        schoolsSelected <- left_join(MultipleKids, schoolsRenamed, by = c("schoolID", "personAge")) %>%
          select(schoolID, personAge, personCounts, schoolType, originalCounts) %>%
          mutate(personCounts = personCounts - 1,
                 personCounts = ifelse(personCounts < 0, 0, personCounts)
          ) 
        
        schoolsNotSelected <- anti_join(schoolsRenamed, MultipleKids, by = c("schoolID", "personAge")) %>%
          select(schoolID, personAge, personCounts, schoolType, originalCounts) 
        
        # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")
        
        schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
        
        # cat("Final size", nrow(schoolsRenamed), "\n")
        
        if(exists("ChildSchoolMatches")) {
          
          ChildSchoolMatches <- bind_rows(ChildSchoolMatches, MultipleKids)
          
          # closes if(exists("ChildSchoolMatches")) {
          
        } else {
          
          ChildSchoolMatches <- MultipleKids
          
          # closes else to if(exists("ChildSchoolMatches")) {
        }
        
        ###############################################
        ###############################################
        # closes no kids go to the same school
        ###############################################
        ###############################################
        
      }
      
      
      # closes if(NumKids > 1 & max(personAges$CountsByAge) == 1) {
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ###############################################
    ###############################################
    ###############################################
    # households with twins etc
    ###############################################
    ###############################################
    ###############################################

    if(NumKids > 1 & max(personAgesMaxCount$CountsByAge) > 1) {


      # cat("More than one child and no twins \n")
      # cat("The twin household is", CurrentHousehold, "max person ages is", max(personAgesMaxCount$CountsByAge), "\n")

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

      # cat("Prob used is", probUsed, "\n")

      if(probUsed == 1) {

        ###############################################
        ###############################################
        ###############################################
        # all kids to same schools
        ###############################################
        ###############################################
        ###############################################


        schoolsubset <- left_join(peopleInHousehold, schoolsRenamed, by = "personAge",
                                  relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
          mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
          filter(personCounts > 0,
                 isMatch == "Y") %>%
          mutate(remainingPeople = personCounts - 1) %>%
          filter(remainingPeople > 0)

        
        ###############################################
        ###############################################
        # start single sex schools code block
        ###############################################
        ###############################################

        schoolTypeCheck <- schoolsubset %>%
          group_by(schoolType) %>%
          distinct(schoolType)

        # cat("The schoolTypeCheck contains", nrow(schoolTypeCheck), "rows \n")

        if(nrow(schoolTypeCheck) > 1) {


          # create a new variable that is a not-same sex versus same-sex marker

          sexedsubset <- schoolsubset %>%
            mutate(SexDual = ifelse(schoolType == "C", "O", "S"))


          # put the single sex schools into one subset
          # has count by age only, no school ID, no person ID

          SingleSexSubAges <- sexedsubset %>%
            filter(SexDual == "S") %>%
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

          # cat("Number single sex schools is", SingleSexNum, "with", SingleSexWeight, "roll places \n")

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

          # cat("Number opposite sex schools is", OppSexNum, "with", OppSexWeight, "roll places \n")



          ###############################################
          # first tranche for kids in same schools
          # there are more same-sex schools than co-ed
          # twins
          ###############################################

           if(SingleSexNum > OppSexNum) {

            # cat("Entered SS > OS loop \n")

            InitialMatches <- schoolsubset %>%
              filter(schoolID %in% c(SingleSexSub$schoolID)) %>%
              select(-c(personCounts, originalCounts, remainingPeople, schoolType, isMatch))


            # fix school counts

            schoolInfo <- InitialMatches %>%
              ungroup() %>%
              select(schoolID, personAge) %>%
              group_by(schoolID, personAge) %>%
              summarise(NumUsed  = n()) %>%
              ungroup()


            schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
              mutate(personCounts = personCounts - NumUsed,
                     personCounts = ifelse(personCounts < 0, 0, personCounts)) %>%
              select(-NumUsed)

            schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
              select(schoolID, personAge, personCounts, schoolType, originalCounts)

            # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")

            schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)

            remainingAges <- peopleInHousehold %>%
              filter(!personID %in% c(InitialMatches$personID))

            if(nrow(remainingAges) == 0) {

              if(exists("ChildSchoolMatches")) {

                ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)

                # closes if(exists("ChildSchoolMatches")) {

              } else {

                ChildSchoolMatches <- InitialMatches

                # closes else to if(exists("ChildSchoolMatches")) {
              }

              # closes if(nrow(remainingAges) == 0) {
            }


            # closes if(SingleSexNum > OppSexNum) {
            ###############################################
            # there are more coed-sex schools than same-sex
            # twins
            ###############################################
          } else if(OppSexNum > SingleSexNum) {

            InitialMatches <- schoolsubset %>%
              filter(schoolID %in% c(SelectedOppSexSchool$schoolID)) %>%
              select(-c(personCounts, originalCounts, remainingPeople, schoolType, isMatch))

            schoolInfo <- InitialMatches %>%
              ungroup() %>%
              select(schoolID, personAge) %>%
              group_by(schoolID, personAge) %>%
              summarise(NumUsed  = n()) %>%
              ungroup()

            schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
              mutate(personCounts = personCounts - NumUsed,
                     personCounts = ifelse(personCounts < 0, 0, personCounts)) %>%
              select(-NumUsed)

            schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
              select(schoolID, personAge, personCounts, schoolType, originalCounts)

            # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")

            schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)

            remainingAges <- peopleInHousehold %>%
              filter(!personID %in% c(InitialMatches$personID))

            if(nrow(remainingAges) == 0) {

              if(exists("ChildSchoolMatches")) {

                ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)

                # closes if(exists("ChildSchoolMatches")) {

              } else {

                ChildSchoolMatches <- InitialMatches

                # closes else to if(exists("ChildSchoolMatches")) {
              }

              # closes if(nrow(remainingAges) == 0) {
            }


            ###############################################
            # there are the same number of coed-sex and same-sex schools
            # no twins
            ###############################################
          } else if (SingleSexNum == OppSexNum) {

            # cat("Entered SS == OS loop \n")

            if(SingleSexWeight >= OppSexWeight) {

              InitialMatches <- schoolsubset %>%
                filter(schoolID %in% c(SingleSexSub$schoolID)) %>%
                select(-c(personCounts, originalCounts, remainingPeople, schoolType, isMatch))

              # cat("Initial matches file constructed \n")

              # fix school counts

              schoolInfo <- InitialMatches %>%
                ungroup() %>%
                select(schoolID, personAge) %>%
                group_by(schoolID, personAge) %>%
                summarise(NumUsed  = n()) %>%
                ungroup()

              # cat("school info file constructed \n")

              schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                mutate(personCounts = personCounts - NumUsed,
                       personCounts = ifelse(personCounts < 0, 0, personCounts)) %>%
                select(-NumUsed)

              # cat("schoolsselected file constructed \n")

              schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                select(schoolID, personAge, personCounts, schoolType, originalCounts)


              # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")

              schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)

              # cat("schools renamed file reconstructed \n")

              remainingAges <- peopleInHousehold %>%
                filter(!personID %in% c(InitialMatches$personID))

              # cat("remaining people file constructed \n")

              if(nrow(remainingAges) == 0) {

                if(exists("ChildSchoolMatches")) {

                  ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)

                  # closes if(exists("ChildSchoolMatches")) {

                } else {

                  ChildSchoolMatches <- InitialMatches

                  # closes else to if(exists("ChildSchoolMatches")) {
                }

                # closes if(nrow(remainingAges) == 0) {
              }

            } else {

              # cat("Entered OS > SS loop \n")

              InitialMatches <- schoolsubset %>%
                filter(schoolID %in% c(SelectedOppSexSchool$schoolID)) %>%
                select(-c(personCounts, originalCounts, remainingPeople, schoolType, isMatch))

              schoolInfo <- InitialMatches %>%
                select(schoolID, personAge) %>%
                group_by(schoolID, personAge) %>%
                summarise(NumUsed  = n()) %>%
                ungroup()


              schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                mutate(personCounts = personCounts - NumUsed,
                       personCounts = ifelse(personCounts < 0, 0, personCounts)) %>%
                select(-NumUsed)


              schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                select(schoolID, personAge, personCounts, schoolType, originalCounts)

              # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")

              schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)


              remainingAges <- peopleInHousehold %>%
                filter(!personID %in% c(InitialMatches$personID))

              if(nrow(remainingAges) == 0) {

                if(exists("ChildSchoolMatches")) {

                  ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)

                  # closes if(exists("ChildSchoolMatches")) {

                } else {

                  ChildSchoolMatches <- InitialMatches

                  # closes else to if(exists("ChildSchoolMatches")) {
                }

                # closes if(nrow(remainingAges) == 0) {
              }

              # closes else to if(SingleSexWeight >= OppSexWeight) {
            }



          } else {

            stop("The same-sex loops haven't included all options, current household is", CurrentHousehold, "\n")
          }



          ###############################################
          # second tranche for kids in same schools
          # there are children remaining
          # twins
          ###############################################

          if(nrow(remainingAges) > 0) {

            # cat("Children still remain to be allocated, starting second tranche, with same sex schools \n")
            # cat("The number remaining are", nrow(remainingAges), "the number available in schools is", 
            #     sum(schoolsRenamed$personCounts), "\n")

            # construct a new schools subset
            schoolsubset <- left_join(remainingAges, schoolsRenamed, by = "personAge",
                                      relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
              mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
              filter(personCounts > 0,
                     isMatch == "Y") %>%
              mutate(remainingPeople = personCounts - 1) %>%
              filter(remainingPeople > -1)
            

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

              # cat("Number single sex schools is", SingleSexNum, "with", SingleSexWeight, "roll places \n")

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

              # cat("Number opposite sex schools is", OppSexNum, "with", OppSexWeight, "roll places \n")

              if(SingleSexNum > OppSexNum) {

                # cat("Entered SS > OS loop \n")

                SecondMatches <- schoolsubset %>%
                  filter(schoolID %in% c(SingleSexSub$schoolID)) %>%
                  ungroup() %>%
                  select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople, personType)) %>%
                  left_join(peopleInHousehold, by = "personAge")


                schoolInfo <- SecondMatches %>%
                  select(schoolID, personAge) %>%
                  group_by(schoolID, personAge) %>%
                  summarise(NumUsed  = n()) %>%
                  ungroup()

                schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                  mutate(personCounts = personCounts - NumUsed,
                         personCounts = ifelse(personCounts < 0, 0, personCounts)) %>%
                  select(-NumUsed)

                schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                  select(schoolID, personAge, personCounts, schoolType, originalCounts)

                # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")

                schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)

                multipleKids <- bind_rows(InitialMatches, SecondMatches)

                remainingAges2 <- peopleInHousehold %>%
                  filter(!personID %in% c(multipleKids$personID))

                if(nrow(remainingAges2) == 0) {

                  if(exists("ChildSchoolMatches")) {

                    ChildSchoolMatches <- bind_rows(ChildSchoolMatches, multipleKids)

                    # closes if(exists("ChildSchoolMatches")) {

                  } else {

                    ChildSchoolMatches <- bind_rows(InitialMatches, multipleKids)

                    # closes else to if(exists("ChildSchoolMatches")) {
                  }

                  # closes if(nrow(remainingAges) == 0) {
                }

                # closes if(SingleSexNum > OppSexNum) {

                ###############################################
                # there are more coed-sex schools than same-sex
                # no twins
                ###############################################
              } else if (OppSexNum > SingleSexNum) {

                SecondMatches <- schoolsubset %>%
                  filter(schoolID %in% c(SelectedOppSexSchool$schoolID)) %>%
                  ungroup() %>%
                  select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople, personType)) %>%
                  left_join(peopleInHousehold, by = "personAge")


                schoolInfo <- SecondMatches %>%
                  select(schoolID, personAge) %>%
                  group_by(schoolID, personAge) %>%
                  summarise(NumUsed  = n()) %>%
                  ungroup()

                schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                  mutate(personCounts = personCounts - NumUsed,
                         personCounts = ifelse(personCounts < 0, 0, personCounts)) %>%
                  select(-NumUsed)

                schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                  select(schoolID, personAge, personCounts, schoolType, originalCounts)

                # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")

                schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)

                multipleKids <- bind_rows(InitialMatches, SecondMatches)

                remainingAges2 <- peopleInHousehold %>%
                  filter(!personID %in% c(multipleKids$personID))

                if(nrow(remainingAges2) == 0) {

                  if(exists("ChildSchoolMatches")) {

                    ChildSchoolMatches <- bind_rows(ChildSchoolMatches, multipleKids)

                    # closes if(exists("ChildSchoolMatches")) {

                  } else {

                    ChildSchoolMatches <- bind_rows(InitialMatches, multipleKids)

                    # closes else to if(exists("ChildSchoolMatches")) {
                  }

                  # closes if(nrow(remainingAges) == 0) {
                }


              # there is only one type of school left in the data

              } else if (SingleSexNum == OppSexNum) {

                # cat("Entered SS == OS loop \n")

                if(SingleSexWeight >= OppSexWeight) {

                  # cat("Entered SS > OS loop \n")

                  SecondMatches <- schoolsubset %>%
                    filter(schoolID %in% c(SingleSexSub$schoolID)) %>%
                    ungroup() %>%
                    select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople, personType)) %>%
                    left_join(peopleInHousehold, by = "personAge")


                  schoolInfo <- SecondMatches %>%
                    select(schoolID, personAge) %>%
                    group_by(schoolID, personAge) %>%
                    summarise(NumUsed  = n()) %>%
                    ungroup()

                  schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                    mutate(personCounts = personCounts - NumUsed,
                           personCounts = ifelse(personCounts < 0, 0, personCounts)) %>%
                    select(-NumUsed)

                  schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                    select(schoolID, personAge, personCounts, schoolType, originalCounts)

                  # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")

                  schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)

                  multipleKids <- bind_rows(InitialMatches, SecondMatches)

                  remainingAges2 <- peopleInHousehold %>%
                    filter(!personID %in% c(multipleKids$personID))

                  if(nrow(remainingAges2) == 0) {

                    if(exists("ChildSchoolMatches")) {

                      ChildSchoolMatches <- bind_rows(ChildSchoolMatches, multipleKids)

                      # closes if(exists("ChildSchoolMatches")) {

                    } else {

                      ChildSchoolMatches <- multipleKids

                      # closes else to if(exists("ChildSchoolMatches")) {
                    }

                    # closes if(nrow(remainingAges) == 0) {
                  }


                  # closes if(SingleSexWeight >= OppSexWeight) {
                } else {

                  # cat("Entered OS > SS loop \n")

                  SecondMatches <- schoolsubset %>%
                    filter(schoolID %in% c(SelectedOppSexSchool$schoolID)) %>%
                    ungroup() %>%
                    select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople, personType)) %>%
                    left_join(peopleInHousehold, by = "personAge")


                  schoolInfo <- SecondMatches %>%
                    select(schoolID, personAge) %>%
                    group_by(schoolID, personAge) %>%
                    summarise(NumUsed  = n()) %>%
                    ungroup()


                  schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                    mutate(personCounts = personCounts - NumUsed,
                           personCounts = ifelse(personCounts < 0, 0, personCounts)) %>%
                    select(-NumUsed)

                  schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                    select(schoolID, personAge, personCounts, schoolType, originalCounts)

                  # cat("Original sizes",nrow(schoolsSelected), "and", nrow(schoolsNotSelected), "for InitialMatches smaller \n")

                  schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)

                  multipleKids <- bind_rows(InitialMatches, SecondMatches)

                  remainingAges2 <- remainingAges %>%
                    filter(!personID %in% c(multipleKids$personID))

                  if(nrow(remainingAges2) == 0) {

                    if(exists("ChildSchoolMatches")) {

                      ChildSchoolMatches <- bind_rows(ChildSchoolMatches, multipleKids)

                      # closes if(exists("ChildSchoolMatches")) {

                    } else {

                      ChildSchoolMatches <- multipleKids

                      # closes else to if(exists("ChildSchoolMatches")) {
                    }

                    # closes if(nrow(remainingAges2) == 0) {
                  }

                  # closes else to if(SingleSexWeight >= OppSexWeight) {
                }



              } else {

                stop("The same-sex loops haven't included all options, current household is", CurrentHousehold, "\n")
              }


             # only one type of school available, select from these schools

            } else {


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
                slice_head(n=1)

              SecondMatches <- schoolsubset %>%
                filter(schoolID %in% c(selectedSchool$schoolID)) %>%
                select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))
              
              schoolInfo <- SecondMatches %>%
                select(schoolID, personAge) %>%
                group_by(schoolID, personAge) %>%
                summarise(NumUsed  = n()) %>%
                ungroup()
              
              schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                mutate(personCounts = personCounts - NumUsed,
                       personCounts = ifelse(personCounts <= 0, 0, personCounts)) %>%
                select(-NumUsed)

              schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                select(schoolID, personAge, personCounts, schoolType, originalCounts)

              schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)

              multipleKids <- bind_rows(InitialMatches, SecondMatches)

              remainingAges2 <- peopleInHousehold %>%
                filter(!personID %in% c(multipleKids$personID))

              if(nrow(remainingAges2) == 0) {


                if(exists("ChildSchoolMatches")) {

                  ChildSchoolMatches <- bind_rows(ChildSchoolMatches, multipleKids)

                  # closes if(exists("ChildSchoolMatches")) {

                } else {

                  ChildSchoolMatches <- multipleKids

                  # closes else to if(exists("ChildSchoolMatches")) {
                }

                # closes if(nrow(remainingAges) == 0) {
              }


              # closes else to if(nrow(schoolTypeCheck) > 1) { FOR SECOND TRANCHE
            }



          if(nrow(remainingAges2) > 0) {


            stop("Need third tranche for kids in same-sex schools, twins households \n")
          }

            ###############################################
            # second tranche for kids in same schools closed
            # twins
            ###############################################
          }



          ###############################################
          ###############################################
          # finish single sex schools code block
          ###############################################
          ###############################################

          # closes if(nrow(schoolTypeCheck) > 1) {
          } else {

          ###############################################
          ###############################################
          # start there are no same sex schools code block
          ###############################################
          ###############################################

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
              slice_head(n=1)

            InitialMatches <- schoolsubset %>%
              filter(schoolID %in% c(selectedSchool$schoolID)) %>%
              select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))


            schoolInfo <- InitialMatches %>%
              select(schoolID, personAge) %>%
              group_by(schoolID, personAge) %>%
              summarise(NumUsed  = n()) %>%
              ungroup()


            schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
              mutate(personCounts = personCounts - NumUsed,
                     personCounts = ifelse(personCounts <= 0, 0, personCounts)) %>%
              select(-NumUsed)


            schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
              select(schoolID, personAge, personCounts, schoolType, originalCounts)

            schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)

            remainingAges <- peopleInHousehold %>%
              filter(!personID %in% c(InitialMatches$personID))


            if(nrow(remainingAges) == 0) {

              if(exists("ChildSchoolMatches")) {

                ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)



                # closes if(exists("ChildSchoolMatches")) {

              } else {

                ChildSchoolMatches <- InitialMatches

                # closes else to if(exists("ChildSchoolMatches")) {
              }

              # closes if(nrow(remainingAges) == 0) {
            } else {


              cat("entered else if \n")

              schoolsubset <- schoolsubset %>%
                filter(personAge %in% remainingAges$personAge)

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
                slice_head(n=1)

              SecondMatches <- schoolsubset %>%
                filter(schoolID %in% c(selectedSchool$schoolID)) %>%
                select(-c(personCounts, schoolType, originalCounts, isMatch, remainingPeople))

              schoolInfo <- SecondMatches %>%
                select(schoolID, personAge) %>%
                group_by(schoolID, personAge) %>%
                summarise(NumUsed  = n()) %>%
                ungroup()


              schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
                mutate(personCounts = personCounts - NumKids,
                       personCounts = ifelse(personCounts <= 0, 0, personCounts)) %>%
                select(-NumUsed)

              schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
                select(schoolID, personAge, personCounts, schoolType, originalCounts)

              schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)

              multipleKids <- bind_rows(InitialMatches, SecondMatches)

              remainingAges2 <- peopleInHousehold %>%
                filter(!personID %in% c(multipleKids$personID))

              if(nrow(remainingAges2) == 0) {

                if(exists("ChildSchoolMatches")) {

                  ChildSchoolMatches <- bind_rows(ChildSchoolMatches, multipleKids)

                  # closes if(exists("ChildSchoolMatches")) {

                } else {

                  ChildSchoolMatches <- multipleKids

                  # closes else to if(exists("ChildSchoolMatches")) {
                }

                # closes if(nrow(remainingAges2) == 0) {
              }

              #closes } else if (nrow(remainingAges) > 0) {
            }

            if(nrow(remainingAges2) > 0) {

              stop("Twins, no single sex schools, need third tranche")
            }

            #TODO code block here if remainingAges2 > 0

            ###############################################
            ###############################################
            # end there are no same sex schools code block
            ###############################################
            ###############################################

          # closes else to if(nrow(schoolTypeCheck) > 1) {
        }








         # closes if(probUsed == 1) {
      } else {

        ###############################################
        ###############################################
        ###############################################
        # all kids to different schools
        ###############################################
        ###############################################
        ###############################################
        
        peopleInHouseholdSeq <- peopleInHousehold %>%
          mutate(posInHousehold = row_number())
        

        schoolsubset <- left_join(peopleInHouseholdSeq, schoolsRenamed, by = "personAge",
                                  relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
          mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
          filter(personCounts > 0,
                 isMatch == "Y") %>%
          mutate(remainingPeople = personCounts - 1,
                 origPersonAge = personAge) %>%
          select(-personAge) %>%
          rename(personAge = posInHousehold) %>%
          filter(remainingPeople > -1) %>%
          select(personAge, schoolID, remainingPeople)
          
        
        matchedSchools <- kidsAdd(schoolsubset)
        
        InitialMatches <- matchedSchools %>%
          rename(posInHousehold = personAge) %>%
          left_join(peopleInHouseholdSeq, by = "posInHousehold") %>%
          select(-posInHousehold)
     
        schoolInfo <- InitialMatches %>%
          select(schoolID, personAge)

        # reduce the counts in the schools dataframe
        schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
          select(schoolID, personAge, personCounts, schoolType, originalCounts) %>%
          mutate(personCounts = personCounts - 1,
                 personCounts = ifelse(personCounts < 0, 0, personCounts)
          )

        schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
          select(schoolID, personAge, personCounts, schoolType, originalCounts)

        schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)

        remainingAges <- peopleInHousehold %>%
          filter(!(personID %in% c(InitialMatches$personID)))


        if(nrow(remainingAges) == 0) {


          if(exists("ChildSchoolMatches")) {

            ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches)


            # closes if(exists("ChildSchoolMatches")) {

          } else {

            ChildSchoolMatches <- InitialMatches
          # all kids succesfully added

          }

        }

        if(nrow(remainingAges) > 0) {
          
          stop("There are not enough schools available for every child to go to a different school for household", CurrentHousehold, "\n")
        # 
        #   # kids remaining after first allocation
        #   # see if spots are available at the same schools
        # 
        #   schoolsubset <- left_join(remainingAges, schoolsRenamed, by = "personAge",
        #                          relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
        #     mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
        #     filter(isMatch == "Y") %>%
        #     ungroup() %>%
        #     mutate(remainingPeople = personCounts - 1) %>%
        #     filter(remainingPeople > -1) %>%
        #     select(personAge, schoolID, remainingPeople)
        #   
        #   return(schoolsubset)
        # 
        #   if(nrow(schoolsubset) > 0) {
        # 
        #     secondSchools <- kidsAdd(schoolsubset)
        # 
        #   if(nrow(secondSchools) == nrow(remainingAges)) {
        # 
        #     matchedSchoolsWithType <- left_join(secondSchools, schoolsRenamed, by = c("schoolID", "personAge")) %>%
        #       select(schoolID, personAge, schoolType)
        # 
        #     SecondMatches <- left_join(matchedSchoolsWithType, remainingAges, by = "personAge") %>%
        #       mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N")) %>%
        #       filter(isMatch == "Y") %>%
        #       select(-c(isMatch, schoolType)) %>%
        #       distinct(schoolID, .keep_all = TRUE)
        # 
        #     schoolInfo <- SecondMatches %>%
        #       select(schoolID, personAge)
        # 
        #     # reduce the counts in the schools dataframe
        #     schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
        #       select(schoolID, personAge, personCounts, schoolType, originalCounts) %>%
        #       mutate(personCounts = personCounts - 1,
        #              personCounts = ifelse(personCounts < 0, 0, personCounts)
        #       )
        # 
        #     schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
        #       select(schoolID, personAge, personCounts, schoolType, originalCounts)
        # 
        #     schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
        # 
        #     if(exists("ChildSchoolMatches")) {
        # 
        #       ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches, SecondMatches)
        # 
        # 
        #       # closes if(exists("ChildSchoolMatches")) {
        # 
        #     } else {
        # 
        #       ChildSchoolMatches <- bind_rows(InitialMatches, SecondMatches)
        #       # all kids successfully added
        # 
        #     }
        # 
        #     }
        # 
        #   # may not have spaces at the same school, if not, pull a new school
        # 
        #   if(nrow(secondSchools) < nrow(remainingAges)) {
        # 
        #     if(nrow(secondSchools) == 0) {
        #       
        #       SecondMatches <- left_join(remainingAges, schoolsRenamed, by = "personAge",
        #                                 relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
        #         mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
        #         filter(isMatch == "Y") %>%
        #         ungroup() %>%
        #         mutate(remainingPeople = personCounts - 1) %>%
        #         filter(remainingPeople > -1) %>%
        #         group_by(personID) %>%
        #         slice_sample(n=1, weight_by = remainingPeople, replace = FALSE)
        #       
        #       stop("May still need more people if there are no roll slots available, twins, different schools")
        #       return(SecondMatches)
        #     }
        #      
        # 
        #     SecondMatches <- left_join(remainingAges, schoolsRenamed, by = "personAge",
        #                                relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
        #       mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
        #       filter(isMatch == "Y") %>%
        #       ungroup() %>%
        #       mutate(remainingPeople = personCounts - 1) %>%
        #       filter(remainingPeople > -1) %>%
        #       group_by(personID) %>%
        #       slice_sample(n=1, weight_by = remainingPeople, replace = FALSE)
        # 
        #     return(schoolsubset)
        #     
        #     # closes if(nrow(OtherKids) == 0) {
        #   }
        # 
        #   #
        #   } else {
        #     
        #     # no matches to schools with non-zero places
        # 
        #     schoolsubset <- left_join(remainingAges, schoolsRenamed, by = "personAge",
        #                               relationship = "many-to-many") %>% # have grouped by sex as well, next line handles it
        #       mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N"))  %>%
        #       filter(isMatch == "Y") %>%
        #       ungroup() %>%
        #       select(personAge, schoolID, remainingPeople)
        #     
        #     secondSchools <- kidsAdd(schoolsubset)
        #     
        #     if(nrow(secondSchools) == nrow(remainingAges)) {
        #       
        #       matchedSchoolsWithType <- left_join(secondSchools, schoolsRenamed, by = c("schoolID", "personAge")) %>%
        #         select(schoolID, personAge, schoolType)
        #       
        #       SecondMatches <- left_join(matchedSchoolsWithType, remainingAges, by = "personAge") %>%
        #         mutate(isMatch = ifelse(schoolType == "C" | schoolType == personType, "Y", "N")) %>%
        #         filter(isMatch == "Y") %>%
        #         select(-c(isMatch, schoolType)) %>%
        #         distinct(schoolID, .keep_all = TRUE)
        #       
        #       schoolInfo <- SecondMatches %>%
        #         select(schoolID, personAge)
        #       
        #       # reduce the counts in the schools dataframe
        #       schoolsSelected <- left_join(schoolInfo, schoolsRenamed, by = c("schoolID", "personAge")) %>%
        #         select(schoolID, personAge, personCounts, schoolType, originalCounts) %>%
        #         mutate(personCounts = personCounts - 1,
        #                personCounts = ifelse(personCounts < 0, 0, personCounts)
        #         )
        #       
        #       schoolsNotSelected <- anti_join(schoolsRenamed, schoolInfo, by = c("schoolID", "personAge")) %>%
        #         select(schoolID, personAge, personCounts, schoolType, originalCounts)
        #       
        #       schoolsRenamed <- bind_rows(schoolsNotSelected, schoolsSelected)
        #       
        #       if(exists("ChildSchoolMatches")) {
        #         
        #         ChildSchoolMatches <- bind_rows(ChildSchoolMatches, InitialMatches, SecondMatches)
        #         
        #         
        #         # closes if(exists("ChildSchoolMatches")) {
        #         
        #       } else {
        #         
        #         ChildSchoolMatches <- bind_rows(InitialMatches, SecondMatches)
        #         # all kids succesfully added
        #         
        #       }
        #       
        #     }
        #     
        #     if(nrow(secondSchools) < nrow(remainingAges)) {
        #       
        #       stop("nrow(secondSchools) < nrow(remainingAges)")
        #       
        #       
        #       
        #       # closes if(nrow(OtherKids) == 0) {
        #     }
        # 
        #     # matched to schools with no roll spots available.
        # }
        # 
        #  # all kids have had schools added
        }


        # closes else to # closes if(probUsed == 1) {
      }

      # closes if(NumKids > 1 & max(personAgesMaxCount$CountsByAge) > 1) {
    }
    
    
    # closes for(i in 1:nrow(ListofHouseholds))
    
    # cat("The number of school roll slots remaining is", sum(schoolsRenamed$personCounts), "\n")
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
  

  Output <- list()
  Output$Population <- allPeople
  Output$Schools <- schoolsOutput
  return(Output)
  
  # closes function
}

