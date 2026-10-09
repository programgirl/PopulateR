#' @importFrom data.table :=
#' @importFrom dplyr arrange bind_rows distinct filter full_join group_by mutate rename select slice ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data sym !!
NULL

#' Creates the four data frames of weighted contact pairs for use in Covasim, plus age and ID matching data frames.
#'
#' Creates the household, school, workplace, and contacts layers, from ABMPop, for use with the Python package Covasim. This function can be used with other data frames, as long as the following requirements are met: a single variable of unique IDs, a variable for age, a variable for sex, a single variable for household ID, a single variable for the second location of people and an associated variable that indicates whether the second location is a workplace or school. For people who stay in the household, the second location must be their household ID and the associated placetype must be "H". These people will only appear in the workplaces layer if at least one other person is household-bound.
#' 
#' A 1xn data frame of ages, and a 1xn data frame of sexes, is also created. The household, school, and workplace layers are identified using the placetype variable. As Covasim requires sequential numbering of people from 0 to max-1, the function will produce the corrected ID numbers.
#'
#'
#' @export
#' @param ABMPop The agent-based modelling data frame.
#' @param ABMID The variable containing the unique identifier for each person, in the ABMPop data frame.
#' @param ABMAge The variable containing the ages, in the in the ABMPop data frame.
#' @param ABMSex The variable containing the sexes, in the ABMPop data frame. This must be in the format where females have the value 0, "F", or "Female" and males have the value "1", "M", or "Male".
#' @param place1 The variable containing the Household ID.
#' @param place2 The variable containing the school and workplace codes.
#' @param placetype The variable defining the layer that the place2 variable represents. Valid values are "H" (person remains in the household, e.g. person is not employed), "S" (child at school or school staff member), "W" (employed person), "R" (rest home resident, if identified separately to rest home staff). "W" and "R" are used to construct the workplace layer. 
#' @param contacts A data frame consisting of existing contact pairs. The first two variables define the two people in the pair.
#' @param agebyID if TRUE, the function will output a data frame of the age for each of the new IDs. This is not used in Covasim, which requires a file that only contains age.The default value is FALSE.

#' @return A data frame of the household, school, workplace, contact layers, people's ages, people's sexes, and a data frame of original versus corrected IDs, for use in Covasim.
#'
#'
ABMToCova <- function(ABMPop, ABMID, ABMAge, ABMSex, place1, place2, placetype, contacts = NULL, agebyID = FALSE) {

  IDcolName <- sym(names(ABMPop[ABMID]))
  AgeColName <- sym(names(ABMPop[ABMAge]))
  SexColName <- sym(names(ABMPop[ABMSex]))
  HouseholdIDcolName <- sym(names(ABMPop[place1]))
  PlaceTwocolName <- sym(names(ABMPop[place2]))
  PlaceTypecolName <- sym(names(ABMPop[placetype]))

  ABMPop <- ABMPop %>%
    rename(ID = !! IDcolName,
           Age = !! AgeColName,
           OrigSex = !! SexColName,
           HouseholdID = !! HouseholdIDcolName,
           PlaceTwo = !! PlaceTwocolName,
           PlaceType = !! PlaceTypecolName) %>%
    mutate(Sex = ifelse(.data$OrigSex %in% c("F", "Female"), 0, 
                        ifelse(.data$OrigSex %in% c("M", "Male"), 1, .data$OrigSex))) %>%
    select(.data$ID, .data$HouseholdID, .data$PlaceTwo, .data$PlaceType, .data$Age, .data$Sex)

  if(is.null(contacts)) {
    
    stop("A contacts data frame must be supplied.")

  }
  
  ############################################################################################
  # Prep for renumbering
  ############################################################################################
  
  # IDs of the people in the contacts data frame
  
  IntContactsp1 <- contacts[,1]
  IntContactsp2 <- contacts[,2]
  
  IntContacts <- as.vector(rbind(IntContactsp1, IntContactsp2))
  IntContacts <- unique(IntContacts)

  
  # IDs of the people in the household layer, households with > 1 resident
  
  HouseholdIDs <- ABMPop %>%
    select(.data$HouseholdID) %>%
    group_by(.data$HouseholdID) %>%
    summarise(NumPeople = n()) %>%
    filter(.data$NumPeople > 1) %>%
    left_join(ABMPop, by = "HouseholdID") %>%
    ungroup() %>%
    select(.data$ID)
  
  HouseholdVector <- pull(HouseholdIDs, .data$ID)
 
  # bind the people in multiple-person households to those with contacts, remove duplicates
  
  MergedContacts <- unique(c(IntContacts, HouseholdVector))
  
  # finally, get the people who share a second location
  
  LocationIDs <- ABMPop %>%
    select(.data$PlaceTwo, .data$PlaceType) %>%
    group_by(.data$PlaceTwo, .data$PlaceType) %>%
    summarise(NumPeople = n()) %>%
    filter(.data$NumPeople > 1) %>%
    left_join(ABMPop, by = c("PlaceTwo", "PlaceType")) %>%
    ungroup() %>%
    select(.data$ID)
    
  LocationVector <- pull(LocationIDs, .data$ID)
  
  FinalMergedContacts <- unique(c(MergedContacts, LocationVector))
  

  
  
  
  
  
  ############################################################################################
  # Delete out people who don't have contacts
  ############################################################################################

  ABMPop <- ABMPop %>%
    filter(.data$ID %in% FinalMergedContacts)
  
  IDReordering <- ABMPop %>%
    select(.data$ID) %>%
    distinct() %>%
    arrange(.data$ID) %>%
    mutate(NewID = row_number()-1)
  
  ABMPopSeqNumbed <- left_join(ABMPop, IDReordering, by = "ID")

  ABMPop <- ABMPopSeqNumbed %>%
    select(- .data$ID) %>%
    rename(ID = .data$NewID)

  ############################################################################################
  # Household Layer
  ############################################################################################

  h <- full_join(ABMPop, ABMPop, by = "HouseholdID", relationship = "many-to-many") %>%
    rename(p1 = .data$ID.x,
           p2 = .data$ID.y) %>%
    filter(.data$p1 != .data$p2) %>%
    group_by(grp = paste(pmax(.data$p1, .data$p2), pmin(.data$p1, .data$p2), sep = "_")) %>%
    slice(1) %>%
    ungroup() %>%
    select(c(.data$p1, .data$p2)) %>%
    mutate(beta=1)


  ############################################################################################
  # School Layer
  ############################################################################################

  s1 <- ABMPop %>%
    filter(.data$PlaceType == "S")


  if(nrow(s1) == 0) {

    stop("There must be a school layer \n")

  }

    s <- full_join(s1, s1, by = "PlaceTwo", relationship = "many-to-many") %>%
      filter(.data$ID.x != .data$ID.y) %>%
      rename(p1 = .data$ID.x,
             p2 = .data$ID.y) %>%
    select(c(.data$p1, .data$p2))  %>%
      group_by(grp = paste(pmax(.data$p1, .data$p2), pmin(.data$p1, .data$p2), sep = "_")) %>%
      slice(1) %>%
      ungroup() %>%
      select(- .data$grp) %>%
      mutate(beta = 1)

  ############################################################################################
  # Workplace Layer
  ############################################################################################

    # workers
  w1 <- ABMPop %>%
    filter(.data$PlaceType %in% c("W", "R"))

    if(nrow(w1) == 0) {

      stop("There must be a workplace layer \n")

    }

  w2 <- full_join(w1, w1, by = "PlaceTwo", relationship = "many-to-many") %>%
    filter(.data$ID.x != .data$ID.y) %>%
    rename(p1 = .data$ID.x,
           p2 = .data$ID.y) %>%
    select(c(.data$p1, .data$p2)) %>%
    group_by(grp = paste(pmax(.data$p1, .data$p2), pmin(.data$p1, .data$p2), sep = "_")) %>%
    slice(1) %>%
    ungroup() %>%
    select(- .data$grp)

  workers <- w2 %>%
    mutate(beta=1)


  # those at home, code == "H"

  hw1 <- ABMPop %>%
    filter(.data$PlaceType %in% c("H"))

  if(nrow(hw1) > 1) {

    hw2 <- full_join(hw1, hw1, by = "PlaceTwo", relationship = "many-to-many") %>%
      filter(.data$ID.x != .data$ID.y) %>%
      rename(p1 = .data$ID.x,
             p2 = .data$ID.y) %>%
      select(c(.data$p1, .data$p2)) %>%
      group_by(grp = paste(pmax(.data$p1, .data$p2), pmin(.data$p1, .data$p2), sep = "_")) %>%
      slice(1) %>%
      ungroup() %>%
      select(- .data$grp)

    if(nrow(hw2) > 0)
    homers <- hw2 %>%
      mutate(beta=1)

    w <- bind_rows(workers, homers)

  } else {

    w <- workers
  }

  ############################################################################################
  # Contacts Layer
  ############################################################################################

  communitycol1 <- contacts %>%
    select(.data$from) %>%
    left_join(IDReordering, by = c("from" = "ID")) %>%
    distinct()

  communitycol2 <- contacts %>%
    select(.data$to) %>%
    left_join(IDReordering, by = c("to" = "ID")) %>%
    distinct()

  c = left_join(contacts, communitycol1, by = "from") %>%
   left_join(communitycol2, by = "to") %>%
    select(c(.data$NewID.x, .data$NewID.y)) %>%
    rename(p1 = .data$NewID.x,
           p2 = .data$NewID.y) %>%
    mutate(beta=1)

  ############################################################################################
  # Age per person
  # needs to be sorted by ID
  ############################################################################################
  
  age <- ABMPop %>%
   arrange(.data$ID) %>%
   distinct(.data$ID,.data$Age) %>%
   select(.data$Age)
  
  if(agebyID == TRUE) {
    
    agebyID <- ABMPop %>%
      arrange(.data$ID) %>%
      distinct(.data$ID,.data$Age)
  }

  
  ############################################################################################
  # Sex per person
  # needs to be sorted by ID
  ############################################################################################
  
  sex <- ABMPop %>%
    arrange(.data$ID) %>%
    distinct(.data$ID,.data$Sex) %>%
    select(.data$Sex)



  OutputDataFrame <- list()
  OutputDataFrame$h <- h
  OutputDataFrame$s <- s
  OutputDataFrame$w <- w
  OutputDataFrame$c <- c
  OutputDataFrame$age <- age
  OutputDataFrame$sex <- sex
  OutputDataFrame$Renumbering <- IDReordering
  
  if(exists("agebyID")) {
    OutputDataFrame$agebyID <- agebyID
  }

  return(OutputDataFrame)
  # closes function
}
