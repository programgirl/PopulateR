#' @importFrom data.table :=
#' @importFrom dplyr arrange bind_rows distinct filter full_join group_by mutate rename select slice ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang .data sym !!
NULL

#' Creates the four data frames of weighted contact pairs for use in Covasim
#'
#' Creates the household, school, workplace, and contacts layers, from ABMPop, for use with the Python package Covasim. A 1xn data frame of ages is also created.
#'
#' There are three restrictions for use. First, the place2 codes for preschool, primary school, and secondary school must be set to "P801000", "P802100", and "P802200", respectively. Second, at least one school type must be "TRUE" as Covasim requires a school layer. Third, the place2 value for people who are not in school, and not in a workplace, must be "Not employed".
#'
#'
#' @export
#' @param ABMPop The agent-based modelling data frame.
#' @param ABMID The variable containing the unique identifier for each person, in the ABMPop data frame.
#' @param ABMAge The variable containing the ages, in the in the ABMPop data frame.
#' @param place1 The variable containing the Household ID.
#' @param place2 The variable containing the school and workplace IDs.
#' @param ECE Are ECE centres open? Default is TRUE, change to FALSE if ECEs are to close.
#' @param PSchool Are primary schools open? Default is TRUE, change to FALSE if primary schools are to close.
#' @param SSchool Are secondary schools open? Default is TRUE, change to FALSE if secondary schools are to close.
#' @param contacts A data frame consisting of existing contact pairs. The first two variables define the two people in the pair.
#' @param excludeDF A data frame of industries to exclude. This must be the relevant IndNum variable in the ABMPop data frame. If this data frame is not included, all industries will be represented in the output data frame.

#' @return A data frame of the household, school, workplace, contact layers, and people's ages, for use in Covasim.
#'
#'
ABMToCova <- function(ABMPop, ABMID, ABMAge, place1, place2, ECE = TRUE, PSchool = TRUE, SSchool = TRUE, contacts = NULL,
                      excludeDF = NULL) {

  IDcolName <- sym(names(ABMPop[ABMID]))
  AgeColName <- sym(names(ABMPop[ABMAge]))
  HouseholdIDcolName <- sym(names(ABMPop[place1]))
  PlaceTwocolName <- sym(names(ABMPop[place2]))

  ABMPop <- ABMPop %>%
    rename(ID = !! IDcolName,
           Age = !! AgeColName,
           HouseholdID = !! HouseholdIDcolName,
           PlaceTwo = !! PlaceTwocolName) %>%
    select(.data$ID, .data$HouseholdID, .data$PlaceTwo, .data$IndCode, .data$IndName, .data$Age)


  # TODO implement beta other than 1
  # TODO when beta is implemented, must ensure it is between 0 and 1
  # if(is.null(betaDF)) {
   # }




  ############################################################################################
  # Household Layer
  ############################################################################################

  h1 <- full_join(ABMPop, ABMPop, by = "HouseholdID", relationship = "many-to-many") %>%
    rename(p1 = .data$ID.x,
           p2 = .data$ID.y) %>%
    filter(.data$p1 != .data$p2) %>%
    group_by(grp = paste(pmax(.data$p1, .data$p2), pmin(.data$p1, .data$p2), sep = "_")) %>%
    slice(1) %>%
    ungroup() %>%
    select(.data$p1, .data$p2)

  h <- h1 %>%
    mutate(beta=1)




  ############################################################################################
  # School Layer
  ############################################################################################

  SchoolLayer <- ABMPop %>%
    filter(.data$IndCode %in% c("P801000", "P802100", "P802200"))

  if(ECE == FALSE & PSchool == FALSE & SSchool == FALSE) {

    stop("All three school types are set to N. A school layer must be output. \n")
  }

  if(ECE == FALSE | PSchool == FALSE | SSchool == FALSE) {

    warning("At least one educational type is being excluded. \n")



    # remove ECE
    if(ECE == FALSE) {
      SchoolLayer <- SchoolLayer %>%
        filter(! .data$IndCode == "P801000")
    }

    # remove primary schools
    if(PSchool == FALSE) {
      SchoolLayer <- SchoolLayer %>%
        filter(! .data$IndCode == "P802100")
    }

    # remove secondary schools
    if(SSchool == FALSE) {
      SchoolLayer <- SchoolLayer %>%
        filter(! .data$IndCode == "P802200")
      }

  }

  if(nrow(SchoolLayer) == 0) {

    stop("There must be a school layer \n")

  }

    Layer2 <- SchoolLayer

    s <- full_join(SchoolLayer, Layer2, by = "PlaceTwo", relationship = "many-to-many") %>%
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

  if(is.null(excludeDF)) {

    # if there are no industries to exclude, make all the contacts

  w1 <- ABMPop %>%
    filter(! .data$IndCode %in% c("P801000", "P802100", "P802200", "Not employed"))

  w2 <- full_join(w1, w1, by = "PlaceTwo", relationship = "many-to-many") %>%
    filter(.data$ID.x != .data$ID.y) %>%
    rename(p1 = .data$ID.x,
           p2 = .data$ID.y) %>%
    select(.data$p1, .data$p2) %>%
    group_by(grp = paste(pmax(.data$p1, .data$p2), pmin(.data$p1, .data$p2), sep = "_")) %>%
    slice(1) %>%
    ungroup() %>%
    select(- .data$grp)

  w <- w2 %>%
    mutate(beta=1)


  } else {

    # there are industries to exclude, remove them from the population

    w1 <- ABMPop %>%
      filter(! .data$IndCode %in% c("P801000", "P802100", "P802200", "Not employed")) %>%
      filter(! .data$IndCode %in% excludeDF[,1])

    w2 <- full_join(w1, w1, by = "PlaceTwo", relationship = "many-to-many") %>%
      filter(.data$ID.x != .data$ID.y) %>%
      rename(p1 = .data$ID.x,
             p2 = .data$ID.y) %>%
      select(.data$p1, .data$p2) %>%
      group_by(grp = paste(pmax(.data$p1, .data$p2), pmin(.data$p1, .data$p2), sep = "_")) %>%
      slice(1) %>%
      ungroup() %>%
      select(- .data$grp)

    w <- w2 %>%
      mutate(beta = 1)

  }


  ############################################################################################
  # Contacts Layer
  ############################################################################################

  c1 <- contacts %>%
    rename(p1 = .data$from,
           p2 = .data$to)

  c <- c1 %>%
    mutate(beta=1)

  ############################################################################################
  # Age per person
  # needs to be sorted by ID
  ############################################################################################

  age <- ABMPop %>%
    arrange(.data$ID) %>%
    select(.data$Age)

  OutputDataFrame <- list()
  OutputDataFrame$h <- h
  OutputDataFrame$s <- s
  OutputDataFrame$w <- w
  OutputDataFrame$c <- c
  OutputDataFrame$age <- age

  return(OutputDataFrame)
  # closes function
}
