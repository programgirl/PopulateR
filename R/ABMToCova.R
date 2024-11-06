#' Creates a data frame of weighted contact pairs.
#'
#' This function creates the household, school, workplace, and contacts layers, from ABMPop, for use with the Python package Covasim. A 1xn dataframe of ages is also created. The data frame must be converted to a Python n x 3 flat dictionary.
#' 
#'
#' @export
#' @param ABMPop The agent-based modelling dataframe.
#' @param ABMID The variable containing the unique identifier for each person, in the ABMPop data frame.
#' @param ABMAge The variable containing the ages, in the in the ABMPop data frame.
#' @param place1 TThe variable containing the Household ID.
#' @param place2 The variable containing the school and workplace IDs.
#' @param ECE Are ECE centres open? Default is "Y", change to "N" if ECEs are to close.
#' @param PSchool Are primary schools open? Default is "Y", change to "N" if primary schools are to close.
#' @param SSchool Are secondary schools open? Default is "Y", change to "N" if secondary schools are to close.
#' @param contacts A data frame consisting of existing contact pairs. The first two variables define the two people in the pair.
#' @param excludeDF A data frame of industries to exclude. This must be the relevant IndNum variable in the ABMPop data frame. If this data frame is not included, all industries will be represented in the output data frame. 

#' @return A data frame of the household, school, workplace, contact layers, and people's ages, for use in Covasim.
#' 
#' 
ABMToCova <- function(ABMPop, ABMID, ABMAge, place1, place2, ECE = "Y", PSchool = "Y", SSchool = "Y", contacts = NULL, 
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
    select(ID, HouseholdID, PlaceTwo, IndCode, IndName, Age)
  
  
  # TODO implement beta other than 1
  # TODO when beta is implemented, must ensure it is between 0 and 1
  # if(is.null(betaDF)) {
  #   
  #   
  # } else {
  #   
  #   Internal <- as.data.frame(betaDF %>%
  #                               rename(IntAge = !! betaAge, 
  #                                      IntBeta = !! beta2Use))
  #   
  #   # cat("Internal constructed \n")
  #   # add sort
  #   
  #   if(priority == "Y") {
  #     
  #     Internal <- Internal %>%
  #       arrange(desc(IntAge))
  #     
  #   }
  #   
  #   if(priority == "O") {
  #     
  #     
  #     Internal <- Internal %>%
  #       arrange(IntAge)
  #   }
  #   
  #   # create copy of the output data frame to iterate through
  #   
  #   ODFCopy <- OutputDataFrame
  #   
  #   cat("ODFCopy made with ", nrow(ODFCopy), "rows \n")
  #   
  #   # start applying the replacement betas
  #   
  #   for (j in 1:nrow(Internal)) {
  #     
  #     # print(j)
  #     
  #     CurrentAge <- as.numeric(Internal$IntAge[j])
  #     CurrentBeta <- as.numeric(Internal$IntBeta[j])
  #     
  #     # only need their IDs
  #     
  #     PeepsThatAge <- ABMPop %>%
  #       filter(Age == CurrentAge) %>%
  #       select(ID)
  #     
  #     # cat("CurrentAge is ", CurrentAge, "and CurrentBeta is ", CurrentBeta, "\n") 
  #     
  #     # locate the people in the Covasim dataset
  #     
  #     WorkingPeople <- ODFCopy %>%
  #       filter(p1 %in% PeepsThatAge$ID | p2 %in% PeepsThatAge) %>%
  #       mutate(beta = CurrentBeta)
  #     
  #     if(exists("UpdatedBetas")) {
  #       
  #       UpdatedBetas <- bind_rows(UpdatedBetas, WorkingPeople)
  #       
  #       ODFCopy <- anti_join(ODFCopy, WorkingPeople, by = c("p1", "p2")) 
  #       
  #       # closes if(exists("UpdatedBetas")) {
  #       
  #     } else {
  #       
  #       UpdatedBetas <- WorkingPeople
  #       
  #       ODFCopy <- anti_join(ODFCopy, WorkingPeople, by = c("p1", "p2")) 
  #       
  #       
  #       
  #       # closes else to if(exists("UpdatedBetas")) {
  #     }
  #     
  #     # closes for (j in 1:nrow(Internal)) {
  #   }
  #   
  #   OutputDataFrame <- bind_rows(UpdatedBetas, ODFCopy)
  #   
  # }
  
  
  
  
  ############################################################################################
  # Household Layer
  ############################################################################################
  
  h1 <- full_join(ABMPop, ABMPop, by = "HouseholdID", relationship = "many-to-many") %>%
    rename(p1 = ID.x,
           p2 = ID.y) %>%
    select(p1, p2) %>%
    filter(p1 != p2) %>%
    group_by(grp = paste(pmax(p1, p2), pmin(p1, p2), sep = "_")) %>%
    slice(1) %>%
    ungroup() %>%
    select(-grp)
  
# 
#   h0 <- ABMPop %>%
#     select(ID) %>%
#     filter(!ID %in% c(h1$p1, h1$p2)) %>%
#     rename(p1 = ID) %>%
#     mutate(p2=0)
  # 
  # 
  # h <- bind_rows(h1, h0) %>%
  #   mutate(beta = 1)

  
  h <- h1 %>%
    mutate(beta=1)
  
  
  
  
  ############################################################################################
  # School Layer 
  ############################################################################################
  
  SchoolLayer <- ABMPop %>%
    filter(IndCode %in% c("P801000", "P802100", "P802200"))

  if(ECE == "N" | PSchool == "N" | SSchool == "N") {
    
    cat("At least one educational type is being excluded \n")
    
    
    
    # remove ECE
    if(ECE == "N") {
      SchoolLayer <- SchoolLayer %>%
        filter(!IndCode == "P801000")
    }

    # remove primary schools
    if(PSchool == "N") {
      SchoolLayer <- SchoolLayer %>%
        filter(!IndCode == "P802100")
    }

    # remove secondary schools
    if(SSchool == "N") {
      SchoolLayer <- SchoolLayer %>%
        filter(!IndCode == "P802200")
      }
    
  }

  if(nrow(SchoolLayer) > 0) {
    
    Layer2 <- SchoolLayer
    
    s1 <- full_join(SchoolLayer, Layer2, by = "PlaceTwo", relationship = "many-to-many") %>%
      filter(ID.x != ID.y) %>%
      rename(p1 = ID.x,
             p2 = ID.y) %>%
    select(c(p1,p2))  %>%
      group_by(grp = paste(pmax(p1, p2), pmin(p1, p2), sep = "_")) %>%
      slice(1) %>%
      ungroup() %>%
      select(-grp)

  }
  
  # create all the 0s
  
  if(exists("s1")) {
    
    s2 <- ABMPop %>%
      select(ID) %>%
      filter(!ID %in% s1$p1) %>%
      rename(p1=ID) %>%
      mutate(p2=0)
    
   s <- bind_rows(s1,s2) %>%
     mutate(beta = 1)

  } else {
    
    s <- ABMPop %>%
      select(ID) %>%
      rename(p1=ID) %>%
      mutate(p2=0) %>%
      distinct() %>%
      mutate(beta = 1)
    
  }
  
  
  ############################################################################################
  # Workplace Layer 
  ############################################################################################
  
  if(is.null(excludeDF)) {
    
    # if there are no industries to exclude, make all the contacts
  
  w1 <- ABMPop %>%
    filter(!IndCode %in% c("P801000", "P802100", "P802200", "Not employed"))
  
  w2 <- full_join(w1, w1, by = "PlaceTwo", relationship = "many-to-many") %>%
    filter(ID.x != ID.y) %>%
    rename(p1 = ID.x,
           p2 = ID.y) %>%
    select(p1, p2) %>%
    group_by(grp = paste(pmax(p1, p2), pmin(p1, p2), sep = "_")) %>%
    slice(1) %>%
    ungroup() %>%
    select(-grp)
  
  # w3 <- ABMPop %>%
  #   select(ID) %>%
  #   filter(!ID %in% w2$p1) %>%
  #   rename(p1 = ID) %>%
  #   mutate(p2 = 0)
  # 
  # w <- bind_rows(w2, w3) %>%
  #   mutate(beta = 1)
  
  w <- w2 %>%
    mutate(beta=1)
  

  } else {
    
    # there are industries to exclude, remove them from the population
    
    w1 <- ABMPop %>%
      filter(!IndCode %in% c("P801000", "P802100", "P802200", "Not employed")) %>%
      filter(!IndCode %in% excludeDF[,1])
    
    w2 <- full_join(w1, w1, by = "PlaceTwo", relationship = "many-to-many") %>%
      filter(ID.x != ID.y) %>%
      rename(p1 = ID.x,
             p2 = ID.y) %>%
      select(p1, p2) %>%
      group_by(grp = paste(pmax(p1, p2), pmin(p1, p2), sep = "_")) %>%
      slice(1) %>%
      ungroup() %>%
      select(-grp)
    # 
    # w3 <- ABMPop %>%
    #   select(ID) %>%
    #   filter(!ID %in% w2$p1) %>%
    #   rename(p1 = ID) %>%
    #   mutate(p2 = 0)
    # 
    # w <- bind_rows(w2, w3) %>%
    #   mutate(beta = 1)
    
    w <- w2 %>%
      mutate(beta = 1)

  }
  
 

  ############################################################################################
  # Contacts Layer 
  ############################################################################################
  
  c1 <- contacts %>%
    rename(p1 = from,
           p2 = to)
  # 
  # c2 <- contacts %>%
  #   rename(p1 = to,
  #          p2 = from)
  # 
  # c3 <- ABMPop %>%
  #   select(ID) %>%
  #   filter(!ID %in% c(c1$p1, c1$p2)) %>%
  #   rename(p1 = ID) %>%
  #   mutate(p2 = 0)
# 
#   c <- bind_rows(c1, c2, c3) %>%
#     mutate(beta = 1)
#   
  
  c <- c1 %>%
    mutate(beta=1)
  
  ############################################################################################
  # Age per person 
  # needs to be sorted by ID
  ############################################################################################
  
  age <- ABMPop %>%
    arrange(ID) %>%
    select(Age)
  


  
  
  OutputDataFrame <- list()
  OutputDataFrame$h <- h
  OutputDataFrame$s <- s
  OutputDataFrame$w <- w
  OutputDataFrame$c <- c
  OutputDataFrame$age <- age

  return(OutputDataFrame)
  # closes function
}