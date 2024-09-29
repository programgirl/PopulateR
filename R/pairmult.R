#' Create child- parent/guardian pairs as many-to-one and place them into new households.
#' This function creates a data frame of child-parent/guardian pairs, based on a distribution of age differences. Multiple children will be matched to the same parent.
#' Two data frames are required: one for children and one for potential parents.
#' The minimum and maximum ages of parents must be specified. This ensures that there are no parents who were too young (e.g. 11 years) or too old (e.g. 70 years) at the time the child was born. The presence of too young and too old parents is tested throughout this function. Thus, pre-cleaning the parents data frame is not required.
#' 
#' @export
#' @param children The data frame containing the children to be paired with a parent/guardian.
#' @param chlid The variable containing the unique ID for each person,in the children data frame.
#' @param chlage The age variable, in the children data frame.
#' @param numchild The number of children that are required in each household.
#' @param twinprob The probability that a person is a twin.
#' @param parents The data frame containing the potential parents.(This data frame must contain at least the same number of observations as the children data frame.)
#' @param parid The variable containing the unique ID for each person,in the parents data frame.
#' @param parage The age variable, in the parent data frame.
#' @param minparage The youngest age at which a person becomes a parent. The default value is NULL, which will cause the function to stop.
#' @param maxparage The oldest age at which a person becomes a parent. The default value is NULL, which will cause the function to stop.
#' @param HHStartNum The starting value for HHNumVar Must be numeric.
#' @param HHNumVar The name for the household variable.
#' @param userseed If specified, this will set the seed to the number provided. If not, the normal set.seed() function will be used.
#' @param maxdiff The maximum age difference for the children in a household ages. This is applied to the first child randomly selected for the household, so overall age differences may be 2* maxdiff. Default value is no constraints on child age differences in the household.
#' 
#' @return A list of three  data frames. $Matched contains the data frame of child-parent matches. $Adults contains any unmatched observations from the parents data frame. $Children contains any unmatched observations from the children data frame. $Adults and/or $Children may be empty data frames.
#'
#' @examples
#' library(dplyr)
#' set.seed(1)
#' Parents <- Township %>%
#'   filter(Relationship == "Partnered", Age > 18) %>%
#'   slice_sample(n = 500) %>%
#'   mutate(HouseholdID = row_number()+500)
#'
#' Children <- Township %>%
#'   filter(Relationship == "NonPartnered", Age < 20) %>%
#'   slice_sample(n = 400)
#'
#' # example with assigning two children to a parent
#' # the same number of children is assigned to all parents
#' # adding two children to each parent
#'
#' ChildrenMatchedID <- childrenyes(Children, chlid = 3, chlage = 4, numchild = 5,
#'                      twinprob = .2, Parents, parid = 3, parage = 4,
#'                      minparage = 18, maxparage = 54,userseed = 4)
#'                      
pairchild <- function(children, chlid, chlage, numchild = 2, twinprob = 0, parents, parid, parage,
                      minparage = NULL, maxparage = NULL, HHStartNum = NULL, HHNumVar= NULL, 
                      userseed=NULL, maxdiff=1000)
  
{
  
  options(dplyr.summarise.inform=F)
  
  # check if the maximum difference between the child ages fits with the number of children
  if(maxdiff < (0.5*numchild)) {
    stop("The maximum age difference for the children is too small for non-twin families.")
  }
  
  
  # content check
  # child dataframe
  if (!chlid %in% names(children)) {
    stop("The ID variable in the children data frame does not exist.")
  }
  
  if (!chlage %in% names(children)) {
    stop("The age variable in the children data frame does not exist.")
  }
  
  # parent dataframe
  if (!parid %in% names(parents)) {
    stop("The ID variable in the parents data frame does not exist.")
  }
  
  if (!parage %in% names(parents)) {
    stop("The age variable in the parents data frame does not exist.")
  }
  
  if (is.null(minparage)) {
    stop("The minimum parent age must be supplied.")
  }
  
  if (is.null(maxparage)) {
    stop("The maximum parent age must be supplied.")
  }
  
  
  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################
  # smalldf ID variable
  chidcolName <- sym(names(children[chlid]))
  # smalldf age variable
  chagecolName <- sym(names(children[chlage]))
  
  # largedf ID variable
  paridcolName <- sym(names(parents[parid]))
  # largedf age variable
  paragecolName <- sym(names(parents[parage]))
  
  #####################################
  #####################################
  # end column names
  #####################################
  #####################################
  
  # more testing
  
  if (!any(duplicated(children[[chidcolName]])) == FALSE) {
    stop("The ID variable in the children data frame has duplicated values.")
  }
  
  if (!any(duplicated(parents[[paridcolName]])) == FALSE) {
    stop("The ID variable in the parents data frame has duplicated values.")
  }
  
  if (!is.numeric(children[[chagecolName]])) {
    stop("The age variable in the children data frame is not numeric.")
  }
  
  if (!is.numeric(parents[[paragecolName]])) {
    stop("The age variable in the parents data frame is not numeric.")
  }
  
  #####################################
  #####################################
  # start set up
  #####################################
  #####################################
  
  
  # create the internal data frames
  childrenRenamed <- children %>%
    rename(ChildID = !! chidcolName, 
           ChildAge = !! chagecolName)
  
  parentsRenamed <- parents %>%
    rename(ParentID = !! paridcolName, 
           ParentAge = !! paragecolName)
  
  # internal HHID variable
  currentHHID <- HHStartNum
  # give info on expected number of twin households
  # ExpctNumHH <- round(nrow(childrenRenamed)*twinprob) - round(nrow(childrenRenamed)*twinprob) %% 2

  ExpctNumHH <- round((nrow(childrenRenamed)*twinprob)/2,0)
  
  cat("Expected number of twins is", ExpctNumHH*2, "\n")
  
  # seed must come before first sample is cut
  if (!is.null(userseed)) {
    set.seed(userseed)
  }
  
  #####################################
  #####################################
  # end set up
  #####################################
  #####################################
  #####################################
  #####################################
  # Functions for twins and their siblings
  #####################################
  #####################################
  
  
  #####################################
  #####################################
  # Split into twins and non-twins
  #####################################
  #####################################
  
  if (ExpctNumHH > 0 & numchild == 2) {
    
    # restrict child dataframe to those of which there are at least two children, cannot twin if there isn't a second child
    # make sure the minimum and maximum ages have enough other child ages to meet the numkids and maxdiff requirements
    AgesForTwins <- childrenRenamed %>%
      group_by(ChildAge) %>%
      summarise(NumAge = n()) %>%
      filter(NumAge > 1)
    # select twin ages
    
    for (i in 1:ExpctNumHH) {
      
      # cat("Number of kids is", sum(AgesForTwins$NumAge), "\n")
      
      # get age, need to select randomly
      
      TwinAgeSelect <- AgesForTwins %>%
        slice_sample(n=1, weight_by = NumAge)
      
      CurrentAge <- TwinAgeSelect$ChildAge
      
      # pull children from the child dataframe who are that age
      SelectedKids <- childrenRenamed %>%
        filter(ChildAge==CurrentAge) %>%
        slice_sample(n=2, replace = FALSE) %>%
        mutate(internalHHID = currentHHID)
      
      # get parent
      NeededMin <- minparage + CurrentAge
      NeededMax <- maxparage + CurrentAge
      
      SelectedParent <- parentsRenamed %>%
        filter(between(ParentAge, NeededMin, NeededMax)) %>%
        slice_sample(n=1) %>%
        mutate(internalHHID = currentHHID)
      
      # add these to a file that will continue to be appended
      # make sure that the household ID is added.
      # separate the parent and child files at this point as the column names are different
      
      if(exists("ChildAgeMatch")) {
        
        ChildAgeMatch <- bind_rows(ChildAgeMatch, SelectedKids)
        ParentAgeMatch <- bind_rows(ParentAgeMatch, SelectedParent)
        currentHHID <- currentHHID + 1
        
      } else {
        
        ChildAgeMatch <- SelectedKids
        ParentAgeMatch <- SelectedParent
        currentHHID <- currentHHID + 1
        
      }
      
      # remove the children from the available children and update the counts to enter into the loop
      childrenRenamed <- childrenRenamed %>%
        filter(!(ChildID %in% c(SelectedKids$ChildID)))
      AgesForTwins <- childrenRenamed %>%
        group_by(ChildAge) %>%
        summarise(NumAge = n()) %>%
        filter(NumAge > 1)
      
      # remove the parent
      
      parentsRenamed <- parentsRenamed %>%
        filter(!(ParentID %in% c(SelectedParent$ParentID)))
      
      # have to add the additional twin stuff in here, need to check if children needed > 0
      
      # closes for (i in 1:ExpctNumHH) {
    }
    
    # closes if (ExpctNumHH > 0 & numchild == 2) {
  }
  
  
  #####################################
  #####################################
  # Loop if twin rate > 0 and numkids > 2
  #####################################
  #####################################
  
  if (ExpctNumHH > 0 & numchild > 2) {
    
    # restrict child dataframe to those of which there are at least two children, cannot twin if there isn't a second child
    NumAddtionalKids <- numchild - 2
    
    MinChildrenRenamedAge <- min(childrenRenamed$ChildAge)
    MaxChildrenRenamedAge <- max(childrenRenamed$ChildAge)
    
    AgesForTwins <- childrenRenamed %>%
      group_by(ChildAge) %>%
      summarise(NumAge = n()) %>%
      filter(NumAge > 1)
    
    
    # minTwinAge <- min(AgesForTwins) + (maxdiff - 1)
    # maxTwinAge <- max(AgesForTwins) - (maxdiff - 1)
    # # 
    # cat("Min twin age is", minTwinAge, "max twin age is", maxTwinAge, "\n")
    # 
    # AgesForTwins <- AgesForTwins %>%
    #   filter(between(ChildAge, minTwinAge, maxTwinAge))
    # select twin ages
    
    for (i in 1:ExpctNumHH) {
      
      # cat("Number of kids is", sum(AgesForTwins$NumAge), "\n")
      
      # get age, need to select randomly
      
      # TODO loop to make sure that twin age is reasonable given number of kids needed
      
      TwinAgeSelect <- AgesForTwins %>%
        slice_sample(n=1, weight_by = NumAge)
      
      CurrentAge <- TwinAgeSelect$ChildAge
      
      # cat("Current age is", CurrentAge, "\n")
      
      # pull children from the child dataframe who are that age
      SelectedKids <- childrenRenamed %>%
        filter(ChildAge==CurrentAge) %>%
        slice_sample(n=2, replace = FALSE) %>%
        mutate(internalHHID = currentHHID)
      # get parent
      NeededMin <- minparage + CurrentAge
      NeededMax <- maxparage + CurrentAge
      
      SelectedParent <- parentsRenamed %>%
        filter(between(ParentAge, NeededMin, NeededMax)) %>%
        slice_sample(n=1) %>%
        mutate(internalHHID = currentHHID)
      
      # ParentExists <- nrow(SelectedParent)
      # print(ParentExists)
      
      # find the kids that also need to be added
      
      
      # find another n = NumAdditionalKids children
      
      childrenRenamed <- childrenRenamed %>%
        filter(!(ChildID %in% c(SelectedKids$ChildID)))
      
      # filter out children same age, plus children too old or young to be added
      
      AddChildMinAge <- max(0, CurrentAge-maxdiff)
      AddChildMaxAge <- CurrentAge+maxdiff
      
      # shortlist based on the age of the twins
      AgesForExtraChildren <- childrenRenamed %>%
        group_by(ChildAge) %>%
        filter(!(ChildAge==CurrentAge),
               between(ChildAge, AddChildMinAge, AddChildMaxAge)) 
      
      # short list based on the age of the parent
      # need the possible child age range for the selected parent
      # looks weird, but the smallest number is when you deduct the largest age gap
      # and the largest number is when you deduct the smallest age gap
      MinChildAgeToAdd <- SelectedParent$ParentAge - maxparage
      MaxChildAgeToAdd <- SelectedParent$ParentAge - minparage
      
      # cat("Parent age is", SelectedParent$ParentAge, "min child age that can be added is", MinChildAgeToAdd, 
      #     "and maximum child age is", MaxChildAgeToAdd, "\n")
      
      AgesForExtraChildren <- AgesForExtraChildren %>%
        filter(between(ChildAge, MinChildAgeToAdd, MaxChildAgeToAdd)) %>%
        group_by(ChildAge) %>%
        summarise(NumAge = n())
      # sample NumAddtionalKids rows
      
      NumExtraAges <- nrow(AgesForExtraChildren)
      
      # cat("Num extra ages is", NumExtraAges, "num additional kids is", NumAddtionalKids, "\n")
      
      if(NumExtraAges >= NumAddtionalKids) {
        
        # cat("Entered loop \n")
        
        ChildAgesChosen <- AgesForExtraChildren %>%
          slice_sample(n= NumAddtionalKids, replace = FALSE)
        
        # randomly select a child of each age in the ChildAgesChosen data frame
        for(m in 1:NumAddtionalKids) {
          
          NewChildToAdd <- ChildAgesChosen[m,]
          NewChildAge <- NewChildToAdd$ChildAge
          
          NewChild <- childrenRenamed %>%
            filter(ChildAge == NewChildAge) %>%
            slice_sample(n=1) %>%
            mutate(internalHHID = currentHHID)
          
          
          if(exists("DataframeAddedKids")) {
            
            DataframeAddedKids <- bind_rows(DataframeAddedKids, NewChild)
            
            # closes if(exists("DataframeAddedKids")) {
          } else {
            
            DataframeAddedKids <- NewChild
            
            # closes else to if(exists("DataframeAddedKids")) {
          }
          
          # closes for(m in 1:NumAddtionalKids) {
        }
        
        # cat("Number of children is", nrow(ChildAgesChosen), "\n")
        
        # randomly select the additional children
        
        
        # closes loop for if(NumExtraAges >= NumAddtionalKids) {
        
        SelectedKids <- bind_rows(SelectedKids, DataframeAddedKids)
        
        rm(DataframeAddedKids)
        
        # add these to a file that will continue to be appended
        # make sure that the household ID is added.
        # separate the parent and child files at this point as the column names are different
        
        if(exists("ChildAgeMatch")) {
          
          ChildAgeMatch <- bind_rows(ChildAgeMatch, SelectedKids)
          ParentAgeMatch <- bind_rows(ParentAgeMatch, SelectedParent)
          currentHHID <- currentHHID + 1
        } else {
          
          ChildAgeMatch <- SelectedKids
          ParentAgeMatch <- SelectedParent
          currentHHID <- currentHHID + 1
          
        }
        
        # remove the children from the available children and update the counts to enter into the loop
        childrenRenamed <- childrenRenamed %>%
          filter(!(ChildID %in% c(SelectedKids$ChildID)))
        
        AgesForTwins <- childrenRenamed %>%
          group_by(ChildAge) %>%
          summarise(NumAge = n()) %>%
          filter(NumAge > 1)
        
        minTwinAge <- min(AgesForTwins) + (maxdiff - 1)
        maxTwinAge <- max(AgesForTwins) - (maxdiff - 1)
        # 
        # cat("Min twin age is", minTwinAge, "max twin age is", maxTwinAge, "\n")
        # 
        # AgesForTwins <- AgesForTwins %>%
        #   filter(between(ChildAge, minTwinAge, maxTwinAge))
        
        # remove the parent
        
        parentsRenamed <- parentsRenamed %>%
          filter(!(ParentID %in% c(SelectedParent$ParentID)))
        
        
      } else {
        
        # NOT ENOUGH KIDS TO ADD, MUST REJECT THE TWIN SELECTION
        # TODO if this becomes a problem, but twins are matched first
        # not sure if this is going to work
        
        cat("Twins have no matching siblings \n")
        
        # remove problem twin age 
        AgesForTwins <- childrenRenamed %>%
          filter(ChildAge == CurrentAge)
        group_by(ChildAge) %>%
          summarise(NumAge = n()) %>%
          filter(NumAge > 1)
        
        # add selected children back into the childrenRenamed dataframe
        # who were removed 
        SelectedKids <- SelectedKids %>%
          select(-internalHHID)
        
        childrenRenamed <- bind_rows(childrenRenamed, SelectedKids)
        
        # closes else to for if(NumExtraAges >= NumAddtionalKids) {
      }
      
      # have to add the additional twin stuff in here, need to check if children needed > 0
      
      # closes for (i in 1:ExpctNumHH) {
    }
    
    # closes if (ExpctNumHH > 0 & numchild > 2) {
  }
  
  # cat("Twin families finished \n")
  
  
  #####################################
  #####################################
  # Create the non-twin families
  #####################################
  #####################################
  # work through the child dataset as these are the ones that need to be used
  
  # may not be a modulo, so check before each iteration
  # just need to see that the number of people available in the child dataset is >= number needed
   
    while(nrow(childrenRenamed) >= numchild & nrow(parentsRenamed) > 0) {
      
      # print(nrow(childrenRenamed))
      # grab a child randomly
      SelectedFirstChild <- childrenRenamed %>%
        slice_sample(n=1, replace = FALSE) 
      
      CurrentAge <- SelectedFirstChild$ChildAge
      
      # cat("Current age is", CurrentAge, "and current ID is", SelectedFirstChild$ChildID, "\n")
      # get parent
      NeededMin <- minparage + CurrentAge
      NeededMax <- maxparage + CurrentAge
      SelectedParent <- parentsRenamed %>%
        filter(between(ParentAge, NeededMin, NeededMax)) %>%
        slice_sample(n=1) 
      
      # print(nrow(SelectedParent))
      
      # only do matching if nrow(SelectedParent) > 0
      # if it is zero, no parent selected
      
      # cat("Fell over at SelectedParent > 0", "\n")
      
      if(nrow(SelectedParent) > 0) {
        
        # get kids by creating a dataframe of ages available
        
        # cat("Fell over at Min and Max creation", "\n")
        MinChildAge <- SelectedParent$ParentAge - maxparage
        MaxChildAge <- SelectedParent$ParentAge - minparage
        # 
        # cat("MinChildAge is", MinChildAge, "and MaxChildAge is", MaxChildAge, "\n")
        
        # cat("Fell over at the filter stage", "\n")
        
        SelectedNextChildAges <- childrenRenamed %>%
          filter(!(ChildID == SelectedFirstChild$ChildID),
                 !(ChildAge == SelectedFirstChild$ChildAge),
                 between(ChildAge, MinChildAge, MaxChildAge)) %>%
          group_by(ChildAge) %>%
          summarise(NumAge = n()) %>%
          filter(NumAge > 0)
        
        # test if there needs to be a distribution restriction on the child ages
        # NULL is the default, no constraints
        
        if(is.null(maxdiff)) {
          
          #####################################
          # only sample if nrow(SelectedNextChildAges) has at least as many rows as numchild-1
          # otherwise there aren't enough kids to sample
          # sample without replacement
          #####################################
          
          if(nrow(SelectedNextChildAges) >= (numchild - 1)) {
            
            # remove selected child from children dataframe
            childrenRenamed <- childrenRenamed %>%
              filter(!(ChildID %in% c(SelectedFirstChild$ChildID)))
            
            
            # add ID to parent
            SelectedParent <- SelectedParent %>%
              mutate(internalHHID = currentHHID)
            
            # just sample ages with no restriction
            SampledChildAges <- SelectedNextChildAges %>%
              slice_sample(n=(numchild-1), weight_by = NumAge, replace = FALSE)
            
            # get child
            
            for(l in 1:nrow(SampledChildAges)) {
              
              SampledAgeRow <- SampledChildAges[l,]
              
              SampledAge <- SampledAgeRow$ChildAge
              
              SelectedKids <- childrenRenamed %>%
                filter(ChildAge == SampledAge) %>%
                slice_sample(n=1) %>%
                mutate(internalHHID = currentHHID)
              
              # remove from childrenRenamed
              
              childrenRenamed <- childrenRenamed %>%
                filter(!(ChildID %in% c(SelectedKids$ChildID)))
              
              # create an interim dataset of children to append to working dataset
              
              if(exists("ChildAgeMatch")) {
                
                ChildAgeMatch <- bind_rows(ChildAgeMatch, SelectedKids)
                ParentAgeMatch <- bind_rows(ParentAgeMatch, SelectedParent)
                
                # childrenRenamed <- childrenRenamed %>%
                #   filter(!(ChildID %in% c(SelectedKids$ChildID)))
                
                parentsRenamed <- parentsRenamed %>%
                  filter(!(ParentID %in% c(SelectedParent$ParentID)))
                
                # closes if(exists("ChildAgeMatch")) {
              } else {
                
                ChildAgeMatch <- SelectedKids
                ParentAgeMatch <- SelectedParent
                
                # childrenRenamed <- childrenRenamed %>%
                #   filter(!(ChildID %in% c(SelectedKids$ChildID)))
                
                parentsRenamed <- parentsRenamed %>%
                  filter(!(ParentID %in% c(SelectedParent$ParentID)))
                
                # closes else to if(exists("ChildAgeMatch")) {
              }
              
              # closes for(l in 1:nrow(SampledChildAges)) {
            }
            # bind first selected child
            # append first child onto the selected kids
            
            SelectedFirstChild <- SelectedFirstChild %>%
              mutate(internalHHID = currentHHID)
            
            ChildAgeMatch <- bind_rows(ChildAgeMatch, SelectedFirstChild)
            
            currentHHID <- currentHHID + 1
            
            
            
            # closes if(nrow(SelectedNextChildAges) > numchild-1) {
          } else {
            
            #####################################
            # need to remove unmatchable child from the childrenRenamed data frame
            #####################################
            
            childrenRenamed <- childrenRenamed %>%
              filter(!(ChildID %in% c(SelectedFirstChild$ChildID)))
            
            # closes else to if(nrow(SelectedNextChildAges) > numchild-1) {
          }
          
          
          
          # closes if(isNULL(maxdiff)) {
        } else {
          
          #####################################
          # used if there is a standard deviation for the child ages
          # only sample if nrow(SelectedNextChildAges) has at least as many rows as numchild-1
          # otherwise there aren't enough kids to sample
          # sample without replacement
          #####################################
          
          # further filter the selected children based on the maxdiff value
          
          # limit extra children to those permitted by the maxdiff value
          NeededMinExtra <- CurrentAge - maxdiff
          NeededMaxExtra <- CurrentAge + maxdiff
          
          SelectedNextChildAges <- SelectedNextChildAges %>%
            filter(between(ChildAge, NeededMinExtra, NeededMaxExtra))
          
          if(nrow(SelectedNextChildAges) >= (numchild - 1)) {
            
            # remove selected child from children dataframe
            childrenRenamed <- childrenRenamed %>%
              filter(!(ChildID %in% c(SelectedFirstChild$ChildID)))
            
            
            # add ID to parent
            SelectedParent <- SelectedParent %>%
              mutate(internalHHID = currentHHID)
            
            # just sample ages with no restriction
            SampledChildAges <- SelectedNextChildAges %>%
              slice_sample(n=(numchild-1), weight_by = NumAge, replace = FALSE)
            
            # get child
            
            for(l in 1:nrow(SampledChildAges)) {
              
              SampledAgeRow <- SampledChildAges[l,]
              
              SampledAge <- SampledAgeRow$ChildAge
              
              SelectedKids <- childrenRenamed %>%
                filter(ChildAge == SampledAge) %>%
                slice_sample(n=1) %>%
                mutate(internalHHID = currentHHID)
              
              # remove from childrenRenamed
              
              childrenRenamed <- childrenRenamed %>%
                filter(!(ChildID %in% c(SelectedKids$ChildID)))
              
              # create an interim dataset of children to append to working dataset
              
              if(exists("ChildAgeMatch")) {
                
                ChildAgeMatch <- bind_rows(ChildAgeMatch, SelectedKids)
                ParentAgeMatch <- bind_rows(ParentAgeMatch, SelectedParent)
                
                # childrenRenamed <- childrenRenamed %>%
                #   filter(!(ChildID %in% c(SelectedKids$ChildID)))
                
                parentsRenamed <- parentsRenamed %>%
                  filter(!(ParentID %in% c(SelectedParent$ParentID)))
                
                # closes if(exists("ChildAgeMatch")) {
              } else {
                
                ChildAgeMatch <- SelectedKids
                ParentAgeMatch <- SelectedParent
                
                # childrenRenamed <- childrenRenamed %>%
                #   filter(!(ChildID %in% c(SelectedKids$ChildID)))
                
                parentsRenamed <- parentsRenamed %>%
                  filter(!(ParentID %in% c(SelectedParent$ParentID)))
                
                # closes else to if(exists("ChildAgeMatch")) {
              }
              
              # closes for(l in 1:nrow(SampledChildAges)) {
            }
            
            # bind first selected child
            # append first child onto the selected kids
            
            SelectedFirstChild <- SelectedFirstChild %>%
              mutate(internalHHID = currentHHID)
            
            ChildAgeMatch <- bind_rows(ChildAgeMatch, SelectedFirstChild)
            
            currentHHID <- currentHHID + 1
            
            
            
            # closes if(nrow(SelectedNextChildAges) > numchild-1) {
          } else {
            
            #####################################
            # need to remove unmatchable child from the childrenRenamed data frame
            
            
            childrenRenamed <- childrenRenamed %>%
              filter(!(ChildID %in% c(SelectedFirstChild$ChildID)))
            
            # closes else to if(nrow(SelectedNextChildAges) > numchild-1) {
          }
          
          
          
          
          # closes else to if(is.null(maxdiff)) {
        }
        
        # closes if(nrow(SelectedParent) > 0)
  
    } else {
      # need an else in here
      # remove all ages of that child in the data frame
      # alternatively, remove child and see if that fixes it
      childrenRenamed <- childrenRenamed %>%
        filter(!(ChildID %in% c(SelectedFirstChild$ChildID)))
      
    }
    
    # closes if(nrow(childrenRenamed) >= numchild)
  }
  
  #####################################
  #####################################
  # Create the output data frames
  #####################################
  #####################################
  
  
  FullMatchedChld <- ChildAgeMatch %>%
    rename(!! chidcolName := ChildID,
           !! chagecolName := ChildAge,
           {{HHNumVar}} := internalHHID)
  
  # cat("The FullMatchedChld dataset info is", "\n")
  # str(FullMatchedChld)
  
  FullMatchedPrnt <- ParentAgeMatch %>%
    rename(!! paridcolName := ParentID,
           !! paragecolName := ParentAge,
           {{HHNumVar}} := internalHHID) %>%
    unique()
  
  # cat("The FullMatchedPrnt dataset info is", "\n")
  # str(FullMatchedPrnt)
  
  OutputDataframe <- rbind(FullMatchedChld, FullMatchedPrnt)
  
  # print(NumAttempts)
  
  cat("The individual dataframes are $Matched, $Children, and $Adults", "\n")
  cat("$Children contains unmatched observations from the children data frame", "\n")
  cat("$Adults contains unmatched observations from the parent data frame", "\n")
  
  MatchedIDs <- OutputDataframe %>%
    pull({{paridcolName}})
  
  noParents <- children %>%
    filter(!({{chidcolName}} %in% MatchedIDs))
  
  noKids <- parents %>%
    filter(!({{paridcolName}} %in% MatchedIDs))
  MergedList <- list()
  
  MergedList$Matched <- OutputDataframe
  MergedList$Children <- noParents
  MergedList$Adults <- noKids
  
  return(MergedList)
  # closes function
}