#


#' Create a subset of observations containing only opposite-sex couples
#'
#' This function creates a data frame of couples, based on a population distribution of age differences. The distribution used is the skew normal.
#' Two data frames are required. The recipient data frame contains the age data, of one sex, to which the distribution will be applied. The donor data frame contains the age data, of the other sex, from which the age counts to match are constructed. If the two data frames are different lengths, the recipient data frame must be the shortest of the two. In this situation, a random subsample of the donor data frame will be used.
#' The network size variable is the number of people in that person's networks, including the person. Thus, a person with a network size of 0 has no social contacts. A person with a network size of 1 has one social contact, and so forth.
#'
#' Only a standard deviation is required, as the ages are centred around 0. The age of the person on a vertex is the age against which linked vertices are tested. A larger standard deviation enables the construction of networks with larger age differences. With triads, the age test is conducted comparing one person in the triad with the other two people. Thus, friends of friends will tend to be a similar age to friends.
#' The function attempts to ensure that no people in the same household will be placed inside the same network. However, this may be difficult to achieve given the number of networks. The number of iterations specified by the user ensures that the function will not enter an endless loop.
#'
#'
#' The function performs a reasonableness check for the first five variables. If any other parameters are missing, the usual error messages from the imported functions will be output.
#'
#' If desired, this can be used to construct same-sex couples.
#'
#' @export
#' @param people A data frame containing people to be matched to each other using social networks.
#' @param idcol The column number for each person's unique ID.
#' @param agecol The column number for the age variable.
#' @param hhidcol The column number for each person's household.
#' @param netsizecol The column number for the network size.
#' @param sdused The standard deviation for the age differences between two people on an edge.
#' @param probsame The probability that a friend of a friend is an edge. For example, if A and B and friends, and B and C are friends, this is the probability that C is also a friend of A.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param NumIterations The maximum number of iterations used to construct the coupled data frame. This has a default value of 1000000, and is the stopping rule if the algorithm does not converge.
#'
#' @return A data frame of an even number of observations that have been allocated into opposite-sex couples.
#'
#' @examples
#' Recipients <- data.frame(cbind(PersonID = c(1:1000),
#'                               PersonAge = c(round(runif(200, min=18, max=23),0), round(runif(300, min=24, max=50),0), round(runif(500, min=51, max=90),0))))
#'
#' Donors <- data.frame(cbind(PersonID = c(2001:4000),
#'                               PersonAge = c(round(runif(400, min=18, max=23),0), round(runif(500, min=24, max=50),0), round(runif(1100, min=51, max=90),0))))
#'
#' ExampleOutput <- OppositeSex(Recipients, Recipientidcol=1, Recipientagecol=2, Donors, Donoridcol=1, Donoragecol=2, DirectXi=-2, DirectOmega=4,
#'                               AlphaUsed=5, userseed=NULL, pValueToStop=.001, NumIterations=1000, IDStartValue = 10001, HouseholdNumVariable="TheHouseholds")

socnet <- function(people, idcol, agecol, hhidcol, netsizecol, sdused=0, probsame = .5, #NetworkVariable = NULL,
                   userseed=NULL,
                   #pValueToStop=NULL,
                   NumIterations=1000000) {

  options(dplyr.summarise.inform=F)

  if(is.null(idcol)) {
    stop("The ID column number must be supplied.")
  }

  if (!is.numeric(agecol)) {
    stop("The age column number must be supplied.")
  }

  if (!is.numeric(netsizecol)) {
    stop("The network size column number must be supplied.")
  }

  # if(is.null(NetworkVariable)) {
  #   stop("A name for the network variable must be supplied.")
  # }

  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  WorkingDataFrame <- people %>%
    rename(ID = !! idcol, Age = !! agecol,
           #Household = !! hhidcol,
           Network = !! netsizecol) %>%
    select(ID, Age,
           #Household,
           Network)

  # check that we don't end up with one person needing a match and none to spare

  # put seed in here as first randomness occurs below
  if (!is.null(userseed)) {
    set.seed(userseed)
  }


  if (!(sum(WorkingDataFrame$Network) %% 2 == 0) == TRUE){
    cat("The number of network links must be a factor of 2.", "\n")

    PersonToAdd1To <- WorkingDataFrame %>%
      slice_sample(n = 1)

    PersonToAdd1To <- PersonToAdd1To %>%
      mutate(Network = Network + 1)

    cat("Person", PersonToAdd1To$ID, "now has", PersonToAdd1To$Network, "network connections instead of", (PersonToAdd1To$Network)-1, "\n")

    WorkingDataFrame <- WorkingDataFrame %>%
      filter(!(ID==PersonToAdd1To$ID))

    WorkingDataFrame <- bind_rows(WorkingDataFrame, PersonToAdd1To)
  }
  people <- data.frame(ID = 1:20, Age = round(rnorm(20, mean=40, sd=10)))


  # construct a graph with this degree distribution
  WiredNetwork <- igraph::sample_degseq(out.deg = WorkingDataFrame$Network,
                                        method="simple.no.multiple")


  cat("Wirednetwork created", "\n")
  # now, get it so that the clustering is better
  current_clustering <- igraph::transitivity(WiredNetwork)

  target_clustering <- 0.5 # P(triads | length 3 path)


  # https://github.com/cwatson/brainGraph/pull/26
  ClusteredNetwork <- brainGraph::sim.rand.graph.clust(WiredNetwork,
                                                       rewire.iters=0,
                                                       cl=target_clustering,
                                                       max.iters=100)

  igraph::transitivity(ClusteredNetwork)

  # OK, now we want to get our ages on this. One way is to just
  # place them directly, but let's do it slightly indirectly so
  # that we can use it later. We start with a map from nodes to
  # people:
  node_to_people <- 1:nrow(WorkingDataFrame)
  theages <- WorkingDataFrame$Age
  # thehouseholds <- WorkingDataFrame$Network


  # now we can put the ages on the network and plot it:
  # can add more than one attribute, see examples here
  # https://rdrr.io/cran/igraph/src/R/attributes.R

  cat("First reference to network_clustered", "\n")

  ClusteredNetwork %>%
    igraph::set_vertex_attr("Age", value=theages[node_to_people]) #%>%
  #  igraph::set_vertex_attr("Household", value=thehouseholds[node_to_people])

  # cat("Plot is below", "\n")
  # plot()

  # network_clustered %>%
  #   igraph::set_vertex_attr("Age", value=theages[node_to_people]) # %>%
  #   # igraph::set_vertex_attr("Household", value=thehouseholds[node_to_people])
  # plot()

  # now, it'd be nice to work out the age differences as well
  # we do that by grabbing the edges in the graph as a matrix
  # of indices (which we'll then pass through node_to_people
  # to get to the people that each node corresponds to)

  cat("Now getting the edgelist", "\n")

  edges = igraph::get.edgelist(ClusteredNetwork)

  # helper to get the age differences: The idea is that
  # we use the edges matrix (which has the vertex index of
  # each edge) and then pass that through node_to_people
  # to map to the corresponding row in the people data frame
  # and then grab the age out of that. We then compute
  # the difference

  cat("Get the age difference function", "\n")

  get_age_diff <- function(edges, node_to_people, Age) {
    # use the edges and node_to_people to lookup ages
    age1 = Age[node_to_people[edges[,1]]]
    age2 = Age[node_to_people[edges[,2]]]
    age1 - age2
  }

  # and they can't be in the same household
  # get_HHNum_diff <- function(edges, node_to_people, Household) {
  #   Household1 = Household[node_to_people[edges[,1]]]
  #   Household2 = Household[node_to_people[edges[,2]]]
  #   Household1 - Household2
  # }

  cat("get the age differences", "\n")

  age_diff <- get_age_diff(edges, node_to_people, theages)

  cat("Second reference to network_clustered", "\n")
  # we could now plot that on the edges:
  # network_clustered %>%
  #   set_vertex_attr("label", value=ages[node_to_people]) %>%
  #   set_edge_attr("label", value=age_diff) %>%
  #   plot()

  ClusteredNetwork %>%
    igraph::set_vertex_attr("label", value=theages[node_to_people]) %>%
    igraph::set_edge_attr("label", value=age_diff) %>%
    plot()

  # right, now that we have the age difference, we could
  # optimise this through swapping. Assuming we want a
  # normal age difference, then optimising the sum of
  # squares is probably what we want:

  # we're starting with:
  ss <- sum(age_diff^2)
  ss

  cat("ss created", "\n")
  # now, we're going to swap which node represents which person
  # by shuffling the node_to_people vector.

  # I'm not sure the best way to do this. One thing we could try
  # is just use shuffle() to randomly permute it and then compare.
  # but presumably to optimise you'll want to move slowly around
  # the permutation space?

  # But, not too slowly? So what I'm doing here is going to pull
  # out a random number from 2 onwards as the number of nodes to
  # swap around, and then swap them around by shifting indices.

  # TODO: A better strategy (as it seems that it improves a bit early
  # on, then not at all) might be to run an outer loop that does
  # a bunch of big shuffles as starting points, then the inner
  # loop that optimises within - hopefully this might better cover
  # the space? (The space is BIG ofcourse)

  # helper function for shifting a vector (one of many ways to
  # permute one). i.e. shift_vector(c(1,2,3,4),1) = c(2,3,4,1)
  shift_vector <- function(x, n = 1) {
    if (n == 0) x else c(tail(x, -n), head(x, n))
  }

  cat("Shift vector created", "\n")

  accept <- list() # this is just to store how often we accept a proposal
  for (i in 1:10000) { # lots of iterations

    # do the permutation: first off, how many people do we permute?
    num_to_permute <- 2 + rpois(1, 0.5)
    # how much should we shift them around?
    shift <- sample(num_to_permute-1, 1)
    # now pull out this number of people at random
    swap <- sample(length(node_to_people), num_to_permute)
    # and swap them over
    proposed <- node_to_people
    proposed[swap] <- proposed[shift_vector(swap, shift)]

    # compute the age difference for this proposal
    age_diff_prop <- get_age_diff(edges, proposed, ages)
    # very crude here - just optimising sum of squares
    ss_prop <- sum(age_diff_prop^2)
    if (ss_prop < ss) {
      # swap!
      node_to_people <- proposed
      age_diff <- age_diff_prop
      ss <- ss_prop
      accept[[length(accept) + 1]] <- i
    } else {
      # no swap
    }

    print(i)
  }
  unlist(accept)
  ss

  cat("Third reference to network_clustered", "\n")

  # plot with edge weights with differences and ages on nodes
  network_clustered %>%
    set_edge_attr("label", value=age_diff) %>%
    set_vertex_attr("label", value=ages[node_to_people]) %>%
    plot()

  # TODO: from here you can dump out the data to whatever format you like.
  # key thing to note is that node_to_people is the map from vertices
  # on the network to people in your data set



  return(OutputDataframe)
}











# Networks <- function(People, idcol=NULL, agecol=NULL, netsizecol=NULL, MeanUsed=NULL, sdused=NULL, probsame = .5,
#                      NetworkVariable = NULL, userseed=NULL, pValueToStop=NULL, NumIterations=1000000) {
#
#
#   if(is.null(idcol)) {
#     stop("The ID column number must be supplied.")
#   }
#
#   if (!is.numeric(agecol)) {
#     stop("The age column number must be supplied.")
#   }
#
#   if (!is.numeric(netsizecol)) {
#     stop("The network size column number must be supplied.")
#   }
#
#   if(is.null(NetworkVariable)) {
#     stop("A name for the network variable must be supplied.")
#   }
#
#   #################################################
#   # end data check
#   ################################################
#
#   # construct start value for networkID
#
#   #NetworkIDCounter <- 1
#
#   # subset by number of people to match
#   # move from smallest contact network to largest
#   # otherwise may run out of reasonable age pairs for the small numbers
#
#   # find smallest and largest contact sizes
#
#   WorkingDataFrame <- People %>%
#     rename(ID = !! idcol, Age = !! agecol, Network = !! netsizecol) %>%
#     filter(Network > 0)
#
#   if(!(sum(WorkingDataFrame$Network) %% 2 == 0) == TRUE) {
#     stop("The network sizes must sum to a factor of 2.")
#   }
#
#   NetworkSizeCounts <- WorkingDataFrame %>%
#     group_by(Network) %>%
#     summarise(CountofSize = n())
#
#   MinimumSize <- min(NetworkSizeCounts$Network)
#   MaximumSize <- max(NetworkSizeCounts$Network)
#
#   # cat("Minimum network size is", MinimumSize, "and maximum network size is", MaximumSize, "\n")
#
#   # construct empty data frame to hold the matches
#   # one network per row
#   # initialise so that the number of columns is the maximum size of people in a network +1
#   ColCountNeeded <- MaximumSize + 1
#
#   OutputDataFrame <- setNames(data.frame(matrix(ncol = ColCountNeeded, nrow = 0)),
#                               paste0("Person", c(1:ColCountNeeded)))
#
#   # construct column to use for column binding to each person's output data frame
#  BindingColumn <- data.frame(PersonNumbers = paste0("Person", c(1:ColCountNeeded)))
#
#   # work through the data frame, as people are extracted they will be given contacts
#   # and once the contacts are added, they are removed from the data frame
#   # will randomly draw
#
#   # put seed in before start of first loop
#   if (!is.null(userseed)) {
#     set.seed(userseed)
#   }
#
#   for(i in 1:1) {
#  # while(!(is.na(WorkingDataFrame$ID[1])) == TRUE) {
#
#     SelectedPerson <- WorkingDataFrame %>%
#       slice_sample(n=1)
#
#     DataframeOfMatches <- SelectedPerson
#
#     NetworkSizeForSelected <- SelectedPerson$Network
#
#     cat("Network size is", NetworkSizeForSelected, "\n")
#
#     # remove this person from the working data frame
#     # this also prevents them from being selected to match against themselves
#     # that would be bad
#
#     WorkingDataFrame <- WorkingDataFrame %>%
#       filter(!ID==SelectedPerson$ID)
#
#   #  cat("The working data frame is now", nrow(WorkingDataFrame), "rows long.", "\n")
#
#     # for each person in their network size, redo the age difference so that we don't get
#     # everyone aged the same as their network contacts
#
#     # cat(str(WorkingDataFrame))
#
#     for(i in 1:NetworkSizeForSelected) {
#
#       AgeDiffNeeded <- rnorm(1, MeanUsed, sdused)
#       AgeNeeded <- round(SelectedPerson$Age + AgeDiffNeeded)
#
#       # test that random number is working correctly and a different one is drawn each time
#       # cat("Age difference is", AgeDiffNeeded, "so Age needed is", AgeNeeded, "\n")
#
#       OperativeDataFrame <- WorkingDataFrame %>%
#         filter(Age==AgeNeeded)
#
#     # cat("It got to here", "\n")
#
#       # loop for extracting people if OperativeDataFrame is empty
#       # which will occur if there are no people of the required age
#       # principle here is to widen the age range that is capable of being selected
#
#       if(!(is.na(OperativeDataFrame$Age[1])) == TRUE) {
#
#         RandomlySelectedMatch <- OperativeDataFrame %>%
#           slice_sample(n = 1)
#
#       } else {
#
#       n = 0
#
#       while(!(is.na(OperativeDataFrame$Age[1])) == TRUE) {
#
#         n = n + 1
#
#         AgeRangeMin <- AgeNeeded - n
#         AgeRangeMax <- AgeNeeded + n
#
#         # stop the age ranges going beyond the data values
#
#         if(AgeRangeMin > min(WorkingDataFrame$Age)) {
#           AgeRangeMin <- min(WorkingDataFrame$Age)
#         }
#         if(AgeRangeMax < max(WorkingDataFrame$Age)) {
#         AgeRangeMax <- max(WorkingDataFrame$Age)
#         }
#
#         cat("No age match made, new age range is", AgeRangeMin, "to", AgeRangeMax, "\n")
#
#         # filter also removes people already selected as a match
#         OperativeDataFrame <- WorkingDataFrame %>%
#           filter(between(Age, AgeRangeMin,AgeRangeMax), !(ID %in% OperativeDataFrame$ID))
#
#         # closes loop for widening the age range if no match after widening age search
#       }
#
#       RandomlySelectedMatch <- OperativeDataFrame %>%
#         slice_sample(n = 1)
#
#         # closes while loop for getting a match
#       }
#
#       # put selected person into a data frame that will contain all matches
#
#       if(exists("DataframeOfMatches")) {
#
#         DataframeOfMatches <- bind_rows(DataframeOfMatches, RandomlySelectedMatch)
#         WorkingDataFrame <- WorkingDataFrame %>%
#           filter(!ID == RandomlySelectedMatch$ID)
#
#         # process below needs to be done separately for each data frame
#         # 1. add a column variable that aligns with the existing data frame colnames
#         # 2. remove everything but the IDs
#         # 3. convert from long to wide
#         # 4. paste into the empty data frame
#
#
#       } else {
#
#         DataframeOfMatches <- RandomlySelectedMatch
#
#         WorkingDataFrame <- WorkingDataFrame %>%
#           filter(!ID == RandomlySelectedMatch$ID)
#
#       # constructs the data frame of matches for the current person
#       }
#
#
#     #   # closes for loop for selecting all the people into their network size
#     }
#
#     # shape the main dataframe into wide
#     # process is: 1. construct NAs for each person that doesn't exist in this person's network
#     # 2. remove everything but the IDs
#     #  construct the rownames so that they align with the existing data frame colnames
#     # 3. convert from long to wide (uses base R transpose)
#     # 4. paste the wide single-row data into the output data frame
#
#     # make the dataframe the same number of rows for merging
#     NumberRowsToCreate <- ColCountNeeded - nrow(DataframeOfMatches)
#
#     # # cat("The number of NA rows to construct is", NumberRowsToCreate, "\n")
#
#     MissingRowsToAdd <- data.frame(ID = rep(NA, NumberRowsToCreate))
#
#     FriendsList <- DataframeOfMatches %>%
#       slice_tail(n=nrow(DataframeOfMatches)-1)
#
#     DataframeOfMatches <- DataframeOfMatches %>%
#       dplyr::select("ID")  %>%
#       bind_rows(MissingRowsToAdd)
#
#    rownames(DataframeOfMatches) <- BindingColumn[,1]
#
#    WideDataFrame <- as.data.frame(t(DataframeOfMatches))
#
#    OutputDataframe <- OutputDataFrame %>%
#      bind_rows(WideDataFrame)
#
#    # delete DataframeOfMatches
#
#    rm(DataframeOfMatches)
#
#    # now work on the friends
#
#    # decrease network count by 1
#    # remove anyone with a contact count of 0
#    # only need ID, age, and number of contacts
#
#    FriendsList <- FriendsList %>%
#      mutate(Network = Network-1) %>%
#      filter(Network > 0) %>%
#      select(ID, Age, Network)
#
#    # these friends are all in a contact network
#    # so we have to find their friends
#    # this requires looping through that FriendsList data frame
#
#    for(j in 1:nrow(FriendsList)) {
#
#      CurrentPerson <- FriendsList[j,]
#
#      NetworkSizeForSelected <- CurrentPerson$Network
#
#    #  cat("Current person is", CurrentPerson$ID, "and starting number of contacts is", CurrentPerson$Network , "\n")
#
#      # get random matches for this friend
#
#      for(i in 1:NetworkSizeForSelected) {
#
#        AgeDiffNeeded <- rnorm(1, MeanUsed, sdused)
#        AgeNeeded <- round(SelectedPerson$Age + AgeDiffNeeded)
#
#        # test that random number is working correctly and a different one is drawn each time
#        # cat("Age difference is", AgeDiffNeeded, "so Age needed is", AgeNeeded, "\n")
#
#        OperativeDataFrame <- WorkingDataFrame %>%
#          filter(Age==AgeNeeded)
#
#        # cat("It got to here", "\n")
#
#        # loop for extracting people if OperativeDataFrame is empty
#        # which will occur if there are no people of the required age
#        # principle here is to widen the age range that is capable of being selected
#
#        if(!(is.na(OperativeDataFrame$Age[1])) == TRUE) {
#
#          RandomlySelectedMatch <- OperativeDataFrame %>%
#            slice_sample(n = 1) %>%
#            select(ID, Age, Network) %>%
#            mutate(Friend = CurrentPerson$ID)
#
#        } else {
#
#          n = 0
#
#          while(!(is.na(OperativeDataFrame$Age[1])) == TRUE) {
#
#            n = n + 1
#
#            AgeRangeMin <- AgeNeeded - n
#            AgeRangeMax <- AgeNeeded + n
#
#            # stop the age ranges going beyond the data values
#
#            if(AgeRangeMin > min(WorkingDataFrame$Age)) {
#              AgeRangeMin <- min(WorkingDataFrame$Age)
#            }
#            if(AgeRangeMax < max(WorkingDataFrame$Age)) {
#              AgeRangeMax <- max(WorkingDataFrame$Age)
#            }
#
#            cat("No age match made, new age range is", AgeRangeMin, "to", AgeRangeMax, "\n")
#
#            # filter also removes people already selected as a match
#            OperativeDataFrame <- WorkingDataFrame %>%
#              filter(between(Age, AgeRangeMin,AgeRangeMax), !(ID %in% OperativeDataFrame$ID))
#
#            # closes loop for widening the age range if no match after widening age search
#          }
#
#          RandomlySelectedMatch <- OperativeDataFrame %>%
#            slice_sample(n = 1) %>%
#            select(ID, Age, Network) %>%
#            mutate(Friend = CurrentPerson$ID)
#
#          # closes while loop for getting a match
#        }
#
#        # put selected person into a data frame that will contain all matches
#
#        if(exists("DataframeOfMatches")) {
#
#          DataframeOfMatches <- bind_rows(DataframeOfMatches, RandomlySelectedMatch)
#          WorkingDataFrame <- WorkingDataFrame %>%
#            filter(!ID == RandomlySelectedMatch$ID)
#
#          # process below needs to be done separately for each data frame
#          # 1. add a column variable that aligns with the existing data frame colnames
#          # 2. remove everything but the IDs
#          # 3. convert from long to wide
#          # 4. paste into the empty data frame
#
#
#        } else {
#
#          DataframeOfMatches <- RandomlySelectedMatch
#
#          WorkingDataFrame <- WorkingDataFrame %>%
#            filter(!ID == RandomlySelectedMatch$ID)
#
#          # constructs the data frame of matches for the current friend
#        }
#
#
#        #   # closes for loop for selecting all the people into their network size
#      }
#
#      # TODO decrease the number of contacts of friends by 1
#
#      DataframeOfMatches <- DataframeOfMatches %>%
#        mutate(Network = Network - 1) %>%
#        filter(Network > 0)
#
#      MatchesExhausted <- DataframeOfMatches <- DataframeOfMatches %>%
#        mutate(Network = Network - 1) %>%
#        filter(Network == 0)
#
#      # TODO delete the dataframe of matches once each friend is matched
#
#      # TODO check if the friend is a friend of a friend, so long as contact number is > 0
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#      #closes loop for going through friends' list
#    }
#
#
#
#
#
#    #
#    # TODO: decrement the contact number count from the selected people.
#    # if contact == 0, remove from the working data frame
#
#    #
#
#
#     # TODO: check if there is a probability > 0 that a person can be a friend of a friend
#     # do this for each person, but not for checks already made
#     # e.g. if network has A matched to B, C, D, and E, then need to:
#     # 1. get remaining contacts for B (e.g. F, G)
#     # 2. decrement the contact size for F and G by 1 for each. If either now has network size == 0, remove
#     # 3. remove B from list of people who can be matched.
#     # 4. test if either of F or G are friends of C, D, or E (assuming F and G have > 1 friend)
#     # 5. if so, add these to the relevant contact list for C, D, or E
#     # 6. if so, decrement the network size of C, D, and E
#     # 7. and in all of these, C, D, E, F, and G need to be held out as selected people
#     # this will get complicated as need to loop through each match
#     # non-matches can be ignored
#
#
#     #closes while loop for selecting people from the working data frame
#   }
#
#   #
#   return(MatchesExhausted)
#
#     # closes function
# }
