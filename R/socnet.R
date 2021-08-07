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
#' @param netmax A data frame containing the 1-dimensional matrix of network sizes. Must contain only integers and be the same length as the people data frame.
#' @param netsizecol The column number for the network size.
#' @param sdused The standard deviation for the age differences between two people on an edge.
#' @param probsame The probability that a friend of a friend is an edge. For example, if A and B and friends, and B and C are friends, this is the probability that C is also a friend of A.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param NumIterations The maximum number of iterations used to construct the coupled data frame. This has a default value of 1000000, and is the stopping rule if the algorithm does not converge.
#' @param usematrix If an adjacency matrix is output instead of an igraph object. Default is "Y".
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

socnet <- function(people, idcol, agecol, hhidcol, netmax, sdused=0, probsame = .5, userseed=NULL,
                   NumIterations=1000000, usematrix = "Y") {

  options(dplyr.summarise.inform=F)

  if(is.null(idcol)) {
    stop("The ID column number must be supplied.")
  }

  if (!is.numeric(agecol)) {
    stop("The age column number must be supplied.")
  }

  if (!(length(netmax) == nrow(people))) {
    stop("The network matrix must be the same length as the number of rows in the people data frame.")
  }

  if (!(is.integer(netmax))) {
    stop("The network matrix must only contain integers.")
  }


  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  WorkingDataFrame <- people %>%
    rename(ID = !! idcol, Age = !! agecol) %>%
    select(ID, Age)

  # check that we don't end up with one person needing a match and none to spare

  # put seed in here as first randomness occurs below
  if (!is.null(userseed)) {
    set.seed(userseed)
  }


  if (!(sum(netmax) %% 2 == 0) == TRUE){
    cat("The number of network links must be a factor of 2.", "\n")

    # random draw an index position

    IndexToAdd1 <- sample.int(length(netmax), 1)

    OriginalValue <- netmax[IndexToAdd1]

    cat("1 has been added to the network matrix at position", IndexToAdd1, "\n")
    cat("The value has been changed from", OriginalValue, "to", OriginalValue + 1, "\n")

    netmax[IndexToAdd1] <- OriginalValue + 1

  }


  # construct a graph with this degree distribution
  WiredNetwork <- igraph::sample_degseq(out.deg = netmax,
                                        method="simple.no.multiple")


  # cat("Wirednetwork created", "\n")
  # now, get it so that the clustering is better
  current_clustering <- igraph::transitivity(WiredNetwork)

  target_clustering <- probsame # P(triads | length 3 path)


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
  theIDs <- WorkingDataFrame$ID


  # now we can put the ages on the network and plot it:
  # can add more than one attribute, see examples here
  # https://rdrr.io/cran/igraph/src/R/attributes.R

  # cat("First reference to network_clustered", "\n")

  ClusteredNetwork %>%
    igraph::set_vertex_attr("Age", value=theages[node_to_people])

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

  # cat("Now getting the edgelist", "\n")

  edges = igraph::get.edgelist(ClusteredNetwork)

  # helper to get the age differences: The idea is that
  # we use the edges matrix (which has the vertex index of
  # each edge) and then pass that through node_to_people
  # to map to the corresponding row in the people data frame
  # and then grab the age out of that. We then compute
  # the difference

  # cat("Get the age difference function", "\n")

  get_age_diff <- function(edges, node_to_people, Age) {
    # use the edges and node_to_people to lookup ages
    age1 = Age[node_to_people[edges[,1]]]
    age2 = Age[node_to_people[edges[,2]]]
    age1 - age2
  }

  # cat("get the age differences", "\n")

  age_diff <- get_age_diff(edges, node_to_people, theages)

  # cat("Second reference to network_clustered", "\n")
  # we could now plot that on the edges:
  # network_clustered %>%
  #   set_vertex_attr("label", value=ages[node_to_people]) %>%
  #   set_edge_attr("label", value=age_diff) %>%
  #   plot()

  ClusteredNetwork %>%
    igraph::set_vertex_attr("label", value=theages[node_to_people]) %>%
    igraph::set_edge_attr("label", value=age_diff)
    plot(ClusteredNetwork)

  # right, now that we have the age difference, we could
  # optimise this through swapping. Assuming we want a
  # normal age difference, then optimising the sum of
  # squares is probably what we want:

  # we're starting with:
  ss <- sum(age_diff^2)
  # ss

  # cat("ss created", "\n")
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

  # cat("Shift vector created", "\n")

  accept <- list() # this is just to store how often we accept a proposal
  for (i in 1:NumIterations) { # lots of iterations

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
    age_diff_prop <- get_age_diff(edges, proposed, theages)
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

    # print(i)
  }
  unlist(accept)
  # ss

  # cat("Third reference to network_clustered", "\n")

  # plot with edge weights with differences and ages on nodes
  # network_clustered %>%
  #   set_edge_attr("label", value=age_diff) %>%
  #   set_vertex_attr("label", value=theages[node_to_people]) %>%
  #   plot()

  ClusteredNetwork <- ClusteredNetwork %>%
    igraph::set_edge_attr("label", value=age_diff) %>%
     igraph::set_vertex_attr("label", value=theIDs[node_to_people])

  # key thing to note is that node_to_people is the map from vertices
  # on the network to people in your data set

 if (!(usematrix) == "Y") {

   return(ClusteredNetwork)

 } else {

  return(igraph::as_adj(ClusteredNetwork, type = "both", names = TRUE))
 }


}


