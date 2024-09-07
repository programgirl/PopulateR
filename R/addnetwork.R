# Create a social network
#' This function creates social networks between people, based on age differences. A data frame of people with ages is required. These are the people who will have social relationships between each other. A a 1x n matrix of counts must also be supplied, where n is the number of rows in the people data frame. As person-to-person pairs are constructed, the sum of the matrix counts must be even. If it is not, the function will randomly select one count from the matrix and add 1 to it. If this correction happens, an explanation, including the index position of the count, will be printed to the console.
#'
#' A normal distribution is used, using the age differences between the pairs. This is centred on 0, i.e. the people in the pair are the same age. If people B and C are in person A's network, probsame is used to determine the likelihood that people B and C know each other. The larger this proportion, the more likely that people in one person's network know each other, compared to random construction of a network between them.
#' @export
#' @param people A data frame containing people to be matched to each other using social networks.
#' @param pplid The variable for each person's unique ID.
#' @param pplage The variable for each person's age.
#' @param netmax A matrix containing the 1-dimensional matrix of network sizes. Must contain only integers and be the same length as the people data frame.
#' @param sdused The standard deviation for the age differences between two people.
#' @param probsame The probability that a friend of a friend is also a friend. For example, if A and B and friends, and B and C are friends, this is the probability that C is also a friend of A.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param numiters The maximum number of iterations used to construct the coupled data frame. This has a default value of 1000000, and is the stopping rule if the algorithm does not converge.
#' @param usematrix If an adjacency matrix is output instead of an igraph object. Default is "Y" so an n x n adjacency matrix is output. This is a dgCMatrix.
#'
#' @return Either an igraph of social networks, or a dgCMatrix of n x n.
#'
#' @examples
#' library("dplyr")
#' # with the 50% sample from Township
#' # output as igraph
#' NetworksMadeN <- socnet(Ppl4networks, pplid = 3, pplage = 4, NetworkMatrix, sdused=2,
#'                         probsame = .5, userseed=4, numiters = 100000, usematrix = "N")
#'
#' # output as n x n matrix
#' NetworksMadeY <- socnet(Ppl4networks, pplid = 3, pplage = 4, NetworkMatrix, sdused=2,
#'                         probsame = .5, userseed=4, numiters = 100000, usematrix = "Y")
#'
#' # smaller sample for visualisation
#' #








addnetwork <- function(people, pplid, pplage, netmax, sdused=0, probsame = .5, userseed=NULL,
                   numiters=1000000, usematrix = "Y") {
  
  options(dplyr.summarise.inform=F)
  
  if(!pplid %in% names(people)) {
    stop("The ID variable in the people data frame does not exist.")
  }
  
  if (!pplage %in% names(people)) {
    stop("The age variable in the people data frame does not exist.")
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
  
  # smalldf ID variable
  pplsidcolName <- sym(names(people[pplid]))
  # smalldf age variable
  pplagecolName <- sym(names(people[pplage]))
  
  WorkingDataFrame <- people %>%
    rename(ID = !! pplid, 
           Age = !! pplage) %>%
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
  # plot(ClusteredNetwork)
  
  # right, now that we have the age difference, we could
  # optimise this through swapping. Assuming we want a
  # normal age difference, then optimising the sum of
  # squares is probably what we want:
  
  # we're starting with:
  ss <- sum(age_diff^2)
  # print(ss)
  
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
  for (i in 1:numiters) { # lots of iterations
    
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
      
      # cat("i is", i, "ss is", ss, "and proposed ss is", ss_prop, "\n")
      # swap!
      node_to_people <- proposed
      age_diff <- age_diff_prop
      ss <- ss_prop
      accept[[length(accept) + 1]] <- i
    } else {
      # no swap
    }
    
    # closes for (i in 1:numiters)
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
    igraph::set_vertex_attr("label", value=theIDs[node_to_people]) %>%
    igraph::set_vertex_attr("name", value=theIDs[node_to_people])
  
  # key thing to note is that node_to_people is the map from vertices
  # on the network to people in your data set
  
  if (!(usematrix) == "Y") {
    
    return(ClusteredNetwork)
    
  } else {
    
    return(igraph::as_adj(ClusteredNetwork, type = "both", names = TRUE))
  }
  
}


