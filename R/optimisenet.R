#' @importFrom dplyr rename select
#' @importFrom rlang sym !!
#' @importFrom utils head tail
NULL

#' Modify an igraph object to meet the required tolerance for transitivity
#'
#'The transitivity of an igraph is the probability that three people are completely connected. In social terms, this is whether the friend of a friend is also my friend. If A is connected to B, and B is connected to C, this is the probability of whether C is also connected to A, creating a triangle
#'
#'People with less than two edges cannot form a triangle, and are omitted from the calculation.Thus, transitivity is calculated on the number of triangles given the number of possible triangles. Transitivity ranges from 0 (no triangles) to 1 (the maximum number of triangles).
#'
#'The probsame value specifies the desired transitivity of the igraph, with the tolerance parameter creating an acceptance range. The tolerance value must be in the range 0 < tolerance < 1. The tolerance range is 0 <= tolerance <= 1.
#'
#'An igraph object is output.
#'
#'
#' @export
#' @param currentnet An igraph.
#' @param probsame The probability that a friend of a friend is also a friend. For example, if A and B and friends, and B and C are friends, this is the probability that C is also a friend of A. The default is 0.5.
#' @param tolerance The tolerance value for accepting the probsame result. The default is 0.05.
#' @param userseed The user-defined seed for reproducibility. If left blank, the normal set.seed() function will be used.
#' @param numiters The maximum number of iterations used to construct the coupled data frame. This has a default value of 1000, and is the stopping rule if the algorithm does not converge.
#' @param verbose Whether a notification is printed to the console if the number of contacts must be increased by one. Notification is that it has occurred, where the value has been increased, and the original and new number of contacts. The default is FALSE, so no information will be printed to the console.
#'
#' @return An igraph of social networks with the specified transitivity.
#'
#' @examples
#
# library("dplyr")
#
# # smaller sample for visualisation
# set.seed(2) # small datasets can cause problems if a random seed is used for sampling
#  SmallDemo <- Township %>%
#   filter(between(Age, 20, 29)) %>%
#   slice_sample(n = 20)
#   Smallnetwork <- rpois(n = nrow(SmallDemo), lambda = 1.5)
#   NetworkSmallN <- addnetwork(SmallDemo, "ID", "Age", Smallnetwork, sdused=2,
#                               probsame = .5, userseed=4, numiters = 10)
#  # plot(NetworkSmallN)


optimisenet <- function(currentnet, probsame = .5, tolerance = 0.05, userseed=NULL,
                       numiters=1000, verbose = FALSE) {

  withr::local_options(dplyr.summarise.inform = FALSE)

  if(!igraph::is_igraph(currentnet)) {
    stop("The data is not in igraph format")
  }

  if (!(tolerance > 0)) {
    stop("The tolerance must be greater than zero.")
  }

  if(probsame - tolerance < 0 | probsame + tolerance > 1) {
    stop("Tolerance range is outside 0 to 1")
  }
  #
  #
  # #####################################
  # #####################################
  # # get column names as symbols to use inside data frame subfunctions
  # #####################################
  # #####################################
  #
  # # smalldf ID variable
  # pplsidcolName <- sym(names(people[pplid]))
  # # smalldf age variable
  # pplagecolName <- sym(names(people[pplage]))
  #
  # WorkingDataFrame <- people %>%
  #   rename(ID = !! pplid,
  #          Age = !! pplage) %>%
  #   select("ID", "Age")
  #
  # # check that we don't end up with one person needing a match and none to spare
  #
  # # put seed in here as first randomness occurs below
  # if (!is.null(userseed)) {
  #   set.seed(userseed)
  # }
  #
  #
  # if (!(sum(netmax) %% 2 == 0) == TRUE){
  #
  #   # random draw an index position
  #
  #   IndexToAdd1 <- sample.int(length(netmax), 1)
  #
  #   OriginalValue <- netmax[IndexToAdd1]
  #
  #   netmax[IndexToAdd1] <- OriginalValue + 1
  #
  #   if(verbose == TRUE) {
  #     message("The number of network links must be a factor of 2.", "\n")
  #     message("1 has been added to the network matrix at position", IndexToAdd1, "\n")
  #     message("The value has been changed from", OriginalValue, "to", OriginalValue + 1, "\n")
  #
  #     # closes if(prsummary == TRUE) {
  #   }
  #
  #   # closes if (!(sum(netmax) %% 2 == 0) == TRUE){
  # }
  #
  #
  # # construct a graph with this degree distribution
  # WiredNetwork <- igraph::sample_degseq(out.deg = netmax,
  #                                       method="fast.heur.simple")
  #
  # # now, get it so that the clustering is better
  # current_clustering <- igraph::transitivity(WiredNetwork)
  #
  # target_clustering <- probsame # P(triads | length 3 path)
  #
  # print(current_clustering)
  # cat(sprintf("%.10f", igraph::transitivity(WiredNetwork)))
  # return(WiredNetwork)
  #
  # # # https://github.com/cwatson/brainGraph/pull/26
  # # ClusteredNetwork <- brainGraph::sim.rand.graph.clust(WiredNetwork,
  # #                                                      rewire.iters=0,
  # #                                                      cl=target_clustering,
  # #                                                      max.iters=100)
  #
  #
  # igraph::transitivity(ClusteredNetwork)
  #
  #
  # # OK, now we want to get our ages on this. One way is to just
  # # place them directly, but let's do it slightly indirectly so
  # # that we can use it later. We start with a map from nodes to
  # # people:
  # node_to_people <- 1:nrow(WorkingDataFrame)
  # theages <- WorkingDataFrame$Age
  # theIDs <- WorkingDataFrame$ID
  #
  #
  # # now we can put the ages on the network and plot it:
  # # can add more than one attribute, see examples here
  # # https://rdrr.io/cran/igraph/src/R/attributes.R
  #
  #
  # ClusteredNetwork %>%
  #   igraph::set_vertex_attr("Age", value=theages[node_to_people])
  #
  # # now, it'd be nice to work out the age differences as well
  # # we do that by grabbing the edges in the graph as a matrix
  # # of indices (which we'll then pass through node_to_people
  # # to get to the people that each node corresponds to)
  #
  # edges = igraph::as_edgelist(ClusteredNetwork)
  #
  # # helper to get the age differences: The idea is that
  # # we use the edges matrix (which has the vertex index of
  # # each edge) and then pass that through node_to_people
  # # to map to the corresponding row in the people data frame
  # # and then grab the age out of that. We then compute
  # # the difference
  #
  # get_age_diff <- function(edges, node_to_people, Age) {
  #   # use the edges and node_to_people to lookup ages
  #   age1 = Age[node_to_people[edges[,1]]]
  #   age2 = Age[node_to_people[edges[,2]]]
  #   age1 - age2
  # }
  #
  # age_diff <- get_age_diff(edges, node_to_people, theages)
  #
  # ClusteredNetwork %>%
  #   igraph::set_vertex_attr("label", value=theages[node_to_people]) %>%
  #   igraph::set_edge_attr("label", value=age_diff)
  #
  # # right, now that we have the age difference, we could
  # # optimise this through swapping. Assuming we want a
  # # normal age difference, then optimising the sum of
  # # squares is probably what we want:
  #
  # # we're starting with:
  # ss <- sum(age_diff^2)
  #
  # # now, we're going to swap which node represents which person
  # # by shuffling the node_to_people vector.
  #
  # # I'm not sure the best way to do this. One thing we could try
  # # is just use shuffle() to randomly permute it and then compare.
  # # but presumably to optimise you'll want to move slowly around
  # # the permutation space?
  #
  # # But, not too slowly? So what I'm doing here is going to pull
  # # out a random number from 2 onwards as the number of nodes to
  # # swap around, and then swap them around by shifting indices.
  #
  # # TODO: A better strategy (as it seems that it improves a bit early
  # # on, then not at all) might be to run an outer loop that does
  # # a bunch of big shuffles as starting points, then the inner
  # # loop that optimises within - hopefully this might better cover
  # # the space? (The space is BIG ofcourse)
  #
  # # helper function for shifting a vector (one of many ways to
  # # permute one). i.e. shift_vector(c(1,2,3,4),1) = c(2,3,4,1)
  # shift_vector <- function(x, n = 1) {
  #   if (n == 0) x else c(tail(x, -n), head(x, n))
  # }
  #
  # accept <- list() # this is just to store how often we accept a proposal
  # for (i in 1:numiters) { # lots of iterations
  #
  #
  #   # do the permutation: first off, how many people do we permute?
  #   num_to_permute <- 2 + rpois(1, 0.5)
  #   # how much should we shift them around?
  #   shift <- sample(num_to_permute-1, 1)
  #   # now pull out this number of people at random
  #   swap <- sample(length(node_to_people), num_to_permute)
  #   # and swap them over
  #   proposed <- node_to_people
  #   proposed[swap] <- proposed[shift_vector(swap, shift)]
  #
  #   # compute the age difference for this proposal
  #   age_diff_prop <- get_age_diff(edges, proposed, theages)
  #   # very crude here - just optimising sum of squares
  #   ss_prop <- sum(age_diff_prop^2)
  #
  #   if (ss_prop < ss) {
  #
  #     # swap!
  #     node_to_people <- proposed
  #     age_diff <- age_diff_prop
  #     ss <- ss_prop
  #     accept[[length(accept) + 1]] <- i
  #   } else {
  #     # no swap
  #   }
  #
  #   # closes for (i in 1:numiters)
  # }
  # unlist(accept)
  # # ss
  #
  # # plot with edge weights with differences and ages on nodes
  #
  # ClusteredNetwork <- ClusteredNetwork %>%
  #   igraph::set_edge_attr("label", value=age_diff) %>%
  #   igraph::set_vertex_attr("label", value=theIDs[node_to_people]) %>%
  #   igraph::set_vertex_attr("name", value=theIDs[node_to_people])
  #
  # # key thing to note is that node_to_people is the map from vertices
  # # on the network to people in your data set
  #
  # if (!(usematrix) == TRUE) {
  #
  #   return(ClusteredNetwork)
  #
  # } else {
  #
  #   return(igraph::as_adj(ClusteredNetwork, type = "both", names = TRUE))
  }



