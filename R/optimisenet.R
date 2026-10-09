#' @importFrom dplyr rename select
#' @importFrom rlang sym !!
#' @importFrom utils head tail
NULL

#' Modify an undirected igraph object to meet the required tolerance for transitivity
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

  ###################################################################
  # data checking
  ###################################################################

  if(!igraph::is_igraph(currentnet)) {
    stop("The data is not in igraph format")
  }

  if (igraph::is_directed(currentnet)) {
    stop("The network must be undirected.")
  }

  if (!(tolerance > 0)) {
    stop("The tolerance must be greater than zero.")
  }

  if(probsame - tolerance < 0 | probsame + tolerance > 1) {
    stop("Tolerance range is outside 0 to 1")
  }


  ###################################################################
  # setup for improvement
  ###################################################################

  mintarget <- probsame - tolerance
  maxtarget <- probsame + tolerance

  current_clustering <- igraph::transitivity(currentnet)

  cat("Min tolerance is", mintarget, "max tolerance is", maxtarget, "current value is",
      current_clustering, "\n")

  if(current_clustering < mintarget | current_clustering > maxtarget) {

    cat("Entered loop")

    for(i in 1:numiters) {
      #ensure that there is no endless loop



      i = i + 1
      # ends for(i in 1:numiters) {
    }

    # ends if(current_clustering < mintarget | current_clustering > maxtarget) {
  }

  #   return(igraph::as_adj(ClusteredNetwork, type = "both", names = TRUE))
  }



