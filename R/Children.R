#' Create a subset of observations containing children matched to guardians
#'
#' This function creates a data frame of guardian and child matches, based on a population distribution of age differences. The distribution used is the skew normal.
#' Two data frames are required. The Guardians data frame contains the age data, to which the distribution will be applied. The
#' Children data frame contains the age data, of the children, from which the age counts to match are constructed. The sum of the number of children to match
#' in the Guardians data frame must equal the row number in the Children data frame. If the counts are different, no matching will occur.
#' The number of observations output is the sum of the number of rows in the Guardians data frame and the sum of the rows in the Children data frame.
#'
#' @export
#' @param Guardians A data frame containing observations limited to the guardians to which the children will be matched.
#' @param GuardianIDVariable The column number for the ID variable in the Parents data frame.
#' @param GuardianAgeVariable The column number for the age variable in the Parents data frame.
#' @param GuardianType The column number for the type of the guardian, restricted to "P" or "G". If P, the guardian is assumed to be a parent, and the age distribution
#' entered will be applied against the ages provided in the GuardianAgeVariable. If "G", the guardian is assumed to be a grandparent, and the age distribution will
#' be used twice because the child will be a grandchild. The first use estimates the age of the grandparent at the age of the parent's birth. The second use
#' estimates the age of the parent at the age of the child's birth. A two-step process is required as most of the children will be aged more than 0 years in the
#' Children data frame.
#' @param NumberChildren the column number for the number of children to match to the a guardian.
#' @param TwinRate The expected proportion of twins. This requires between number between 0 and 1. The default value is 0. This proportion is applied to the total count of children,
#' with singletons included in the estimation.
#' @param Children A data frame containing observations limited to children who will be matched to the guardians.
#' @param ChildrenIDVariable The column number for the ID. variable in the Children data frame.
#' @param ChildrenAgeVariable The column number for the age variable in the Children data frame.
#' @param xiUsed The xi value for the skew normal distribution.
#' @param OmegaUsed The omega value for the skew normal distribution.
#' @param AlphaUsed The alpha value for the skew normal distribution.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param pValueToStop The primary stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param NumIterations The maximum number of iterations used to construct the coupled data frame. This has a default value of 1000000, and is the stopping rule
#' if the algorithm does not converge.
#' @param PairedIDValue The starting number for generating a variable that identifies the observations in guardian/children match. Must be numeric.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied in quotes.
#'
#' @return A data frame of the matched guardian/child observations.
#'
#' @examples

# guardian_child <- function(Guardians, GuardiansIDVariable=NULL, GuardiansAgeVariable=NULL, GuardianType=NULL, NumberChildren=NULL, TwinRate=0, Children, ChildrenIDVariable=NULL,
#                          ChildrenAgeVariable=NULL, xiUsed=NULL, OmegaUsed=NULL, AlphaUsed=NULL, UserSeed=NULL, pValueToStop=NULL, NumIterations=NULL, PairedIDValue=NULL,
#                          HouseholdNumVariable=NULL) {
#
#   # content check
#   if (!any(duplicated(Guardians[GuardianIDVariable])) == FALSE) {
#     stop("The column number for the ID variable in the Guardian data frame must be supplied.")
#   }
#
#   if(sum(Guardians[NumberChildren]) !=nrow(Children)) {
#     stop("The number of children to match does not equal the number of children supplied.")
#   }
#
#   if (!is.numeric(GuardianAgeVariable)) {
#     stop("Both the Guardian ID and the Guardian age column numbers must be supplied.")
#   }
#
#   if(!(GuardianType %in% c("G", "P"))) {
#     stop("The Guardian type must be either P or G.")
#   }
#
#   if (!any(duplicated(Children[ChildrenIDVariable])) == FALSE) {
#     stop("The column number for the ID variable in the Children data frame must be supplied.")
#   }
#
#   if(is.null(HouseholdNumVariable)) {
#     stop("A name for the household count variable must be supplied.")
#   }
#
#
# }

#TestStops <- guardian_child(SubsetNoPartners, )

