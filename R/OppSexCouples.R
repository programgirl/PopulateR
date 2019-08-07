#' Create a subset of observations containing only opposite-sex couples
#'
#' This is a wrapper for randomly sampling observations into opposite-sex couples.
#' An even number of observations is output, with an appropriate age-difference distribution between the female and male ages in the couples.
#'
#' @export
#' @param dataframe A data frame containing observations limited to one sex and includes an age column.
#' @param ProbSameSex The probability of any observation being assigned to a same-sex couple.
#' @param UpWeight If TRUE, a subset of ages will be over-sampled.
#' @param UpWeightLowerAge The youngest age for the over-sampling. Required if UpWeight is TRUE.
#' @param UpWeightUpperAge The oldest age for the over-sampling. Required if UpWeight is TRUE.
#' @param AgeVariableIndex The column number of the data frame that contains the ages. Only used if over-sampling is specified. Required if UpWeight is TRUE.
#' @param CoupleIDValue The starting number for generating a variable that identifies the observations in a couple. Must be numeric.
#'
#' @return A data frame of an even number of observations for allocation into same-sex couples.
#'
#' @examples
#'

Create.OppSexCouples <- function(dataframe) {


}
