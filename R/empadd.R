#' Create a data frame of people with employers added
#' #'
#' This function constructs individual employers from aggregate counts, such as number of employers per employer type. Employer type is often industry, such as "Sheep, Beef Cattle and Grain Farming". Within each employer type, the number of employers is extracted. The number of employees is then randomly assigned to each of those employers, using the total employee count for that industry. A randomisation method is used to ensure that the company counts can be quite dissimilar across the employers within a type. However, this is constructed by the ratio of employers to employees. If the counts are similar, in this case the number of employees will tend to be 1 for each employer.
#'
#' @export
#' @param employers A data frame containing employer data.
#' @param empid The column number for the employer ID.
#' @param empcount The column number that provides the number of employees for each employer.
#' @param workers A data frame containing the people that must be matched to employers.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return
#'
#'
#'    #'A list of three data frames $Companies contains the data frame of synthetic companies, with the number of employees and a mock company name. $Overcounts contains the companies where the number of employers in an industry exceeds the number of employees. This is an informational data frame provides the original values for employee and employee counts. These industries are included in the $Companies file. The count of employers is reset to the count of employees, resulting in synthetic companies with employees count of 1. $NoEmps contains industries with employer counts but no employee counts, employee counts but no employer counts, and a combination of no employers and no employees. These are excluded from the $Companies data frame. Industries with 0 employee and 0 employer counts is likely due to a standardised list of industries being used for all geographic regions. The existence of employers with no employees is indicative of sole-trader/director-only companies. The presence of employees but no employers suggests a data accuracy problem.
#'
#' @example
#' library("dplyr")
#'
#' TownshipEmployment <- empcreate(AllEmployers, emptypecol = 1, empnumcol = 2, staffnumcol = 3, userseed = 4)

empadd <- function(employers, empid, empcount, workers, maxfill = 1, userseed = NULL) {

  # expand the employers to one row per employee




}
