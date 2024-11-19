#' @importFrom data.table :=
#' @importFrom dplyr bind_rows mutate pull rename slice slice_tail
#' @importFrom magrittr %>%
#' @importFrom rlang sym !!
#' @importFrom stats rmultinom runif
NULL

#' Create employers, each with employee counts
#'
#' Constructs individual employers from aggregate counts, such as number of employers per employer type. Employer type is often industry, such as "Sheep, Beef Cattle and Grain Farming". Within each employer type, the number of employers is extracted. The number of employees is then randomly assigned to each of those employers, using the total employee count for that industry. A randomisation method is used to ensure that the company counts can be quite dissimilar across the employers within a type. However, this is constructed by the ratio of employers to employees. If the number of employers is similar to the number of employees, the number of employees will tend to be 1 for each employer.
#'
#' @export
#' @param employers A data frame containing aggregate data on employers.
#' @param industry The variable containing the types of employers. This can be an industry code.
#' @param indsmin The variable containing the minimum number of employees in each industry.
#' @param indsmax The variable containing the maximum number of employees in each industry.
#' @param pplmin The variable containing the minimum number of staff in each industry.
#' @param pplmax The variable containing the maximum number of staff in each industry.
#' @param stffname The variable name to use for the staff counts for each employer.
#' @param cpyname The variable name to use for the companies.
#' @param userseed If specified, this will set the seed to the number provided. If not, the normal set.seed() function will be used.
#'
#' @return #'A data frames of synthetic companies, with the number of employees and a mock company name.
#'
#' @examples
#'
#' library("dplyr")
#'
#' TownshipEmployment <- createemp(AllEmployers, industry = "ANZSIC06", indsmin = "minCo",
#'                                 indsmax = "maxCo", pplmin = "minStaff", pplmax = "maxStaff",
#'                                 stffname="NumEmployees", cpyname="Company", userseed = 4)

createemp <- function(employers, industry, indsmin, indsmax, pplmin, pplmax, stffname=NULL, cpyname=NULL, userseed = NULL) {

  withr::local_options(dplyr.summarise.inform=F)

  if(is.null(stffname)) {

    stop("The name of the variable containing the staff counts must be supplied.")
  }

  if(is.null(cpyname)) {

    stop("The name of the variable containing the company names must be supplied.")
  }

  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  emptypecolName <- sym(names(employers[industry]))
  empmincolName <- sym(names(employers[indsmin]))
  empmaxcolName <- sym(names(employers[indsmax]))
  pplmincolName <- sym(names(employers[pplmin]))
  pplmaxcolName <- sym(names(employers[pplmax]))

  #####################################
  #####################################
  # construct internal data frame
  #####################################
  #####################################

  employerRenamed <- employers %>%
    rename(companyCtMin = !! empmincolName,
           companyCtMax = !! empmaxcolName,
           staffCtMin = !! pplmincolName,
           staffCtMax = !! pplmaxcolName)



  # put in the starting value for the paste0 used in the company name
  Paste0Value <- 1

  # set the seed
  if (!is.null(userseed)) {
    set.seed(userseed)
  }

  #####################################
  #####################################
  # Construct the separate companies
  #####################################
  #####################################


  for (i in 1:nrow(employerRenamed)) {

    currentEmp <- employerRenamed[i,]

    minEmp <- currentEmp$companyCtMin
    maxEmp <- currentEmp$companyCtMax

    numEmp <- round(runif(1, minEmp, maxEmp),0)

    minStaff <- currentEmp$staffCtMin
    maxStaff <- currentEmp$staffCtMax

    numStaff <- round(runif(1, minStaff, maxStaff),0)

    # cat("minEmp is", minEmp, "maxEmp is", maxEmp, "minStaff is", minStaff, "maxStaff is", maxStaff, "numEmp is", numEmp, "numStaff is", numStaff, "\n")

    if(numStaff < numEmp) {

      # cat("loop entered \n")
      numEmp <- numStaff
    }


    #####################################
    #####################################
    # Number of companies is the same as the number of employees in the industry
    #####################################
    #####################################

    if(numEmp == numStaff) {

      Internalemployer <- currentEmp %>%
              slice(rep(seq_len(n()), numEmp)) %>%
              mutate(n = row_number(),
                     Staff = 1,
                     CompanyName = paste0("Company", Paste0Value+(n-1))) %>%
              select(- "n")

            # get max paste0 value so that the starting company name is updated for the next loop
            EndPaste0Value <- Internalemployer %>%
              slice_tail(n = 1) %>%
              mutate(ValueIs =  as.numeric(gsub("[^[:digit:].]", "",  .data$CompanyName))) %>%
              pull(.data$ValueIs)

            # cat("The last company number is", EndPaste0Value, "\n")

            Paste0Value <- EndPaste0Value + 1

                # add the employers to the final dataframe

                if(exists("OutputDataframe")) {

                  OutputDataframe <- bind_rows(OutputDataframe, Internalemployer)

                } else {

                  OutputDataframe <- Internalemployer

                  # closes else for if(exists("OutputDataframe"))
                }

            #####################################
            #####################################
            # Number of employees > number of employees in the industry
            #####################################
            #####################################
    } else {


      # note this stackoverflow answer https://stackoverflow.com/a/52559775/1030648

          AchievedCompSize <- as.integer(rmultinom(n = 1, size = numStaff, prob = rep(1/numEmp, numEmp)))

          # ensure that all companies have at least one employee

          while(0 %in% AchievedCompSize) {

            BaseCompSize <- rep(1, numEmp)
            NewNumStaff <- numStaff - sum(BaseCompSize)

            AddedCompSize <- as.integer(rmultinom(n = 1, size = NewNumStaff, prob = rep(1/numEmp, numEmp)))

            AchievedCompSize <- BaseCompSize + AddedCompSize

            # closes while(0 %in% AchievedCompSize)
          }

          Internalemployer <- currentEmp %>%
            slice(rep(seq_len(n()), numEmp)) %>%
            mutate(n = row_number(),
                   CompanyName = paste0("Company", Paste0Value+(n-1)),
                   Staff = AchievedCompSize) %>%
            select(- "n")

          EndPaste0Value <- Internalemployer %>%
            slice_tail(n = 1) %>%
            mutate(ValueIs =  as.numeric(gsub("[^[:digit:].]", "",  .data$CompanyName))) %>%
            pull(.data$ValueIs)

          Paste0Value <- EndPaste0Value + 1

          if(exists("OutputDataframe")) {

            OutputDataframe <- bind_rows(OutputDataframe, Internalemployer)

          } else {

            OutputDataframe <- Internalemployer

            # closes else for if(exists("OutputDataframe"))
          }

          #####################################
          #####################################
          # Number of employees > number of employees in the industry block closed
          #####################################
          #####################################
          }


    # closes for (i in 1:nrow(employerRenamed))
  }


  # rename the variables

  OutputDataframe <- OutputDataframe %>%
  rename(!!empmincolName := "companyCtMin",
         !!empmaxcolName := "companyCtMax",
         !!pplmincolName := "staffCtMin",
         !!pplmaxcolName := "staffCtMax",
         {{stffname}} := "Staff",
         {{cpyname}} := "CompanyName")



  return(OutputDataframe)

}
