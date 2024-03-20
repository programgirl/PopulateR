#' Create a data frame of individual employers, each with aggregate employee counts.
#'
#' This function constructs individual employers from aggregate counts, such as number of employers per employer type. Employer type is often industry, such as "Sheep, Beef Cattle and Grain Farming". Within each employer type, the number of employers is extracted. The number of employees is then randomly assigned to each of those employers, using the total employee count for that industry. A randomisation method is used to ensure that the company counts can be quite dissimilar across the employers within a type. However, this is constructed by the ratio of employers to employees. If the counts are similar, in this case the number of employees will tend to be 1 for each employer.
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
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return #'A list of three data frames $Companies contains the data frame of synthetic companies, with the number of employees and a mock company name. $Overcounts contains the companies where the number of employers in an industry exceeds the number of employees. This is an informational data frame provides the original values for employee and employee counts. These industries are included in the $Companies file. The count of employers is reset to the count of employees, resulting in synthetic companies with employees count of 1. $NoEmps contains industries with employer counts but no employee counts, employee counts but no employer counts, and a combination of no employers and no employees. These are excluded from the $Companies data frame. Industries with 0 employee and 0 employer counts is likely due to a standardised list of industries being used for all geographic regions. The existence of employers with no employees is indicative of sole-trader/director-only companies. The presence of employees but no employers suggests a data accuracy problem.
#'
#' @examples
#' library("dplyr")
#'
#' TownshipEmployment <- empcreate(AllEmployers, emptypecol = 1, empnumcol = 2,
#'                                 staffnumcol = 3, userseed = 4)

createemp <- function(employers, industry, indsmin, indsmax, pplmin, pplmax, stffname=NULL, cpyname=NULL, userseed = NULL) {
  
  options(dplyr.summarise.inform=F)
  
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
              mutate(Staff = 1,
                     CompanyName = paste0("Company", Paste0Value:(Paste0Value+nrow(.)-1)))

            # get max paste0 value so that the starting company name is updated for the next loop
            EndPaste0Value <- Internalemployer %>%
              slice_tail(n = 1) %>%
              mutate(ValueIs =  as.numeric(gsub("[^[:digit:].]", "",  CompanyName))) %>%
              pull(ValueIs)

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
            mutate(CompanyName = paste0("Company", Paste0Value:(Paste0Value+nrow(.)-1)),
                   Staff = AchievedCompSize)

          EndPaste0Value <- Internalemployer %>%
            slice_tail(n = 1) %>%
            mutate(ValueIs =  as.numeric(gsub("[^[:digit:].]", "",  CompanyName))) %>%
            pull(ValueIs)
          
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
  rename(!!empmincolName := companyCtMin,
         !!empmaxcolName := companyCtMax,
         !!pplmincolName := staffCtMin,
         !!pplmaxcolName := staffCtMax,
         {{stffname}} := Staff,
         {{cpyname}} := CompanyName)
  


  return(OutputDataframe)
  
}
