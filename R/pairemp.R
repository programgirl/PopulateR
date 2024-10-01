#' Create a data frame of people with employers added
#'
#' This function creates a data frame of people and matching employers, if employed. Two data frames are required: one for the people and one for the employers.
#' A numeric or ordered factor for working hours is required. The minimum value for being in employment must be specified. Anyone coded under this value will be treated as unemployed. Thus, pre-cleaning the people data frame is not required.
#' The employer data frame can be either a summary in the form of the number of employees by employer. The other option is that each row represents a vacancy for an employee. Thus, an employer with 5 employees may be represented as either: a single row with an employee count of 5, or 5 rows with an employee count of 1 in each row.
#' @export
#' @param employers The data frame containing employer data.
#' @param empid The variable containing the unique identifier for each employer. 
#' @param empcount The variable containing the count of employees for each employer.
#' @param people The data frame containing the people that require employers.
#' @param pplid The variable containing the unique ID for each person, in the people data frame.
#' @param wrkhrs The variable containing the hours worked by each person. Must be an ordered factor or numeric. If the variable is an ordered factor, the levels/values must be ascending for hours worked. This is output as an ordered factor.
#' @param hoursmin The wrkhrs value representing the minimum number of hours worked (numeric) or lowest factor level/number. Any wrkhrs value lower than this number/level will be treated as unemployed.
#' @param missval The value that will be used to replace any NA results in the output data frame. If not supplied, NA will be used for all employer-related variables for the non-working people.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return
#' A data frame of the people, with an employer ID attached to each person. Unemployed people will have an employer ID of NA, or the value specified by missval. All columns in the employers data frame, except for the employee counts, are included in the output data frame.
#'
#' @examples
#' library("dplyr")
#'
#' EmployedPeople <- pairemp(EmployerSet, empid = 3, empcount = 2, Township, pplid = 3,
#'                          wrkhrs = 5, hoursmin = 2, userseed = 4)

pairemp <- function(employers, empid, empcount, people, pplid, wrkhrs, hoursmin, missval = NA, userseed = NULL) {

  # setup

  employersRenamed <- employers %>%
    rename(EmployerID = !! empid, NumStaff = !! empcount)

  # employer variable names
  empidcolName <- sym(names(employers[empid]))

  # workers variable names
  pplidcolName <- sym(names(people[pplid]))

  wrkhrscolName <- sym(names(people[wrkhrs]))

  # expand the employer data frame to one row per employee

  employersRenamed <- tidyr::uncount(employersRenamed, NumStaff)

   workersRenamed <- people %>%
    rename(IntHours = !! wrkhrs,
           workersid = !! pplid)

   if (is.ordered(workersRenamed$IntHours) == FALSE &
       is.numeric(workersRenamed$IntHours) == FALSE) {

     stop("Hours worked must be an ordered factor or numeric.")
   }

   workersWorking <- people %>%
     rename(IntHours = !! wrkhrs,
            workersid = !! pplid) %>%
     filter(as.integer(IntHours) >= hoursmin)

   workersUnemployed <- people %>%
     rename(IntHours = !! wrkhrs,
            workersid = !! pplid) %>%
     filter(!(workersid %in% c(workersWorking$workersid)))

   # check if wrkhrs is an ordered factor or numeric
   # fix the 0 employer id for unemployed
   # does "0" if factor and 0 if numeric on employerid
   if(is.numeric(class(employers[empid])) == TRUE) {

     if(is.na(missval) == FALSE) {
       workersUnemployed <- workersUnemployed %>%
       mutate(EmployerID = missval)

     } 
     
   } else {
     
     if(is.na(missval) == FALSE) {
     workersUnemployed <- workersUnemployed %>%
       mutate(EmployerID = as.character(missval))

     }
     # closes if(is.ordered(class(employers[empid]))
   }

   # check if the employer list can take all the workers
  if (nrow(employersRenamed) < nrow(workersWorking)) {

    stop("The number of employed people in the people data frame exceeds the number of staff in the employer population by", 
         nrow(employersRenamed) - nrow(workingsWorking), "people \n")

  }

   # shuffle the rows of the employer data frame

   ShuffleCount <- nrow(workersWorking)

   employersRenamed <- employersRenamed %>%
     slice_sample(n = ShuffleCount, replace = FALSE)


   OutputDataframe <- bind_cols(workersWorking, employersRenamed)



   OutputDataframe <- bind_rows(OutputDataframe, workersUnemployed)


   if (is.ordered(people[,wrkhrs]) == TRUE) {

     #  cat("Hours worked is a factor")

     HoursLabels <- levels(people[,wrkhrs])

     OutputDataframe <- OutputDataframe %>%
       mutate(IntHours = factor(IntHours, labels = c(HoursLabels), ordered =  TRUE))

     #close factor test for hours worked variable
   }


   OutputDataframe <- OutputDataframe %>%
     rename(!!pplidcolName := workersid,
            !!wrkhrscolName := IntHours,
            !!empidcolName := EmployerID)



return(OutputDataframe)

}
