#' #' Non-partnered synthetic people
#' #'
#' #' @description
#' #' A subset of people from the Township data frame, aged 20 years and older with a relationship status of "NonPartnered".
#' #'
#' #' @format A data frame of 2,213 rows and 5 variables
#' #' \describe{
#' #'     \item{Sex}{Sex of the person}
#' #'     \item{Relationship}{Relationship status of the person}
#' #'     \item{ID}{The unique identifier for the person}
#' #'     \item{Age}{The age of the person}
#' #'     \item{HoursWorked}{The number of hours worked in employment, per week}
#' #' }
#' "AdultsNoID"
#'
#' #' The number of employers and employees, by industry, in the Timaru District, 2018
#' #'
#' #' @description
#' #' The number of businesses and employees by industry, Timaru District, for 2018, sourced from Statistics New Zealand. Statistics New Zealand data are licensed by Stats NZ for reuse under the Creative Commons Attribution 4.0 International licence. The data has been modified by adding in four additional variables, representing the estimated minimum and maximum counts of businesses and employees.
#' #'
#' #' @format A data frame of 183 rows and 7 variables
#' #' \describe{
#' #'     \item{ANZSIC06}{The code and associated name for each industry}
#' #'     \item{BusinessCount}{The random-rounded count of employers in the industry}
#' #'     \item{EmployeeCount}{The random-rounded count of employees in the industry}
#' #'     \item{nimCo}{The minimum number of employers in the industry}
#' #'     \item{maxCo}{The maximum number of employers in the industry}
#' #'     \item{minStaff}{The minimum number of people employed in the industry}
#' #'     \item{maxStaff}{The maximum number of people employed in the industry}
#' #'}
#' "AllEmployers"
#'
#' #' Synthetic people restricted to an age range
#' #'
#' #' @description
#' #' A subset of people from the Township data frame, aged between 20 and 91 years. Age bands, and the associated minimum and maximum ages, have been added.
#' #'
#' #' @format A data frame of 7,568 rows and 8 variables
#' #' \describe{
#' #'     \item{Sex}{Sex of the person}
#' #'     \item{Relationship}{Relationship status of the person}
#' #'     \item{ID}{The unique identifier for the person}
#' #'     \item{Age}{The age of the person}
#' #'     \item{HoursWorked}{The number of hours worked in employment, per week}
#' #'     \item{Ageband}{The ten-year age band for the age}
#' #'     \item{MinAge}{The minimum age in the age band}
#' #'     \item{MaxAge}{The maximum age in the age band}
#' #'     }
#' "BadRels"

#' Synthetic employers and their employee counts
#'
#' @description
#' Synthetic employers and their associated number of employees, randomly constructed using the "AllEmployers" data frame.
#'
#' @format A data frame of 225 rows and 3 variables
#' \describe{
#'     \item{ANZSIC06}{The code and associated name for the industry associated with the employer}
#'     \item{NumEmployees}{The count of employees for the employer}
#'     \item{Company}{The name of the employer}
#' }
#'
"EmployerSet"

#' #' The proportion of people in a relationship, by age band within sex
#' #'
#' #' @description
#' #' The estimated proportion of people in a relationship, by age band within sex, for people aged between 20 and 90 years.
#' #'
#' #' @format A data frame of 14 rows and 7 variables
#' #' \describe{
#' #'     \item{Sex}{Either Male or Female}
#' #'     \item{AgeBand}{The 10-year age band}
#' #'     \item{MinAge}{The minimum age of the age band}
#' #'     \item{MaxAge}{The maximum age of the age band}
#' #'     \item{Relationship}{All people are Partnered}
#' #'     \item{RelProps}{The proportion of people who have a relationship status of "Partnered"}
#' #'     \item{MidPoints}{The median age in the age band}
#' #' }
#' #'
#' "GroupInfo"
#'
#' #' People in age groups, in the Timaru District
#' #'
#' #' @description
#' #' Synthetic people produced from combining aggregate Timaru District 2018 census data, using tablecodes 8277 and 8395, sourced from Statistics New Zealand. Statistics New Zealand data are licensed by Stats NZ for reuse under the Creative Commons Attribution 4.0 International licence.
#' #'
#' #' @format A data frame with 46,293 rows and 6 variables
#' #' \describe{
#' #'     \item{Sex}{Sex of the person}
#' #'     \item{Age.group}{Age group in five-year age bands}
#' #'     \item{Relationship}{Relationship status of the person}
#' #'     \item{LowerAge}{The youngest age in the Age.group}
#' #'     \item{UpperAge}{The oldest age in the Age.group}
#' #'     \item{ID}{The unique identifier for the person}
#' #' }
#' "InitialDataframe"
#'
#' #' Four person households, with a school status for each person
#' #'
#' #' @description
#' #' Four-person households, consisting of one parent and three children, with a combination of people in school and not in school. Ages 15 through 18 contain a mixture of people in school and those who have left school. This has been constructed from the Township data frame.
#' #'
#' #' @format A data frame of 3,052 rows and 8 variables
#' #' \describe{
#' #'     \item{Sex}{Sex of the person}
#' #'     \item{Relationship}{Relationship status of the person}
#' #'     \item{ID}{The unique identifier for the person}
#' #'     \item{Age}{The age of the person}
#' #'     \item{HoursWorked}{The number of hours worked in employment, per week}
#' #'     \item{SchoolStatus}{The indicator of whether the person is in school (Y) or not (N)}
#' #'     \item{HouseholdID}{The household identifier for the person}
#' #'     \item{SexCode}{Sex, (F)emale or (M)ale}
#' #' }
#' "IntoSchools"

#' School leavers
#'
#' @description
#' School leavers in the Canterbury Region, counts by age and sex, for the period 2009 to 2018.
#'
#' @format A data frame with 120 rows and 4 variables
#' \describe{
#'     \item{YearLeft}{The year for the school leaver count}
#'     \item{Sex}{The sex for the school leaver count}
#'     \item{Age}{The age for the school leaver count}
#'     \item{Total}{The count of adolescents who left school in that year, of that age and sex}
#' }
#' @source Ministry of Education. The Ministry of Education's data are licensed by the Ministry of Education for reuse under the Creative Commons Attribution 4.0 International licence.
"LeftSchool"

#' The number of contacts for 5000 person
#'
#' @description
#' A matrix of 1,000 integers constricted using a Poisson distribution. Each value is the number of contacts for a person.
#'
#' @format A list of 1,000 integers
"NetworkMatrix"

#' Synthetic people living in the Timaru District
#'
#' @description
#' 1000 synthetic people.
#'
#' @format A data frame with 1,000 rows and 5 variables
#' \describe{
#'     \item{Sex}{Sex of the person}
#'     \item{Relationship}{Relationship status of the person}
#'     \item{ID}{The unique identifier for the person}
#'     \item{Age}{The age of the person}
#'     \item{HoursWorked}{The number of hours worked in employment, per week}
#' }
#' @source Timaru District 2018 census data (tablecodes 8277, 8395, and 8460), sourced from Statistics New Zealand. Statistics New Zealand data are licensed by Stats NZ for reuse under the Creative Commons Attribution 4.0 International licence.
"Ppl4networks"

#' Sex/Age pyramid for teenagers in the Canterbury Region
#'
#' @description
#' The number of people, by age and sex, living in the Canterbury region, restricted to ages 13 to 19 years.
#'
#' @format A data frame with 14 observations and 4 variables
#' \describe{
#'     \item{Sex}{The sex relating to the count}
#'     \item{Age.group}{String variable of age plus the text " years"}
#'     \item{Value}{The count of adolescents}
#'     \item{Age}{The age relating to that count}
#' }
#' @source Canterbury region 2018 census data (tablecode 8277), sourced from Statistics New Zealand. Statistics New Zealand data are licensed by Stats NZ for reuse under the Creative Commons Attribution 4.0 International licence.
"RegionalStructure"

#' #' Schools and their roll counts
#' #'
#' #' @description
#' #' Nineteen schools in the Canterbury region, with their 2018 roll counts, sourced from the Ministry of Education. The Ministry of Education's data are licensed by the Ministry of Education for reuse under the Creative Commons Attribution 4.0 International licence.
#' #'
#' #' @format A data frame with 266 rows and 5 variables
#' #' \describe{
#' #'    \item{School.ID}{The numeric ID for the school}
#' #'    \item{School.Name}{The name for the school}
#' #'    \item{Gender}{Indicator of whether the school is (C)o-ed, (F)emale-only, or (M)ale-only}
#' #'    \item{AgeInRoll}{The age of possible students}
#' #'    \item{RollCount}{The number of students. The value is 0 if no students that age attend.}
#' #' }
#' "SchoolsToUse"
#'
#' #' Sex/Age pyramid data for Timaru District
#' #'
#' #' @description
#' #' The number of people, by age and sex, living in the Timaru District. The data is based on 2018 census data in tablecode 8277, sourced from Statistics New Zealand. Statistics New Zealand data are licensed by Stats NZ for reuse under the Creative Commons Attribution 4.0 International licence.
#' #'
#' #' @format A data frame with 190 rows and 4 variables
#' #' \describe{
#' #'     \item{Age.group}{Age group, in five-year age bands}
#' #'     \item{Sex}{Sex, either Male or Female}
#' #'     \item{Value}{The number of people that age and sex}
#' #'     \item{Age}{Age at last birthday}
#' #' }
#' "SingleAges"

#' Simulated township
#'
#' @description
#' 10,000 simulated people.
#'
#' @format A data frame with 10,000 rows and 5 variables
#' \describe{
#'     \item{Sex}{Sex of the person}
#'     \item{Relationship}{Relationship status of the person}
#'     \item{ID}{The unique identifier for the person}
#'     \item{Age}{The age of the person}
#'     \item{HoursWorked}{The number of hours worked in employment, per week}
#' }
#' @source Timaru District 2018 census data, using tablecodes 8277, 8395, and 8460, sourced from Statistics New Zealand. Statistics New Zealand data are licensed by Stats NZ for reuse under the Creative Commons Attribution 4.0 International licence.
"Township"

#' #' People with a school indicator
#' #'
#' #' @description
#' #' A modified version of "Township" data frame with a school indicator variable added.
#' #'
#' #' @format A data frame with 10,000 rows and 6 variables
#' #' \describe{
#' #'     \item{Sex}{Sex of the person}
#' #'     \item{Relationship}{Relationship status of the person}
#' #'     \item{ID}{The unique identifier for the person}
#' #'     \item{Age}{The age of the person}
#' #'     \item{HoursWorked}{The number of hours worked in employment, per week. This is an ordered factor.}
#' #'     \item{SchoolStatus}{The indicator of whether the person is in school (Y) or not (N)}
#' #' }
#' "WithSchoolInd"
#'
#' #' Adolescents with a school status and employment hours
#' #'
#' #' @description
#' #' Synthetic adolescents aged between 15 and 18, produced from aggregate Timaru District 2018 census data, using tablecodes 8277, 8395, and 8460. School status was added using school leavers data produced by the Ministry of Education. Statistics New Zealand and the Ministry of Education's data are licensed, separately, for reuse under the Creative Commons Attribution 4.0 International licence.
#' #'
#' #' @format A data frame of 546 observations and 6 variables
#' #' \describe{
#' #'     \item{Sex}{Sex of the person}
#' #'     \item{Relationship}{Relationship status of the person}
#' #'     \item{ID}{The unique identifier for the person}
#' #'     \item{Age}{Age of the person}
#' #'     \item{HoursWorked}{The number of hours worked in employment, per week}
#' #'     \item{SchoolStatus}{The indicator of whether the person is in school (Y) or not (N)}
#' #' }
#' "WorkingAdolescents"
