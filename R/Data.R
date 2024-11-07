#' Mock people with no household identifiers
#'
#' A subset of people from the Township data frame, age 20 years and older with a relationship status of "NonPartnered".
#'
#' @format A data frame of 2,213 rows and 5 variables
#' \describe{
#'     \item{Sex}{Sex of the person}
#'     \item{Relationship}{Relationship status of the person}
#'     \item{ID}{The unique identifier for the person}
#'     \item{Age}{The age of the person}
#'     \item{HoursWorked}{The number of hours worked in employment, per week}
#' }
#'"AdultsNoID"
#'
#'
#' A tibble of the number of employers and employees, by industry, in the Timaru District, 2018
#'
#' A dataset produced from tablecode 7602, from the official Stats NZ table builder website \url{http://nzdotstat.stats.govt.nz/wbos/Index.aspx}. Stats NZ data are licensed by Stats NZ for reuse under the Creative Commons Attribution 4.0 International licence.
#'
#' @format A data frame of 183 rows and 7 variables
#' \describe{
#'     \item{ANZSIC06}{The code and associated name for each industry}
#'     \item{BusinessCount}{The random-rounded count of employers in the industry}
#'     \item{EmployeeCount}{The random-rounded count of employees in the industry}
#'     \item{nimCo}{The minimum number of employers in the industry}
#'     \item(maxCo){The maximum number of employers in the industry}
#'     \item{minStaff}{The minimum number of people employed in the industry}
#'     \item{maxStaff}{The maximum number of people employed in the industry}
#'}
#'"AllEmployers"
#'
#'
#'School rolls in the Canterbury Region, 2018
#'
#' A dataset produced from the official statistics of school rolls produced by the Ministry of Education. The Ministry of Education's data are licensed by the Ministry of Education for reuse under the Creative Commons Attribution 4.0 International licence.
#'
#' @format A tibble of 4,046 rows and 7 columns
#'\describe{
#'    \item{School.ID}{The numeric ID for the school}
#'    \item{School.Name}{The name for the school}
#'    \item{Gender}{Indicator of whether the school is (C)o-ed, (f)emale-only, or (M)ale-only}
#'    \item{Female}{The count of female students for the school, total. 0 if single-sex school for opposite sex}
#'    \item{Male}{The count of male students for the school, total. 0 if single-sex school for opposite sex}
#'    \item{AgeInRoll}{The number of students that age in the school's roll}
#'    \item{RollCount}{The number of students for that school and age combination, at that school. 0 if no students that age attend.}
#'}
#'"CRSchools"
#'
#'
#' Synthetic employers and their employee counts
#'
#' A synthetic set of employers and their associated number of employees, randomly constructed from "AllEmployers". This is a subset of all possible employers.
#'
#' @format A tibble of 225 rows and 3 variables
#' \describe{
#'     \item{ANZSIC06}{The code and associated name for the industry associated with the employer}
#'     \item{EmployeeCount}{The count of employees for the employer}
#'     \item{CompanyName}{The name of the employer}
#' }
#' "EmployerSet"
#'
#'
#' People in age groups, in Timaru District
#'
#' A dataset produced from combining aggregate 2018 census data, using tablecodes 8277 and 8395, from the official Stats NZ table builder website \url{http://nzdotstat.stats.govt.nz/wbos/Index.aspx}. Stats NZ data are licensed by Stats NZ for reuse under the Creative Commons Attribution 4.0 International licence.
#'
#' @format A tibble with 46,293 rows and 6 variables
#' \describe{
#'     \item{Sex}{Sex of the person}
#'     \item{Age.group}{Age group in five-year bands}
#'     \item{Relationship}{Relationship status of the person}
#'     \item{LowerAge}{The youngest age in the Age.group}
#'     \item{UpperAge}{The oldest age in the Age.group}
#'     \item{ID}{The unique identifier for the person}
#' }
#' "InitialDataframe"
#'
#'
#' Four person households, one parent and three children, with school status recorded
#'
#' A dataset of households, with a combination of people in school and not in school. Ages 15 through 18 contain a mixture of people in school and those who have left school. This has been constructed from the Township data frame.
#'
#' @format A data frame of 3,080 rows and 8 variables
#' \describe{
#'     \item{Sex}{Sex of the person}
#'     \item{Relationship}{Relationship status of the person}
#'     \item{ID}{The unique identifier for the person}
#'     \item{Age}{The age of the person}
#'     \item{HoursWorked}{The number of hours worked in employment, per week}
#'     \item{SchoolStatus}{The indicator of whether the person is in school (Y) or not (N)}
#'     \item{HouseholdID}{The household identifier for the person}
#'     \item{SexCode}{Sex, limited to the first letter}
#' }
#' "IntoSchools"
#'
#'
#' School leavers in the Canterbury Region, 2009 to 2018
#'
#' A dataset produced from the official statistics of school leavers produced by the Ministry of Education. The Ministry of Education's data are licensed by the Ministry of Education for reuse under the Creative Commons Attribution 4.0 International licence.
#'
#' @format A tibble with 120 rows and 4 variables
#' \describe{
#'     \item{YearLeft}{The year for the school leaver count}
#'     \item{Sex}{The sex for the school leaver count}
#'     \item{Age}{The age for the school leaver count}
#'     \item{Total}{The count of adolescents who left school in that year, of that age and sex}
#' }
#' "LeftSchool"
#'
#'
#' Distribution of network sizes
#'
#' A matrix of 5,000 integers constricted using a Poisson distribution.
#'
#' @format A list of 5,000 integers
#'
#'
#' Random selection of 5,000 synthetic people in the Timaru District from the 2018 census
#'
#' A dataset produced from combining aggregate 2018 census datasets, using tablecodes 8277, 8395, and 8460, from the official Stats NZ table builder website \url{http://nzdotstat.stats.govt.nz/wbos/Index.aspx}. Stats NZ data are licensed by Stats NZ for reuse under the Creative Commons Attribution 4.0 International licence. A 50% sample of the Township data frame.
#'
#' @format A data frame with 5,000 rows and 5 variables
#' \describe{
#'     \item{Sex}{Sex of the person}
#'     \item{Relationship}{Relationship status of the person}
#'     \item{ID}{The unique identifier for the person}
#'     \item{Age}{The age of the person}
#'     \item{HoursWorked}{The number of hours worked in employment, per week}
#' }
#' "Ppl4networks"
#'
#'
#' Sex/Age pyramid for the Canterbury Region, 2018
#'
#' A dataset produced from combining aggregate 2018 census datasets, using tablecode 8277, from the official Stats NZ table builder website \url{http://nzdotstat.stats.govt.nz/wbos/Index.aspx}. Stats NZ’s data are licensed by Stats NZ for reuse under the Creative Commons Attribution 4.0 International licence.
#'
#' @format A data frame with 14 observations and 4 variables
#' \describe{
#'     \item{Sex}{The sex relating to the count}
#'     \item{Age group}{String variable of age plus the text " years"}
#'     \item{Value}{The count of adolescents of that age and sex}
#'     \item{Age}{The age relating to that count}
#' }
#' "RegionalStructure"
#'
#' Subset of schools and their roll counts in the Canterbury Region, 2018
#'
#' A subset tibble constructed from CRSchools, in order to restrict roll counts. Total school roll counts for female and male students have been removed.
#'
#' @format A tibble with 266 rows and 5 variables
#' \describe{
#'    \item{School.ID}{The numeric ID for the school}
#'    \item{School.Name}{The name for the school}
#'    \item{Gender}{Indicator of whether the school is (C)o-ed, (f)emale-only, or (M)ale-only}
#'    \item{AgeInRoll}{The number of students that age in the school's roll}
#'    \item{RollCount}{The number of students for that school and age combination, at that school. 0 if no students that age attend.}
#' }
#' "SchoolsToUse"
#'
#'
#' Sex/age pyramid data for Timaru District
#'
#' A dataset produced from aggregate 2018 census data, using tablecode 8277, from the official Stats NZ table builder website \url{http://nzdotstat.stats.govt.nz/wbos/Index.aspx}. Stats NZ data are licensed by Stats NZ for reuse under the Creative Commons Attribution 4.0 International licence.
#'
#' @format A data frame with 190 rows and 4 variables
#' \describe{
#'     \item{Age.group}{Age group in five-year bands}
#'     \item{Sex}{Sex relating to the count}
#'     \item{Value}{The number of people that age and sex}
#'     \item{Age}{Age relating to the count}
#' }
#' "SingleAges"
#'
#'
#' Random selection of 10,000 synthetic people in the Timaru District from the 2018 census
#'
#' A dataset produced from combining aggregate 2018 census datasets, using tablecodes 8277, 8395, and 8460, from the official Stats NZ table builder website \url{http://nzdotstat.stats.govt.nz/wbos/Index.aspx}. Stats NZ data are licensed by Stats NZ for reuse under the Creative Commons Attribution 4.0 International licence.
#'
#' @format A data frame with 10,000 rows and 5 variables
#' \describe{
#'     \item{Sex}{Sex of the person}
#'     \item{Relationship}{Relationship status of the person}
#'     \item{ID}{The unique identifier for the person}
#'     \item{Age}{The age of the person}
#'     \item{HoursWorked}{The number of hours worked in employment, per week}
#' }
#' "Township"
#'
#'
#'People with a school indicator
#'
#' A modified version of "Township" data frame with a school indicator variable added. An ordered factor for hours worked per week has also been added.
#'
#' @format A data frame with 10,000 rows and 8 variables
#' \describe{
#'     \item{Sex}{Sex of the person}
#'     \item{Age.group}{The age group of the person}
#'     \item{Relationship}{Relationship status of the person}
#'     \item{ID}{The unique identifier for the person}
#'     \item{Age}{The age of the person}
#'     \item{HoursWorked}{The number of hours worked in employment, per week}
#'     \item{OrderedHours}{HoursWorked re-expressed as an ordered factor}
#'     \item{SchoolStatus}{The indicator of whether the person is in school (Y) or not (N)}
#' }
#' "WithSchoolInd"
#'
#' Adolescents with a school stats and employment hours
#'
#' A dataset of synthetic people aged between 16 and 20, produced by combining the aggregate 2018 census datasets 8277, 8395, and 8460, with school leavers data produced by the Ministry of Education. Stats NZ and the Ministry of Education's data are licensed, separately, for reuse under the Creative Commons Attribution 4.0 International licence.
#'
#' @format A data frame of 546 observations and 6 variables
#' \describe{
#'     \item{Sex}{Sex of the person}
#'     \item{Relationship}{Relationship status of the person}
#'     \item{ID}{The unique identifier for the person}
#'     \item{Age}{Age of the person}
#'     \item{HoursWorked}{The number of hours worked in employment, per week}
#'     \item{SchoolStatus}{The indicator of whether the person is in school (Y) or not (N)}
#' }
#' "WorkingAdolescents"
