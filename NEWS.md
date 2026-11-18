# PopulateR 2.0

* ABMToCova substantially modified. Indicators for school type are changed so that Place2 is the school/classroom ID. Now outputs a single-column age data frame that can be read straight into Covasim. Any person absent from all four layers is removed. As Covasim requires sequential ID numbering from 0, ID renumbering will occur if this requirement is not met. A data frame containing the original versus new ID numbers is output.

* addemp now correctly uses the user seed, if one is provided. The excess number of workers compared to vacant positions is now reported correctly as a positive number.

* addind preamble now states that a probability-based method is used to indicate which pupils are in school. Minor wording fixes to preamble.

* addnetworklite added to package. Uses optimisenet, rather than brainGraph, to construct the igraph with the required transitivity.

* addschool prefixes school IDs with School_ to try to prevent overlapping school/child IDs.

* createemp now permits either a continuous or discrete method of selection for the number of employers and the number of employees.

* optimisenet added to package. Changes the transitivity of an undirected igraph to the level requested by the user. 

* other has been amended by replacing references to column numbers with variable names.

* otherNum has been amended by removing the attempts parameter. This had been replaced by the numiters during development and had been incorrectly retained. Incorrect variable references in matching have been corrected.

# PopulateR 1.13

## New package released to CRAN.

* Added a `NEWS.md` file to track changes to the package.
