# statgenSTA 1.0.4.1

* Option colorBy in plot functions is renamed to colorTrialBy and colorGenoBy for a clear distinction between what is actually colored. For the TD scatter plot both options are now available.
* Options colorTrialBy is added to map plots. This gives the possibility for coloring groups of trials on a map.
* Summary of spatial models fitted using asreml now has an extra column showing heritability.
* When creating a TD object, trLat, trLong, trDesign, and trDate can now be used to specify a data column containing the relevant information. 
* Levels of factor columns are no longer reordered when creating a TD object. 
* A bug where tick marks disappeared in spatial plot is fixed.
* A bug that crashed scatter plot for trials with no overlapping genotypes is fixed.
* A check has been added for unique row x column combinations when creating TD object.
* Some warning messages have been made more clear.
* Full ggplot2 is no longer imported.
* Checking change in last iteration is now working correctly for asreml4.

# statgenSTA 1.0.4

* Patch release to handle new defaults in lme4. No user visual changes.

# statgenSTA 1.0.3

* It is now possible to create a scatter plot matrix for objects of class TD.
* statgenSTA is no longer dependent on reshape2 and methods.
* Plot colors and backgrounds follow a more consistent pattern.
* Links to github are added in the DESCRIPTION file.

# statgenSTA 1.0.2

* Initial CRAN version
