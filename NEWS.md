# statgenSTA 1.0.7

* Heritablity is now rounded to two decimals for models fitted using asreml and lme4 as was already the case for SpATS.
* The border color of the histograms in the scatter plots for TD objects now matches the fill.
* A minor bug in outlierSTA occurring when a column trial is not available is fixed.

# statgenSTA 1.0.6

* Default colors for genotypes and trials have been improved. User specific default colors may now be set in the R options "statgen.colGeno" (for genotypes) and "statgen.colTrial" (for trials).
* New options colGeno and colTrial have been added to the relevant plot functions for TD objects. These options can be used to specify the colors for genotypes (colGeno) and trials (colTrial) in the plots.
* Option trials in plot function for TD objects is now respected for all types of plots.
* Plots for TD and STA object now have an option title.
* Lines between sub block in layout plots for TD objects are now displayed in blue to make them stand out more clearly.
* Map plots for TD objects now have an extra argument printTrialNames that allows for printing and not printing of trial names on the map. The default of TRUE retains the original behavior of the function.
* A bug in scatter plots for TD objects with custom ordering of trials has been fixed.
* The final plot in spatial plots for STA objects now displays the BLUEs/BLUPs instead of the residuals.
* A minor bug in fitTD occurring when a column trial is not available is fixed.

# statgenSTA 1.0.5

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
