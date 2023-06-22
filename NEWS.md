# statgenSTA 1.0.11.9000

* A bug in extracting the the error variances for spatial models fitted with asreml is fixed. Previously for some models NA would be returned as error variance.
* soft-deprecated `facets` in `facet_wrap` from ggplot2 is no longer used.

# statgenSTA 1.0.11

* A bug in `extractSTA` was fixed. It is now possible to extract CV when only a fixed model is fitted.
* The `extract` function that had been deprecated for a long time is now removed from the package.
* soft-deprecated `aes_string` from ggplot2 is no longer used. 

# statgenSTA 1.0.10

* Functions no longer rely on soft-deprecated ggplot2 functionality.
* CV is now computed as phenotypic coefficient of variation instead of genotypic coefficient of variation.

# statgenSTA 1.0.9

* A bug in the STA base plots is fixed. When genotypes were replicated in the field the base plots showed redundant points. Thanks @marktgee.
* Coefficient of Variation can now be extracted for models using SpATS (in previous versions this was only possible for lme4 and asreml)
* When trying to fit models with genotype fixed and checkId is TRUE a nice error message is now displayed. Before this also wasn't possible but the error message would come from the underlying packages and wasn't very clear.
* Improved and extended documentation

# statgenSTA 1.0.8

* Patch release to pass CRAN check for R.4.2.0. No user visual changes.

# statgenSTA 1.0.7

* Heritablity is now rounded to two decimals for models fitted using asreml and lme4 as was already the case for SpATS.
* The border color of the histograms in the scatter plots for TD objects now matches the fill.
* A minor bug in outlierSTA occurring when a column trial is not available is fixed.
* Default names of layout plots for TD objects are now the trial name instead of the location.
* Layout plots now have an extra options sizeGeno, that can be used for specifying the text size of genotypes.
* In layout plots the raw data of a specified trait can now be visualized using the traits parameter.
* Layout plots now have extra options for specifying custom colors when highlighting genotypes (colHighlight) or sub blocks (colSubBlock).
* The vignette has been updated using the data from the DROPS projects as in the other statgen packages.
* Documentation for several functions has been improved by adding extra clarification and extra examples.
* The data in TDMaize now has an extra column indicating the stress regime of the trials.

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
