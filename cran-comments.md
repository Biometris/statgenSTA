## Release containing some minor fixes and improvements in the documentation

----

## Test environments

* local Windows 10 install, R 4.2.1
* Ubuntu (on github actions, devel and release)
* macOS (on github actions, release)
* R-hub (devel and release)

----

## R CMD check results

There were no ERRORs or WARNINGs.

There were 2 NOTES:

  * checking CRAN incoming feasibility ... NOTE    

    Suggests or Enhances not in mainstream repositories: asreml

    - asreml is a commercial R package that is used as one of three alternatives for modeling data.

  * checking package dependencies ... NOTE  
    Package suggested but not available for checking: 'asreml'
    
    - asreml is a commercial R package that is used as one of three alternatives for modeling data.

