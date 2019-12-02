## Test environments
* local Windows 10 install, R 3.6.1
* Debian 10 (on gitlab-ci), R 3.6.1
* R-hub (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There were 2 NOTES:

  * checking CRAN incoming feasibility ... NOTE    
    Maintainer: 'Bart-Jan van Rossum <bart-jan.vanrossum@wur.nl>'
    New submission
    
    Possibly mis-spelled words in DESCRIPTION:
    Biometris (50:49)
    statgen (50:18)
    statgenSSA (53:15)
    
    Suggests or Enhances not in mainstream repositories: asreml
    
    - First submission of the package
    
    - Mis-spelled words are names
    
    - asreml is a commercial R package that is used as one of three alternatives for modeling data.
  
  * checking package dependencies ... NOTE  
    Package suggested but not available for checking: 'asreml'
    
    - asreml is a commercial R package that is used as one of three alternatives for      modeling data.

