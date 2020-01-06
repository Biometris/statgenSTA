statgenSTA
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![pipeline
status](https://git.wur.nl/statistical-genetic-pipeline/statgenSTA/badges/master/pipeline.svg)](https://git.wur.nl/statistical-genetic-pipeline/statgenSTA/commits/master)
[![coverage
report](https://git.wur.nl/statistical-genetic-pipeline/statgenSTA/badges/master/coverage.svg)](https://git.wur.nl/statistical-genetic-pipeline/statgenSTA/commits/master)
<!-- badges: end -->

R package for single trial analysis

## Installation

For direct installation from gitlab use the following code:

``` r
## Replace the location for public and private key with your own.
creds <- git2r::cred_ssh_key(publickey = "C:\\users\\...\\.ssh\\id_rsa.pub",
                             privatekey = "C:\\users\\...\\.ssh\\id_rsa")
remotes::install_git(url = "git@git.wur.nl:statistical-genetic-pipeline/statgenSTA.git", credentials = creds)
```

## Implemented functionality

The following functionality has been implemented:

  - Single trial analysis
      - Single trial analysis using SpATS, asreml or lme4
      - Computation of statistics e.g. BLUEs, BLUPs, SE of BLUEs, SE of
        BLUPs and heritability based on the fitted models.
  - Plotting and reporting (.pdf and .tex) functions for most raw and
    fitted data.
