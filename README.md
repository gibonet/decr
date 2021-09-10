---
title: decr
output: github_document
---



[![R build status](https://github.com/gibonet/decr/workflows/R-CMD-check/badge.svg)](https://github.com/gibonet/decr/actions)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/decr)](http://cran.r-project.org/package=decr)

The decr package provides functions to decompose observed differences in distributional statistics of a numeric variable (y) between two groups.

An example of such analysis is the observed difference between the average wages of men and women.

The decomposition proposed here is of the Blinder (1973) and Oaxaca (1973) type, whose aim is to separate the observed difference between average wages of two groups in two components:

- One that is attributable to the different distributions of observed characteristics of the two groups;
- One that is not explainable by the different distributions of observed characteristics of the two groups.

The decompositions in decr are performed nonparametrically with the reweighting approach of Di Nardo, Fortin and Lemieux (DFL, 1996) and, for what concerns the common support, it is possible to decompose the difference of any distributional statistics (not only the mean). In the case of the difference between the averages (of y) of two groups, a decomposition in 4 components is done, as in Nopo (2008). The reweighting factors, in the common support, are estimated directly from the ratio of the relative frequencies observed in the strata for the two groups. In this sense, the estimation approach is very similar to coarsened exact matching (Iacus, King and Porro 2011), except that here it is possible to consider, if any, sampling weights.


# Installation

At the moment, `decr` is available only on github and can be installed with:

```
remotes::install_github("gibonet/decr", build_vignettes = TRUE)
```

It will be probably available on CRAN after some further testing.


# Getting started



To get started, read the "decr Basic Example" vignette, that can be opened with:

```
vignette("decr_intro", package = "decr")
```



# References

Blinder, A. S. 1973. Wage Discrimination: Reduced Form and Structural Estimates. *The Journal of Human Resources* VIII (4): 436-55.

DiNardo J., N. M. Fortin and T. Lemieux. 1996. Labor Market Institutions and the Distribution of Wages, 1973-1992: A Semiparametric Approach. *Econometrica* 64 (5): 1001-44.

Iacus S. M., G. King and G. Porro. 2011. Causal Inference without Balance Checking: Coarsened Exact Matching. *Political Analysis* 20: 1-24.

Nopo, H. 2008. Matching as a Tool to Decompose Wage Gaps. *Review of Economics and Statistics*, 90(2): 290-299.

Oaxaca, R. 1973. Male-Female Wage Differentials in Urban Labor Markets. *International Economic Review* 14 (3): 693-709.
