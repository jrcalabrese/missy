# missy

> An R package designed for missing data and multiple imputation.

## Install package

``` r
devtools::install_github("jrcalabrese/missy")
library(missy)
```

## Introduction

`missy` is a package that was designed to accompany my candidacy project for Spring 2023 at The Ohio State University. I already had created some of these functions in my `tablecloth` package, but I wanted to add more and have a package specifically for multiply-imputed data.

If you have questions or comments, feel free to DM me on Twitter `@jrosecalabrese`.

## `mice_df`

``` r
data(nhanes)
imp <- mice::mice(nhanes, m = 5, printFlag = FALSE)
vs <- c("bmi", "chl", "age")
nm <- c("BMI", "Cholesterol", "Age")
title <- "Table 1: Descriptive statistics"
mice_df(imp = imp,
         vs = vs,
         title = title,
         nm = nm)
```

## `mice_cor`

``` r
data(nhanes)
imp <- mice::mice(nhanes, m = 5, printFlag = FALSE)
vs <- c("bmi", "chl", "age", "hyp")
title <- "Table 2: Correlation matrix"
mice_cor(imp = imp,
         vs = vs,
         title = title)
```

## `mice_alpha`

``` r
data(nhanes)
imp <- mice::mice(nhanes, m = 5, printFlag = FALSE)
vs <- c("bmi", "chl", "age", "hyp")
title <- "Table 3: Cronbach's alpha for variables from `nhanes`."
mice_alpha(imp = imp,
         varlist = vs,
         bnum = 100,
         title = title)
```
