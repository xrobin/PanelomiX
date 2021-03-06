[![Build Status](https://travis-ci.org/xrobin/PanelomiX.svg?branch=master)](https://travis-ci.org/xrobin/PanelomiX)

PanelomiX
=============

The [R](http://r-project.org/) package to create panels of biomarkers with PanelomiX.

For more information, see:

1. Xavier Robin, Natacha Turck, Alexandre Hainard, *et al.* (2013) “PanelomiX: A threshold-based algorithm to create panels of biomarkers”. *Translational Proteomics*, **1** (1), 57-64. DOI: [10.1016/j.trprot.2013.04.003](http://dx.doi.org/10.1016/j.trprot.2013.04.003)
2. [The official web page](https://www.panelomix.net/)

Experimental
-------

This package is highly experimental. For an easier experience please use [the web-based tool](https://www.panelomix.net/).
Functions and arguments may change at any time and without notice. Functionality is not guaranteed.
Feel free to report bugs on the bug tracker. However this project is no longer under active development, and support and
bug fixes are not guaranteed.

To install this package:

```R
    if (! requireNamespace("devtools")) install.packages("devtools")
    devtools::install_github("xrobin/PanelomiX")
```

System Requirements
-------------------

This package requires a GNU/Linux system with Java installed and a working installation of [rJava](https://cran.r-project.org/web/packages/rJava/index.html).

Windows is supported. MacOS is untested.

Help
-------

Once the library is loaded with `library(PanelomiX)`, you can get help on PanelomiX by typing `?PanelomiX`.

Getting started
-------

If you don't want to read the manual first, try the following:

### Loading some data

```R
library(PanelomiX)
library(pROC)
data(aSAH)
```
### Basic training of a panel
```R
panels <- exh.train( data = aSAH, 
                     predictors = c("age", "s100b", "ndka"), 
                     response = "outcome")
```
### Test a specific threshold for one/many candidate predictor(s) 
```R
panels <- exh.train(data = aSAH, 
                    predictors = c("age", "s100b", "ndka"), 
                    response = "outcome", 
                    test.thresholds.predictors = list(s100b = 0.5))
```

### Test a specific direction for one/many candidate predictor(s) 
```R
panels <- exh.train(data = aSAH, 
                    predictors = c("age", "s100b", "ndka"), 
                    fixed.predictors = c("age"), 
                    response = "outcome",
                    directions = list(age = ">"))
```

### Cross-validation
```R
cv <- exh.train.cv(aSAH, c("age", "s100b", "ndka"), "outcome", progress=FALSE)
```
### Predict new data
```R
predict(panels, aSAH)
```
### Count how many panels were found
```R
nr.panels(panels)
nr.panels(cv)
```
### Inspect the stability of the cross-validation
```R
# Biomarker selection
table.mol.stability(cv)
# Number of biomarkers selected in the panel
table.nr.stability(cv)
# How many biomarkers should be positive for the panel to be positive?
table.min.nr.stability(cv)
```
