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

To install this package:

```R
    if (! requireNamespace("devtools")) install.packages("devtools")
    devtools::install_github("xrobin/pROC")
```

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
panels <- exh.train(aSAH, c("age", "s100b", "ndka"), "outcome")
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
