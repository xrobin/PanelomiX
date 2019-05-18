#' @rdname PanelomiX-package
#' @name PanelomiX-package
#' @aliases PanelomiX PanelomiX-package
#' @title PanelomiX - Create panels, combinations of biomarkers
#' @description This package implements the R command line interface for PanelomiX.
#' You can create panels, run cross-validation, much like with the web interface at 
#' \url{https://www.panelomix.net/}, but with more flexibiliy.
#' @section Citation:
#' If you use PanelomiX in published research, please cite the following paper:
#' 
#' Xavier Robin, Natacha Turck, Alexandre Hainard, Natalia Tiberti,
#' Frédérique Lisacek, Jean-Charles Sanchez and Markus Müller (2011).
#' PanelomiX: A threshold-based algorithm to create panels of biomarkers. 
#' Translational Proteomics, 1(1), p. 57-64.
#' DOI: \href{http://dx.doi.org/10.1016/j.trprot.2013.04.003}{10.1016/j.trprot.2013.04.003}"
#' Type \code{citation("PanelomiX")} for a BibTeX entry.
#' @section Main Functions:
#' \tabular{rl}{
#' Training      \tab \code{\link{exh.train}} \cr
#' Cross-validation  \tab \code{\link{exh.train.cv}}  \cr
#' Prediction           \tab \code{\link{predict}} \cr
#' Count the number of panels found \tab \code{\link{nr.panels}} \cr
#' Predictors stability           \tab \code{\link{table.mol.stability}} \cr
#' Stability of the number of molecules in the panel           \tab \code{\link{table.nr.stability}} \cr
#' Stability of the positivity threshold           \tab \code{\link{table.min.nr.stability}} \cr
#' }
NULL