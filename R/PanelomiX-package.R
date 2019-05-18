#' @rdname PanelomiX-package
#' @name PanelomiX-package
#' @aliases PanelomiX PanelomiX-package
#' @title PanelomiX - Create panels, combinations of biomarkers
#' @description This package implements the R command line interface for PanelomiX.
#' You can create panels, run cross-validation, much like with the web interface at 
#' \url{https://www.panelomix.net/}, but with more flexibiliy.
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