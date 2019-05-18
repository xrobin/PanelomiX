.onLoad <- function(libname, pkgname) {
	rJava::.jpackage(pkgname, lib.loc = libname)
}


.onAttach <- function(lib, pkg) {
	packageStartupMessage("Type 'citation(\"PanelomiX\")' for a citation.")
}